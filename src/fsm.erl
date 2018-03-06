-module(fsm).
-export([start/0]).

-export([choose_direction/2]).           %for debugging

-include("parameters.hrl").
-define(DOOR_OPEN_TIME, 2000).

start() ->
    Floor = init_elevator(),
    main_loop(idle, Floor).

main_loop(idle, Latest_floor) ->
    io:format("FSM: Idle~n"),
    main_loop(idle_loop, Latest_floor);

main_loop(idle_loop, Latest_floor) ->
    %order_manger ! is_idle, 
    marius ! is_idle,         %%DEGUB
    receive 
        Order ->
            Moving_direction = choose_direction(Order, Latest_floor),
            io:format("Moving_dir: ~p~n", [Moving_direction]),
            driver ! {set_motor_dir, Moving_direction},
            main_loop(moving, Latest_floor, Moving_direction, Order)
    after
        2000 ->
        main_loop(idle_loop, Latest_floor)
    end.

main_loop(moving, Latest_floor, Moving_direction, {Button_type, Floor}) ->
    Order = {Button_type, Floor},
    io:format("FSM: moving~n"),
    driver ! {get_floor, self()},
    receive 
        between_floors ->
            timer:sleep(100),
            main_loop(moving, Latest_floor, Moving_direction, Order);
        {floor, New_floor} ->
            io:format("on_floor, ~p~n", [New_floor]),
            case New_floor of
                Floor -> 
                    driver ! {set_motor_dir, stop_dir},
                    main_loop(stopped, New_floor, Order);
                _ ->
                    ok
            end,

            %bør heisen stoppe uten å spørre hvis det er ordren den går mot?
            %%order_manger ! {should_i_stop_at, New_floor, Moving_direction},   %eller button type?
            marius ! {should_i_stop_at, New_floor, Moving_direction},                                       %%DEBUG
            receive 
                true ->
                    driver ! {set_motor_dir, stop_dir},
                    main_loop(stopped, New_floor, Order);
                false ->
                    ok
            after
                1000 ->
                    io:format("Timeout in main_loop(moving) on should_I_stop~n")
            end;
        {error, Reason} ->
            io:format("Error in fsm, main_loop(moving) with reason: ~p~n", [Reason]);
        Unexpected ->
            io:format("Unexpected error in fsm, main_loop(moving) with reason: ~p~n", [Unexpected])
    after 
        2000 ->
            io:format("Timeout in main_loop(moving) og get_floor~n")
    end,
    main_loop(moving, Latest_floor, Moving_direction, {Button_type, Floor}). 

main_loop(stopped, Latest_floor, {Button_type, Floor}) ->
    io:format("FSM: Stopped~n"),
    if Latest_floor == Floor ->
        driver ! {set_door_open_LED, on},
        main_loop(door_open, Latest_floor, {Button_type, Floor});
    Latest_floor /= Floor ->
        main_loop(idle, Latest_floor)
    end;

main_loop(door_open, Latest_floor, {Button_type, Floor}) ->
    Order = {Button_type, Floor},
    io:format("FSM: Door open~n"),
    timer:sleep(?DOOR_OPEN_TIME),
    driver ! {set_door_open_LED, off},
    io:format("FSM: Closed door~n"),
    
    case Latest_floor == Floor of
        true ->
            %order_manager ! {remove_order, Order},             %%Må komme inn når ordermanager er ferdig.
            main_loop(idle, Latest_floor);
        false ->
            Moving_direction = choose_direction(Order, Latest_floor),
            driver ! {set_motor_dir, Moving_direction},
            main_loop(moving, Latest_floor, Moving_direction, Order)
    end.





init_elevator() ->
    io:format("Initialise elevator!\n"),
    driver ! {get_floor, self()},
    receive 
        between_floors ->
            driver ! {set_motor_dir, down_dir},
            timer:sleep(100),
            init_elevator();
        {floor, Latest_floor} ->
            driver ! {set_motor_dir, stop_dir},
            Latest_floor;
        Unexpected ->
            io:format("Unexpected msg received in fsm.erl:~p\n", [Unexpected])
    end.
    


% Help functions

choose_direction({up_button, ?NUMBER_OF_FLOORS}, _Latest_floor) ->
    io:format("Error in choose dir! Called with up_button of top floor~n"),
    stop_dir;
choose_direction({down_button, 1}, _Latest_Floor) ->
    io:format("Error in choose dir! Called with down_button of top floor~n"),
    stop_dir;
choose_direction({_Button_type, Floor}, Latest_floor) when Floor == Latest_floor ->
    io:format("Error in choose dir! Called with Floor == Latest_Floor: ~p~n", [Floor]),
    stop_dir;
choose_direction({_Button_type, Floor}, Latest_floor) when Latest_floor < Floor ->
    up_dir;
choose_direction({_Button_type, Floor}, Latest_floor) when Latest_floor > Floor ->
    down_dir.
