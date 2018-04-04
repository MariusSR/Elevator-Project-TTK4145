%NEW FSM BRANCH

-module(fsm).
-export([start/0]).

-include("parameters.hrl").
-define(DOOR_OPEN_TIME, 2000).
-define(FLOOR_SENSOR_SLEEP_BETWEEN_FLOORS, 100).
-define(DISCONNECTED_TIME, 10000).

-record(state, {movement, floor}).

% TODO: add feature for å blokke vha cab button!

start() ->
    Floor = init_elevator(),
    fsm(idle, Floor).

% Idle state
fsm(idle, Latest_floor) ->
    io:format("FSM: Idle~n"),
    node_communicator ! {reached_new_state, #state{movement = stop_dir, floor = Latest_floor}},
    fsm(idle_loop, Latest_floor);

fsm(idle_loop, Latest_floor) ->
    order_manager ! {get_unassigned_order, self()},
    receive 
        {Button_type, Floor} when is_atom(Button_type) andalso Floor =< ?NUMBER_OF_FLOORS andalso Floor >= 1 ->
            Order = {Button_type, Floor},
            case choose_direction(Order, Latest_floor) of 
                stop_dir when Latest_floor == Floor ->
                    driver ! {set_door_open_LED, on},
                    fsm(door_open, Latest_floor, Order);
                stop_dir ->
                    io:format("Error: recieved illegal order from scheduler: ~p~n", [Order]),
                    fsm(idle, Latest_floor);
                 Moving_direction ->
                    io:format("Moving_dir: ~p~n", [Moving_direction]),
                    driver ! {set_motor_dir, Moving_direction},
                    fsm(moving, Latest_floor, Moving_direction, Order)
                end;
        no_orders_available ->
            timer:sleep(500),
            fsm(idle_loop, Latest_floor);
        Unexpected ->
            io:format("Unexpected error in fsm(idle_loop) recv: recieved illegal order from scheduler: ~p~n", [Unexpected]),
            fsm(idle, Latest_floor)
    after
        2000 ->
            fsm(idle_loop, Latest_floor)
    end.

% Moving state
fsm(moving, Latest_floor, Moving_direction, {Button_type, Floor}) ->
    io:format("FSM: moving with order: ~p~n", [{Button_type, Floor}]),
    node_communicator ! {reached_new_state, #state{movement = Moving_direction, floor = Latest_floor}},
    watchdog ! start_watching_movement,
    fsm(moving_loop, Latest_floor, Moving_direction, {Button_type, Floor});

fsm(moving_loop, Latest_floor, Moving_direction, {Button_type, Floor}) ->
    watchdog ! is_movement_timed_out,
    receive
        true ->
            fsm(error);
        false ->
            ok
    end,

    Order = {Button_type, Floor},
    driver ! {get_floor, self()},
    receive 
        between_floors ->
            timer:sleep(?FLOOR_SENSOR_SLEEP_BETWEEN_FLOORS),
            fsm(moving_loop, Latest_floor, Moving_direction, Order);
        {floor, New_floor} when New_floor == Latest_floor ->
            ok;        
        {floor, New_floor} when New_floor /= Latest_floor ->
            driver ! {set_floor_LED, New_floor},
            node_communicator ! {reached_new_state, #state{movement = Moving_direction, floor = New_floor}},
            watchdog ! stop_watching_movement,
            watchdog ! start_watching_movement,
            case New_floor of
                _Floor when New_floor == Floor orelse (New_floor == ?NUMBER_OF_FLOORS andalso Moving_direction == up_dir) orelse 
                           (New_floor == 1 andalso Moving_direction == down_button) -> 
                    driver ! {set_motor_dir, stop_dir},
                    fsm(stopped, New_floor, Order);
                _Floor ->
                    order_manager ! {should_elevator_stop, New_floor, Moving_direction, self()},
                    receive 
                        true ->
                            driver ! {set_motor_dir, stop_dir},
                            fsm(stopped, New_floor, Order);
                        false ->
                            ok
                    after
                        1000 ->
                            io:format("Timeout in fsm(moving) on should_elevator_stop~n")
                    end,
                    fsm(moving_loop, New_floor, Moving_direction, {Button_type, Floor}) 
            end;
            
        {error, Reason} ->
            io:format("Error in fsm, fsm(moving) with reason: ~p~n", [Reason]);
        Unexpected ->
            io:format("Unexpected error in fsm, fsm(moving) with reason: ~p~n", [Unexpected])
    after 
        2000 ->
            io:format("Timeout in fsm(moving) og get_floor~n")
    end,
    fsm(moving_loop, Latest_floor, Moving_direction, {Button_type, Floor}). 

% Stopped state
fsm(stopped, Latest_floor, {Button_type, Floor}) ->
    io:format("FSM: Stopped~n"),
    driver ! {set_door_open_LED, on},
    watchdog ! stop_watching_movement,
    fsm(door_open, Latest_floor, {Button_type, Floor});


fsm(door_open, Latest_floor, {Button_type, Floor}) ->
    Order = {Button_type, Floor},
    io:format("FSM: Door open~n"),
    timer:sleep(?DOOR_OPEN_TIME),
    driver ! {set_door_open_LED, off},
    io:format("FSM: Closed door~n"),

    case Button_type of
        cab_button when Floor == Latest_floor ->
            node_communicator ! {order_finished, {cab_button, Latest_floor}};
        Button when Floor == Latest_floor ->
            node_communicator ! {order_finished, {Button, Latest_floor}},
            node_communicator ! {order_finished, {cab_button, Latest_floor}};
        _Else ->
            node_communicator ! {order_finished, {convert_to_button_type(choose_direction(Order, Latest_floor)), Latest_floor}},
            node_communicator ! {order_finished, {cab_button, Latest_floor}}
    end,
    
    case Latest_floor == Floor of
        true ->            
            fsm(idle, Latest_floor);
        false ->
            Moving_direction = choose_direction(Order, Latest_floor),
            driver ! {set_motor_dir, Moving_direction},
            fsm(moving, Latest_floor, Moving_direction, Order)
    end.

fsm(error) ->
    io:format("FSM: error~n"),
    driver ! {set_motor_dir, stop_dir},
    node_connection ! disconnect_node,
    timer:sleep(?DISCONNECTED_TIME),
    start().


% ---------------------------------------- Help functions ------------------------------------------------------------

init_elevator() ->
    io:format("FSM: Initialise elevator!\n"),
    watchdog ! start_watching_movement,
    init_elevator_loop().
    
init_elevator_loop() ->
    watchdog ! is_movement_timed_out,
    receive
        true ->
            fsm(error);
        false ->
            ok
    end,
    driver ! {get_floor, self()},
    receive 
        between_floors ->
            driver ! {set_motor_dir, down_dir},
            timer:sleep(100),
            init_elevator_loop();
        {floor, Latest_floor} ->
            driver ! {set_motor_dir, stop_dir},
            driver ! {set_floor_LED, Latest_floor},
            watchdog ! stop_watching_movement,
            Latest_floor;
        Unexpected ->
            io:format("Unexpected msg received in fsm.erl:~p\n", [Unexpected])
    end.


choose_direction({up_button, ?NUMBER_OF_FLOORS}, _Latest_floor) ->
    io:format("Error in choose dir! Called with up_button of top floor~n"),
    stop_dir;
choose_direction({down_button, 1}, _Latest_Floor) ->
    io:format("Error in choose dir! Called with down_button of bottom floor~n"),
    stop_dir;
choose_direction({_Button_type, Floor}, Latest_floor) when Floor == Latest_floor ->
    stop_dir;
choose_direction({_Button_type, Floor}, Latest_floor) when Latest_floor < Floor ->
    up_dir;
choose_direction({_Button_type, Floor}, Latest_floor) when Latest_floor > Floor ->
    down_dir.

convert_to_button_type(up_dir) ->
    up_button;
convert_to_button_type(down_dir) ->
    down_button;
convert_to_button_type(stop_dir) ->
    io:format("ERROR: tried to convert stop_dir to button type\n"),
    error.


%Door open state
% fsm(door_open, Latest_floor, {Button_type, Floor}) ->
%     Order = {Button_type, Floor},
%     io:format("FSM: Door open~n"),
%     Calculated_butten_type = convert_to_button_type(choose_direction(Order, Latest_floor)),

%     case Button_type of
%         cab_button when Floor == Latest_floor ->
%             node_communicator ! {order_finished, {cab_button, Latest_floor}};
%         Button when Floor == Latest_floor ->
%             node_communicator ! {order_finished, {Button, Latest_floor}},
%             node_communicator ! {order_finished, {cab_button, Latest_floor}};
%         _Else when Calculated_butten_type == up_button orelse Calculated_butten_type == down_button ->
%             node_communicator ! {order_finished, {calculated_butten_type, Latest_floor}},
%             node_communicator ! {order_finished, {cab_button, Latest_floor}};
%         _Else ->
%             io:format("asd\n")
%     end,

%     timer:sleep(?DOOR_OPEN_TIME),

%     case Calculated_butten_type of
%         error -> 
%             ok;
%         _Else2 ->
%             order_manager ! {should_elevator_stop, Latest_floor, Calculated_butten_type, self()}
%     end,

%     receive 
%         true ->
%             fsm(door_open, Latest_floor, {Button_type, Floor});
%         false -> 
%             ok
%     after
%             1000 ->
%                 io:format("Timeout in fsm(moving) on should_elevator_stop~n")
%     end,

%     driver ! {set_door_open_LED, off},
%     io:format("FSM: Closed door~n"),

%     case Latest_floor == Floor of
%         true ->            
%             fsm(idle, Latest_floor);
%         false ->
%             Moving_direction = choose_direction(Order, Latest_floor),
%             driver ! {set_motor_dir, Moving_direction},
%             fsm(moving, Latest_floor, Moving_direction, Order)
%     end.