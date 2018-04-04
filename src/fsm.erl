%NEW FSM BRANCH

-module(fsm).
-export([start/0]).

-include("parameters.hrl").
-define(DOOR_OPEN_TIME, 2000).
-define(FLOOR_SENSOR_SLEEP_BETWEEN_FLOORS, 100).
-define(DISCONNECTED_TIME, 10000).

-record(state, {movement, floor}).

% Ordermanager skal sende Assigned ordre regelsmessig når fsm er i idle
% Ordermanager skal sende Unnasigned_order_list når den endres.

start() ->
    Floor = init_elevator(),
    fsm_loop(idle, Floor, _, _, _).


fsm_loop(State, Latest_floor, Moving_dir, Assigned_order, Unassigned_order_list) ->
    {Order_button_type, Order_floor} = Order,
    receive
        {assigned_order, New_assigned_order} ->
            case choose_direction(Order, Latest_floor) of 
                stop_dir when Latest_floor == Order_Floor ->
                    driver ! {set_door_open_LED, on},
                    fsm_loop(door_open, Latest_floor, stop_dir, New_assigned_order, Unassigned_order_list);
                stop_dir ->
                    io:format("Error: recieved illegal order from scheduler: ~p~n", [Order]),
                    node_communicator ! {reached_new_state, #state{movement = stop_dir, floor = Latest_floor}},
                    fsm_loop(idle, Latest_floor, stop_dir, _, _);
                Moving_dir ->
                    io:format("Moving_dir: ~p~n", [Moving_dir]),
                    driver ! {set_motor_dir, Moving_dir},
                    fsm_loop(moving, Latest_floor, Moving_dir, New_assigned_order, Unassigned_order_list)
            end;

        
        {order_list, New_unnasigned_order_list} ->
            fsm_loop(State, Latest_floor, Moving_dir, New_assigned_order, New_unnasigned_order_list);

        {floorSensor, Read_floor} ->
            case {State, Read_floor} of 
                {uninitialized, _} ->
                    %add watchdog functionality
                    io:format("Huhh, burde dette skje da\n");

                {idle, _} -> ok;
                
                {moving, Latest_floor} -> ok;
        
                {moving, between_floors} -> ok;
                
                {moving, Read_floor} ->
                    driver ! {set_floor_LED, Read_floor},
                    node_communicator ! {reached_new_state, #state{movement = Moving_dir, floor = New_floor}},
                    watchdog ! stop_watching_movement,                                                              %%Fiks dette
                    watchdog ! start_watching_movement,
                    case should_elevator_stop(Read_floor, Moving_dir, Assigned_order, Unassigned_order_list) ->      % Lag denne 
                        true ->
                            driver ! {set_motor_dir, stop_dir},
                            fsm_loop(stopped, Read_floor, stop_dir, Assigned_order, Unassigned_order_list);
                        false ->
                            fsm_loop(moving, Read_floor, Moving_dir, Assigned_order, Unassigned_order_list);
                    end;
                
                door_open -> ok;

                Unexpected ->
                    io:format("Unexpected error in fsm, {floorSensor, Read_floor} with reason: ~p\n", [Unexpected])
            end;
        
        close_door ->
            driver ! {set_door_open_LED, off},
            io:format("FSM: Closed door\n"),
            node_communicator ! {order_finished, {cab_button, Latest_floor}},
            node_communicator ! {order_finished, {convert_to_button_type(Moving_dir), Latest_floor}},

            case Latest_floor == Order_floor of 
                true ->            
                    node_communicator ! {reached_new_state, #state{movement = stop_dir, floor = Latest_floor}},
                    fsm_loop(idle, Latest_floor, Moving_dir, Assigned_order, Unassigned_order_list)
                false ->
                    Moving_direction = choose_direction(Order, Latest_floor),
                    driver ! {set_motor_dir, Moving_direction},
                    fsm(moving, Latest_floor, Moving_direction, Order)
            end.

            
        %timeout_close_door ->
        %    driver ! {set_door_open_LED, off},
        %    ;
        
        timeout_movement ->
            io:format("FSM: timeout_movement~n"),
            driver ! {set_motor_dir, stop_dir},
            node_connection ! disconnect_node,
            timer:sleep(?DISCONNECTED_TIME),        %%Fucker denne opp pga køopphoping?
            start().

        Unexpected ->
            io:format("Unexpected error in fsm main recv: ~p\n", [Unexpected])

    end,
fsm_loop(State, Latest_floor, Moving_dir, Assigned_order, Unassigned_order_list)



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