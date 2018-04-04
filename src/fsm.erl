%NEW FSM BRANCH

-module(fsm).
-export([start/0]).

-include("parameters.hrl").
-define(DOOR_OPEN_TIME, 2000).
-define(FLOOR_SENSOR_SLEEP_BETWEEN_FLOORS, 100).
-define(DISCONNECTED_TIME, 10000).

-record(state, {movement, floor}).                                          % trenger vi egentlig denne? 

% Ordermanager skal sende Assigned ordre regelsmessig når fsm er i idle
% Ordermanager skal sende Unnasigned_order_list når den endres.
% Kontroller watchdog

start() ->
    io:format("FSM state: Uninitialized\n"),
    fsm_loop(uninitialized, undefined, stop_dir, none, []).


fsm_loop(State, Latest_floor, Moving_dir, Assigned_order, Unassigned_order_list) ->
    receive
        
        %----------------------------------------------------------------------------------------------
        % Receives a new order to be completed by this elevator
        %----------------------------------------------------------------------------------------------
        {assigned_order, New_assigned_order} when State == idle ->
            case choose_direction(New_assigned_order, Latest_floor) of 
                stop_dir ->
                    driver ! {set_door_open_LED, on},
                    spawn(fun() -> timer:sleep(?DOOR_OPEN_TIME), fsm ! close_door end),
                    fsm_loop(door_open, Latest_floor, stop_dir, New_assigned_order, Unassigned_order_list);
                
                Moving_dir ->
                    io:format("Moving_dir: ~p~n", [Moving_dir]),
                    driver ! {set_motor_dir, Moving_dir},
                    node_communicator ! {reached_new_state, #state{movement = Moving_dir, floor = Latest_floor}},
                    fsm_loop(moving, Latest_floor, Moving_dir, New_assigned_order, Unassigned_order_list)
            end;

        
        %----------------------------------------------------------------------------------------------
        % Receives and updates the list of unassigned orders
        %----------------------------------------------------------------------------------------------
        {update_order_list, Updated_unnasigned_order_list} ->
            % HER MÅ VI HUSKE Å SEND OGSÅ NÅR NOE ASSIGNES (dvs da endre jo unassigned-listen, og det må fsm få vite)
            fsm_loop(State, Latest_floor, Moving_dir, Assigned_order, Updated_unnasigned_order_list);

        
        %----------------------------------------------------------------------------------------------
        % Elevator reched a (potentially new) floor
        %----------------------------------------------------------------------------------------------
        {floor_sensor, Read_floor} ->
            case {State, Read_floor} of 
                {uninitialized, between_floors} ->
                    driver   ! {set_motor_dir, down_dir},
                    watchdog ! start_watching_movement,
                    fsm_loop(uninitialized, undefined, down_dir, none, Unassigned_order_list);

                {uninitialized, _} ->
                    driver            ! {set_motor_dir, stop_dir},
                    driver            ! {set_floor_LED, Read_floor},
                    watchdog          ! stop_watching_movement,
                    node_communicator ! {reached_new_state, #state{movement = Moving_dir, floor = Latest_floor}},
                    io:format("FSM state: Idle\n"),
                    fsm_loop(idle, Read_floor, stop_dir, none, Unassigned_order_list);


                {idle, _}                -> ok;
                {door_open, _}           -> ok;
                {moving, Latest_floor}   -> ok;
                {moving, between_floors} -> ok;
                
                {moving, Read_floor} ->
                    driver ! {set_floor_LED, Read_floor},
                    node_communicator ! {reached_new_state, #state{movement = Moving_dir, floor = Read_floor}},
                    watchdog ! stop_watching_movement,                                                              %%Fiks dette
                    case should_elevator_stop(Read_floor, Moving_dir, Assigned_order ++ Unassigned_order_list) of
                        true ->
                            driver ! {set_motor_dir, stop_dir},
                            driver ! {set_door_open_LED, on},
                            io:format("FSM state: Door open\n"),
                            fsm_loop(door_open, Read_floor, stop_dir, Assigned_order, Unassigned_order_list);
                        false ->
                            watchdog ! start_watching_movement,
                            fsm_loop(moving, Read_floor, Moving_dir, Assigned_order, Unassigned_order_list)
                    end;
                
                Unexpected ->
                    io:format("Unexpected pattern match in fsm, {floor_sensor, Read_floor}: ~p\n", [Unexpected])
            end;
        
        
        %----------------------------------------------------------------------------------------------
        % Received message for door to close after being open for 'DOOR_OPEN_TIME' ms
        %----------------------------------------------------------------------------------------------
        close_door ->
            io:format("fsm: Closing door\n"),
            driver ! {set_door_open_LED, off},
            Direction_headed = choose_direction(Assigned_order, Latest_floor),
            node_communicator ! {order_finished, {cab_button, Latest_floor}},
            node_communicator ! {order_finished, {convert_to_button_type(Direction_headed), Latest_floor}},

            case Latest_floor == element(2, Assigned_order) of 
                true ->            
                    node_communicator ! {reached_new_state, #state{movement = stop_dir, floor = Latest_floor}},
                    io:format("FSM state: Idle\n"),
                    fsm_loop(idle, Latest_floor, stop_dir, none, Unassigned_order_list);
                false ->
                    driver ! {set_motor_dir, Direction_headed},
                    io:format("FSM state: Moving\n"),
                    fsm_loop(moving, Latest_floor, Direction_headed, Assigned_order, Unassigned_order_list)
            end;


        %----------------------------------------------------------------------------------------------
        % Elevator movement to next floor timed out, disconnecting the node and restarts FSM
        %----------------------------------------------------------------------------------------------
        timeout_movement ->
            io:format("FSM: timeout_movement~n"),
            driver ! {set_motor_dir, stop_dir},            
            disconnect_node_and_sleep(),
            io:format("FSM state: Uninitialized\n"),
            fsm_loop(uninitialized, undefined, stop_dir, none, []);

        Unexpected ->
            io:format("Unexpected error in fsm main recv: ~p\n", [Unexpected])

    end,
            
fsm_loop(State, Latest_floor, Moving_dir, Assigned_order, Unassigned_order_list).
                                
                       
% ---------------------------------------- Help functions ------------------------------------------------------------

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
    cab_button.

disconnect_node_and_sleep() ->
    node_connection ! disconnect_node,
    spawn(fun() -> timer:sleep(?DISCONNECTED_TIME), fsm ! {disconnection, timeout} end),
    sleep_loop().

sleep_loop() ->
    receive
        {disconnection, timeout} ->
            ok;
        _Disregarded_message ->
            sleep_loop()
    end.

should_elevator_stop(Latest_floor, Moving_dir, Orders) ->   %%Skriv denne 
    true.

% %----------------------------------------------------------------------------------------------
% % Checks is the elevator should stop at 'Floor' when moving in the specified direction
% %----------------------------------------------------------------------------------------------
% % Moving up
% {should_elevator_stop, Floor, up_dir, PID} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
%     PID ! lists:member({cab_button, Floor}, Orders#orders.cab_orders) or
%             lists:member({up_button,  Floor}, Orders#orders.unassigned_hall_orders), % ++ Orders#orders.assigned_hall_orders),
%     main_loop(Orders, Elevator_states);
% % Moving down
% {should_elevator_stop, Floor, down_dir, PID} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
%     PID ! lists:member({cab_button,  Floor}, Orders#orders.cab_orders) or
%             lists:member({down_button, Floor}, Orders#orders.unassigned_hall_orders), % ++ Orders#orders.assigned_hall_orders),
%     main_loop(Orders, Elevator_states);
% % Idle elevator
% {should_elevator_stop, Floor, stop_dir, PID} when is_pid(PID) ->
%     PID ! lists:member({cab_button, Floor}, Orders#orders.cab_orders),
%     main_loop(Orders, Elevator_states);