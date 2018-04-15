%%=================================================================================================
%% This module controls the behaviour of the elevator, interfaced by messages sent to 'fsm'.
%%=================================================================================================

-module(fsm).
-export([start/0]).

-include("parameters.hrl").
-define(DOOR_OPEN_TIME,    3000).
-define(DISCONNECTED_TIME, 5000).

-record(state, {movement, floor, assigned_order}).



start() ->
    timer:sleep(200),  % Sleep to better align PID prints at start up. None other uses and can thus be safely removed
    io:format("~s uninitialized\n", [color:yellow("FSM state:")]),
    watchdog ! start_watching_movement,
    fsm_loop(uninitialized, undefined, stop_dir, none, []).


fsm_loop(State, Latest_floor, Moving_dir, Assigned_order, Unassigned_orders) ->
    %io:format("~s   S: ~p    Lf: ~p     Md: ~p    Ao: ~p    Uo: ~p\n", [color:greenb("FSM_loop arguments:"), State, Latest_floor, Moving_dir, Assigned_order, Unassigned_orders]),

    receive
        
        %----------------------------------------------------------------------------------------------
        % Receives (from data_manager) a new order to be completed by this elevator.
        %----------------------------------------------------------------------------------------------
        {assigned_order, New_assigned_order, Updated_unassigned_orders} when State == idle ->
            case choose_direction(New_assigned_order, Latest_floor) of 
                stop_dir ->
                    driver ! {set_door_open_LED, on},
                    spawn(fun() -> timer:sleep(?DOOR_OPEN_TIME), fsm ! close_door end),
                    io:format("~s door open\n", [color:yellow("FSM state:")]),
                    fsm_loop(door_open, Latest_floor, stop_dir, New_assigned_order, Updated_unassigned_orders);
                
                New_dir ->
                    communicator ! {reached_new_state, #state{movement = New_dir, floor = Latest_floor, assigned_order = New_assigned_order}},
                    driver       ! {set_motor_dir, New_dir},
                    watchdog     ! start_watching_movement,
                    io:format("~s moving\n", [color:yellow("FSM state:")]),
                    fsm_loop(moving, Latest_floor, New_dir, New_assigned_order, Updated_unassigned_orders)
            end;
        


        %----------------------------------------------------------------------------------------------
        % Cancels (from data_manager) the assigend order if it is already served by another elevator.
        %----------------------------------------------------------------------------------------------
        cancel_assigned_order when Assigned_order == none -> ignore; % Should never happen
        cancel_assigned_order ->
            io:format("~s canceled assigned order\n", [color:yellow("FSM state:")]),
            fsm_loop(cancel_assigned_order, Latest_floor, Moving_dir, none, Unassigned_orders);



        %---------------------------------------------------------------------------------------------->
        % Receives (from data_manager) an updated list of unassigned orders (including cab orders).
        %----------------------------------------------------------------------------------------------
        {update_order_list, Updated_unassigned_orders} ->
            fsm_loop(State, Latest_floor, Moving_dir, Assigned_order, Updated_unassigned_orders);



        %----------------------------------------------------------------------------------------------
        % Handles new florr sensor samples from the hardware_reader module:
        %----------------------------------------------------------------------------------------------
        {floor_sensor, Read_floor} ->
            case {State, Read_floor} of
                %----------------------------------------------------------------------------------------------
                % Elevator not yet initialized, starts moving down until it reaches a defined floor.
                %----------------------------------------------------------------------------------------------
                {uninitialized, between_floors} ->
                    driver   ! {set_motor_dir, down_dir},
                    fsm_loop(uninitialized, undefined, down_dir, none, Unassigned_orders);

                %----------------------------------------------------------------------------------------------
                % Elevator not yet initialized, reached a defined floor and finishes initialization.
                %----------------------------------------------------------------------------------------------
                {uninitialized, Read_floor} ->
                    driver   ! {set_motor_dir, stop_dir},
                    driver   ! {set_floor_LED, Read_floor},
                    watchdog ! stop_watching_movement,
                    case lists:member({cab_button, Read_floor}, Unassigned_orders) of
                        true  -> 
                            driver ! {set_door_open_LED, on},
                            spawn(fun() -> timer:sleep(?DOOR_OPEN_TIME), fsm ! close_door end),
                            io:format("~s door open\n", [color:yellow("FSM state:")]),
                            fsm_loop(door_open, Read_floor, stop_dir, {cab_button, Read_floor}, Unassigned_orders);
                        false -> 
                            communicator ! {reached_new_state, #state{movement = idle, floor = Read_floor, assigned_order = none}},
                            io:format("~s idle\n", [color:yellow("FSM state:")]),
                            fsm_loop(idle, Read_floor, stop_dir, none, Unassigned_orders)
                    end;

                %----------------------------------------------------------------------------------------------
                % Elevator idle, ignores floor sensor data and remains idle.
                %----------------------------------------------------------------------------------------------
                {idle, _} -> ignore;

                %----------------------------------------------------------------------------------------------
                % Elevator door open, ignores floor sensor data and door remains open.
                %----------------------------------------------------------------------------------------------
                {door_open, _} -> ignore;

                %----------------------------------------------------------------------------------------------
                % Elevator in process of reaching a defined floor to become idle, continues unchanged.
                %----------------------------------------------------------------------------------------------
                {cancel_assigned_order, between_floors} -> continue;

                %----------------------------------------------------------------------------------------------
                % Readched a defined floor after canceling its assigned order, stops and changes state to idle.
                %----------------------------------------------------------------------------------------------
                {cancel_assigned_order, Read_floor} ->
                    driver       ! {set_motor_dir, stop_dir},
                    watchdog     ! stop_watching_movement,
                    communicator ! {reached_new_state, #state{movement = idle, floor = Read_floor, assigned_order = none}},
                    io:format("~s idle\n", [color:yellow("FSM state:")]),
                    fsm_loop(idle, Read_floor, stop_dir, none, Unassigned_orders); 

                %----------------------------------------------------------------------------------------------
                % Elevator still at its previous floor, checks if it should stop or continue moving.
                %----------------------------------------------------------------------------------------------
                {moving, Latest_floor} ->
                    case should_elevator_stop(Latest_floor, Moving_dir, Assigned_order, Unassigned_orders) of
                        true  -> 
                            driver   ! {set_motor_dir, stop_dir},
                            driver   ! {set_door_open_LED, on},
                            watchdog ! stop_watching_movement,
                            spawn(fun() -> timer:sleep(?DOOR_OPEN_TIME), fsm ! close_door end),
                            io:format("~s door open\n", [color:yellow("FSM state:")]),
                            fsm_loop(door_open, Read_floor, stop_dir, Assigned_order, Unassigned_orders);
                        false -> 
                            ignore
                    end;

                %----------------------------------------------------------------------------------------------
                % Elevator moving between floors, continues unchanged.
                %----------------------------------------------------------------------------------------------
                {moving, between_floors} -> continue;
                
                %----------------------------------------------------------------------------------------------
                % Elevator reached a new floor, checks if it should stop or continue moving.
                %----------------------------------------------------------------------------------------------
                {moving, Read_floor} ->
                    driver           ! {set_floor_LED, Read_floor},
                    watchdog         ! stop_watching_movement,
                    communicator     ! {reached_new_state, #state{movement = Moving_dir, floor = Read_floor, assigned_order = Assigned_order}},
                    case should_elevator_stop(Read_floor, Moving_dir, Assigned_order, Unassigned_orders) of
                        true ->
                            driver   ! {set_motor_dir, stop_dir},
                            driver   ! {set_door_open_LED, on},
                            spawn(fun() -> timer:sleep(?DOOR_OPEN_TIME), fsm ! close_door end),
                            io:format("~s door open\n", [color:yellow("FSM state:")]),
                            fsm_loop(door_open, Read_floor, stop_dir, Assigned_order, Unassigned_orders);
                        false ->
                            watchdog ! start_watching_movement,
                            fsm_loop(moving, Read_floor, Moving_dir,  Assigned_order, Unassigned_orders)
                    end;
                
                Unexpected ->
                    io:format("~s Unexpected message in floor_sensor pattern of fsm_loop: ~p\n", [color:red("FSM:"), Unexpected])
            end;
        
        

        %----------------------------------------------------------------------------------------------
        % Receives (locally) message for door to close after being open for 'DOOR_OPEN_TIME' ms.
        %----------------------------------------------------------------------------------------------
        close_door ->
            driver ! {set_door_open_LED, off},
            clear_orders(Latest_floor, Assigned_order),

            case Assigned_order == none of
                true ->
                    communicator ! {reached_new_state, #state{movement = idle, floor = Latest_floor, assigned_order = none}},
                    io:format("~s idle\n", [color:yellow("FSM state:")]),
                    fsm_loop(idle, Latest_floor, stop_dir, none, Unassigned_orders);

                false -> 
                    continue
            end,
            
            case Latest_floor == element(2, Assigned_order) of 
                true ->   % Served its assigned order
                    communicator ! {reached_new_state, #state{movement = idle, floor = Latest_floor, assigned_order = none}},
                    io:format("~s idle\n", [color:yellow("FSM state:")]),
                    fsm_loop(idle, Latest_floor, stop_dir, none, Unassigned_orders);

                false ->  % Continue towards the assigned order
                    New_dir = choose_direction(Assigned_order, Latest_floor),
                    driver   ! {set_motor_dir, New_dir},
                    watchdog ! start_watching_movement,
                    io:format("~s moving\n", [color:yellow("FSM state:")]),
                    fsm_loop(moving, Latest_floor, New_dir, Assigned_order, Unassigned_orders)
            end;



        %----------------------------------------------------------------------------------------------
        % Timeout for movement or an order received from 'watchdog'. Disconnects the node, waits
        % 'DISCONNECTED_TIME' ms before it restarts 'fsm'. 'node_connector' then reconnects the node.
        %----------------------------------------------------------------------------------------------
        Timeout when (Timeout == timeout_movement) or (Timeout == timeout_order) ->
            driver   ! {set_motor_dir, stop_dir},            
            disconnect_node_and_sleep(),
            watchdog ! start_watching_movement,
            io:format("~s uninitialized\n", [color:yellow("FSM state:")]),
            fsm_loop(uninitialized, undefined, stop_dir, none, []);



        Unexpected ->
            io:format("~s Unexpected message in fsm_loop: ~p\n", [color:red("FSM:"), Unexpected])

    end,
fsm_loop(State, Latest_floor, Moving_dir, Assigned_order, Unassigned_orders).
                                


      
%%=============================================================================================
%% Help functions used in 'fsm_loop':
%%=============================================================================================

%----------------------------------------------------------------------------------------------
% Tells 'node_connector' to disconnect this node. Then sleeps for 'DISCONNECTED_TIME' ms,
% disregarding all (if any) received messages before it restarts the 'fsm'.
%----------------------------------------------------------------------------------------------
disconnect_node_and_sleep() ->
    node_connector ! disconnect_node,
    spawn(fun() -> timer:sleep(?DISCONNECTED_TIME), fsm ! {disconnection, timeout} end),
    sleep_loop().

sleep_loop() ->
    receive
        {disconnection, timeout} -> restart_fsm;
        _Disregarded_message     -> sleep_loop()
    end.



%----------------------------------------------------------------------------------------------
% Returns the direction in which the elevator should move in order to serve the input order.
%----------------------------------------------------------------------------------------------
choose_direction({_Button_type, Floor}, Latest_floor) when Latest_floor == Floor -> stop_dir;
choose_direction({_Button_type, Floor}, Latest_floor) when Latest_floor <  Floor -> up_dir;
choose_direction({_Button_type, Floor}, Latest_floor) when Latest_floor >  Floor -> down_dir.



%----------------------------------------------------------------------------------------------
% Converts a motor direction to a corresponding button_type.
%----------------------------------------------------------------------------------------------
convert_to_button_type(up_dir)   -> up_button;
convert_to_button_type(down_dir) -> down_button;
convert_to_button_type(stop_dir) -> cab_button.



%----------------------------------------------------------------------------------------------
% Checks is the elevator should stop at 'Floor' when moving in the specified direction.
%----------------------------------------------------------------------------------------------
should_elevator_stop(Current_floor, Moving_dir, {_Button_type, 1}, Orders) ->
    lists:member({cab_button,  Current_floor}, Orders)                           or
    lists:member({down_button, Current_floor}, Orders)                           or
    (Current_floor  == 1)                                                        or
    ((Current_floor == ?NUMBER_OF_FLOORS) andalso (Moving_dir == up_dir));

should_elevator_stop(Current_floor, Moving_dir, {_Button_type, ?NUMBER_OF_FLOORS}, Orders) ->
    lists:member({cab_button, Current_floor}, Orders)                            or
    lists:member({up_button,  Current_floor}, Orders)                            or
    (Current_floor  == ?NUMBER_OF_FLOORS)                                        or
    ((Current_floor == 1) andalso  (Moving_dir == down_dir));

should_elevator_stop(Current_floor, Moving_dir, {Assigned_order_button_type, Assigned_order_floor}, Orders) ->
    lists:member({cab_button, Current_floor}, Orders)                            or
    (lists:member({convert_to_button_type(Moving_dir), Current_floor}, Orders)   and
    (convert_to_button_type(Moving_dir) == Assigned_order_button_type))          or
    (Current_floor  == Assigned_order_floor)                                     or
    ((Current_floor == 1) andalso  (Moving_dir  == down_dir))                    or 
    ((Current_floor == ?NUMBER_OF_FLOORS) andalso (Moving_dir == up_dir)).



%----------------------------------------------------------------------------------------------
% Send message to 'communicator' that one or multiple orders are served.
%----------------------------------------------------------------------------------------------
clear_orders(Current_floor, none) ->
    % Only clear cab orders when no order is assigned
    communicator ! {order_served, {cab_button, Current_floor}};

clear_orders(1, _Assigned_order) ->
    % Clear everything at ground floor
    communicator ! {order_served, {cab_button, 1}},
    communicator ! {order_served, {up_button,  1}};

clear_orders(?NUMBER_OF_FLOORS, _Assigned_order) ->
    % Clear everything at final floor
    communicator ! {order_served, {cab_button,  ?NUMBER_OF_FLOORS}},
    communicator ! {order_served, {down_button, ?NUMBER_OF_FLOORS}};

clear_orders(Current_floor, {Assigned_order_button_type, Assigned_order_floor}) ->
    % General case, clears relevant orders on its way to the assigned order
    communicator ! {order_served, {cab_button, Current_floor}},
    case Assigned_order_floor of
        1                  -> communicator ! {order_served, {down_button, Current_floor}};
        ?NUMBER_OF_FLOORS  -> communicator ! {order_served, {up_button,   Current_floor}};
        Current_floor      -> communicator ! {order_served, {Assigned_order_button_type, Current_floor}};
        _Destination_floor ->
            case Assigned_order_floor > Current_floor of
                true  when Assigned_order_button_type == up_button ->
                    communicator ! {order_served, {up_button,   Current_floor}};
                false when Assigned_order_button_type == down_button ->
                    communicator ! {order_served, {down_button, Current_floor}};
                _Else ->
                    continue
            end
    end.