%%=================================================================================================
%% HER MÅ DET SKRIVES NOE FORNUFTIG. F.eks. bortfrklar hvorfor filen er så lang ...
%%=================================================================================================

-module(fsm).
-export([start/0]).

-include("parameters.hrl").
-define(DOOR_OPEN_TIME, 2000).
-define(FLOOR_SENSOR_SLEEP_BETWEEN_FLOORS, 100).
-define(DISCONNECTED_TIME, 5000).

-record(state, {movement, floor}).

% SKAL VI LA ALLE REKURSIVE KALL GJØRES I BODYEN, ALTSÅ INGEN PÅ SLUTTEN?
% ENDRE LATEST FLOOR TIL PREV_FLOOR

start() ->
    timer:sleep(200),
    io:format("~s Uninitialized\n", [color:yellow("FSM state:")]),
    watchdog ! start_watching_movement,
    fsm_loop(uninitialized, undefined, stop_dir, none, []).


fsm_loop(State, Latest_floor, Moving_dir, Assigned_order, Unassigned_order_list) ->
    io:format("~s   S: ~p    Lf: ~p     Md: ~p    Ao: ~p    Uo: ~p\n", [color:greenb("FSM_loop arguments:"), State, Latest_floor, Moving_dir, Assigned_order, Unassigned_order_list]),

    receive
        
        %----------------------------------------------------------------------------------------------
        % Receives a new order to be completed by this elevator.
        %----------------------------------------------------------------------------------------------
        {assigned_order, New_assigned_order, Updated_unassigned_order_list} when State == idle ->
            case choose_direction(New_assigned_order, Latest_floor) of 
                stop_dir ->
                    driver ! {set_door_open_LED, on},
                    spawn(fun() -> timer:sleep(?DOOR_OPEN_TIME), fsm ! close_door end),
                    fsm_loop(door_open, Latest_floor, stop_dir, New_assigned_order, Updated_unassigned_order_list);
                
                Direction_headed ->
                    communicator ! {reached_new_state, #state{movement = Direction_headed, floor = Latest_floor}},
                    io:format("~s Moving\n", [color:yellow("FSM state:")]),
                    driver ! {set_motor_dir, Direction_headed},
                    watchdog ! start_watching_movement,
                    fsm_loop(moving, Latest_floor, Direction_headed, New_assigned_order, Updated_unassigned_order_list)
            end;
        


        %----------------------------------------------------------------------------------------------
        % Cancels the assigend order if it is already served by another elevator.
        %----------------------------------------------------------------------------------------------
        cancel_assigned_order when Assigned_order == none -> ignore; % Should never happen
        cancel_assigned_order ->
            fsm_loop(cancel_assigned_order, Latest_floor, Moving_dir, none, Unassigned_order_list);



        %----------------------------------------------------------------------------------------------
        % Receives and updates an updated list of unassigned orders (including cab orders).
        %----------------------------------------------------------------------------------------------
        {update_order_list, Updated_unassigned_order_list} ->
            fsm_loop(State, Latest_floor, Moving_dir, Assigned_order, Updated_unassigned_order_list);



        %----------------------------------------------------------------------------------------------
        % Handles new samples from teh hardware_reader module:
        %----------------------------------------------------------------------------------------------
        {floor_sensor, Read_floor} ->
            case {State, Read_floor} of
                %----------------------------------------------------------------------------------------------
                % Elevator not yet initialized, starts moving down until it reaches a defined floor.
                %----------------------------------------------------------------------------------------------
                {uninitialized, between_floors} ->
                    driver   ! {set_motor_dir, down_dir},
                    fsm_loop(uninitialized, undefined, down_dir, none, Unassigned_order_list);

                %----------------------------------------------------------------------------------------------
                % Elevator not yet initialized, reached a defined floor and finishes initialization.
                %----------------------------------------------------------------------------------------------
                {uninitialized, Read_floor} ->
                    driver   ! {set_motor_dir, stop_dir},
                    driver   ! {set_floor_LED, Read_floor},
                    watchdog ! stop_watching_movement,
                    case lists:member({cab_button, Read_floor}, Unassigned_order_list) of
                        true  -> 
                            driver ! {set_door_open_LED, on},
                            spawn(fun() -> timer:sleep(?DOOR_OPEN_TIME), fsm ! close_door end),
                            io:format("~s Door open\n", [color:yellow("FSM state:")]),
                            fsm_loop(door_open, Read_floor, stop_dir, {cab_button, Read_floor}, Unassigned_order_list);
                        false -> 
                            communicator ! {reached_new_state, #state{movement = idle, floor = Read_floor}},
                            io:format("~s Idle\n", [color:yellow("FSM state:")]),
                            fsm_loop(idle, Read_floor, stop_dir, none, Unassigned_order_list)
                    end;

                %----------------------------------------------------------------------------------------------
                % Elevator idle, ignores floor sensor data and remains idle.
                %----------------------------------------------------------------------------------------------
                {idle, _} -> ignore;

                %----------------------------------------------------------------------------------------------
                % Elevator door open, ignores floor sensor data.
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
                    io:format("~s cancel_assigned_order\n", [color:yellow("FSM:")]),
                    driver   ! {set_motor_dir, stop_dir},
                    watchdog ! stop_watching_movement,
                    communicator ! {reached_new_state, #state{movement = idle, floor = Read_floor}},
                    io:format("~s Idle\n", [color:yellow("FSM state:")]),
                    fsm_loop(idle, Read_floor, stop_dir, none, Unassigned_order_list); 

                %----------------------------------------------------------------------------------------------
                % Elevator still at its previous floor, checks if it should stop or continue moving.
                %----------------------------------------------------------------------------------------------
                {moving, Latest_floor} -> % Still at the same floor
                    case should_elevator_stop(Latest_floor, Moving_dir, Assigned_order, Unassigned_order_list) of
                        true  -> 
                            driver   ! {set_motor_dir, stop_dir},
                            watchdog ! stop_watching_movement,
                            driver   ! {set_door_open_LED, on},
                            spawn(fun() -> timer:sleep(?DOOR_OPEN_TIME), fsm ! close_door end),
                            io:format("~s Door open\n", [color:yellow("FSM state:")]),
                            fsm_loop(door_open, Read_floor, stop_dir, Assigned_order, Unassigned_order_list);
                        false -> 
                            ignore
                    end;

                %----------------------------------------------------------------------------------------------
                % Elevator moving between floors, continues unchanged.
                %----------------------------------------------------------------------------------------------
                {moving, between_floors} -> ok;
                
                %----------------------------------------------------------------------------------------------
                % Elevator reached a new floor, checks if it should stop or continue moving.
                %----------------------------------------------------------------------------------------------
                {moving, Read_floor} ->
                    driver ! {set_floor_LED, Read_floor},
                    communicator ! {reached_new_state, #state{movement = Moving_dir, floor = Read_floor}},
                    watchdog ! stop_watching_movement,                                                              
                    case should_elevator_stop(Read_floor, Moving_dir, Assigned_order, Unassigned_order_list) of
                        true ->
                            driver ! {set_motor_dir, stop_dir},
                            driver ! {set_door_open_LED, on},
                            spawn(fun() -> timer:sleep(?DOOR_OPEN_TIME), fsm ! close_door end),
                            io:format("~s Door open\n", [color:yellow("FSM state:")]),
                            fsm_loop(door_open, Read_floor, stop_dir, Assigned_order, Unassigned_order_list);
                        false ->
                            watchdog ! start_watching_movement,
                            fsm_loop(moving, Read_floor, Moving_dir, Assigned_order, Unassigned_order_list)
                    end;
                
                %----------------------------------------------------------------------------------------------
                % Unexpected pattern match.
                %----------------------------------------------------------------------------------------------
                Unexpected ->
                    io:format("~s pattern match in fsm, {floor_sensor, Read_floor}: ~p\n", [color:red("Unexpected:"), Unexpected])
            end;
        
        

        %----------------------------------------------------------------------------------------------
        % Receives message for door to close after being open for 'DOOR_OPEN_TIME' ms.
        %----------------------------------------------------------------------------------------------
        close_door ->
            io:format("~s Closing door\n", [color:yellow("FSM:")]),
            driver ! {set_door_open_LED, off},
            clear_orders(Latest_floor, Assigned_order),

            case Assigned_order == none of
                true ->
                    io:format("~s Idle YOLOOOOO\n", [color:yellow("FSM state:")]), 
                    communicator ! {reached_new_state, #state{movement = idle, floor = Latest_floor}},
                    fsm_loop(idle, Latest_floor, stop_dir, none, Unassigned_order_list);
                false -> 
                    continue
            end,
            
            case Latest_floor == element(2, Assigned_order) of 
                true ->            
                    communicator ! {reached_new_state, #state{movement = idle, floor = Latest_floor}},
                    io:format("~s Idle\n", [color:yellow("FSM state:")]),
                    fsm_loop(idle, Latest_floor, stop_dir, none, Unassigned_order_list);
                false ->
                    Direction_headed = choose_direction(Assigned_order, Latest_floor),
                    driver ! {set_motor_dir, Direction_headed},
                    io:format("~s Moving\n", [color:yellow("FSM state:")]),
                    watchdog ! start_watching_movement,
                    fsm_loop(moving, Latest_floor, Direction_headed, Assigned_order, Unassigned_order_list)
            end;



        %----------------------------------------------------------------------------------------------
        % Elevator movement between or an assigned order timed out. Disconnects the node, waits for
        % 'DISCONNECTED_TIME' ms before it restarts 'fsm'. 'node_connection' then reconnects the node.
        %----------------------------------------------------------------------------------------------
        Timeout when (Timeout == timeout_movement) or (Timeout == timeout_order) ->
            io:format("~s ~s\n", [color:yellow("FSM:"), Timeout]),
            driver   ! {set_motor_dir, stop_dir},            
            disconnect_node_and_sleep(),
            io:format("~s Uninitialized\n", [color:yellow("FSM state:")]),
            watchdog ! start_watching_movement,
            fsm_loop(uninitialized, undefined, stop_dir, none, []);


        %----------------------------------------------------------------------------------------------
        % Unexpected message.
        %----------------------------------------------------------------------------------------------
        Unexpected ->
            io:format("~s error in fsm main recv: ~p\n", [color:red("Unexpected:"), Unexpected])
            
    end,
fsm_loop(State, Latest_floor, Moving_dir, Assigned_order, Unassigned_order_list).
                                


                       
%%=============================================================================================
%% Help functiojns used in 'fsm_loop':
%%=============================================================================================

%----------------------------------------------------------------------------------------------
% Tells 'node_connection' to disconnect this node. Then sleeps for 'DISCONNECTED_TIME' ms,
% disregarding all (if any) received messages before it restarts the 'fsm'
%----------------------------------------------------------------------------------------------
disconnect_node_and_sleep() ->
    node_connection ! disconnect_node,
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



%----------------------------------------------------------------------------------------------
% Converts a motor direction to a corresponding button_type.
%----------------------------------------------------------------------------------------------
convert_to_button_type(up_dir)   -> up_button;
convert_to_button_type(down_dir) -> down_button;
convert_to_button_type(stop_dir) -> cab_button.



%----------------------------------------------------------------------------------------------
% Checks is the elevator should stop at 'Floor' when moving in the specified direction.
%----------------------------------------------------------------------------------------------
should_elevator_stop(_Floor, _Moving_dir, none, _Orders) ->
    io:format("~s SHOULD NOT HAPPEN\n", [color:redb("should_elevator_stop")]), % fix this print
    true;

should_elevator_stop(Floor, Moving_dir, {_Button_type, 1}, Orders) ->
    lists:member({cab_button, Floor}, Orders)                           or
    lists:member({down_button, Floor}, Orders)                          or
    (Floor  == 1)                                                       or
    ((Floor == ?NUMBER_OF_FLOORS) andalso (Moving_dir == up_dir));

should_elevator_stop(Floor, Moving_dir, {_Button_type, ?NUMBER_OF_FLOORS}, Orders) ->
    lists:member({cab_button, Floor}, Orders)                           or
    lists:member({up_button, Floor}, Orders)                            or
    (Floor  == ?NUMBER_OF_FLOORS)                                       or
    ((Floor == 1) andalso (Moving_dir == down_dir));

should_elevator_stop(Floor, Moving_dir, {Assigned_order_button_type, Assigned_order_floor}, Orders) ->
    lists:member({cab_button, Floor}, Orders)                           or
   (lists:member({convert_to_button_type(Moving_dir), Floor}, Orders)   and
   (convert_to_button_type(Moving_dir) == Assigned_order_button_type))  or
   (Floor  == Assigned_order_floor)                                     or
   ((Floor == 1) andalso (Moving_dir == down_dir))                      or 
   ((Floor == ?NUMBER_OF_FLOORS) andalso (Moving_dir == up_dir)).



%----------------------------------------------------------------------------------------------
% Send message to 'communicator' that one or multiple orders are served.
%----------------------------------------------------------------------------------------------
clear_orders(Current_floor, none) ->
    % Only clear cab orders when nothing assigned
    communicator ! {order_finished, {cab_button, Current_floor}};

clear_orders(1, _Assigned_order) ->
    % Clear everything at ground floor
    communicator ! {order_finished, {cab_button, 1}},
    communicator ! {order_finished, {up_button, 1}};

clear_orders(?NUMBER_OF_FLOORS, _Assigned_order) ->
    % Clear everything at final floor
    communicator ! {order_finished, {cab_button, ?NUMBER_OF_FLOORS}},
    communicator ! {order_finished, {down_button, ?NUMBER_OF_FLOORS}};

clear_orders(Current_floor, {Assigned_order_button_type, Assigned_order_floor}) ->
    % General case, clears relevant orders on its way to the assigned order
    communicator ! {order_finished, {cab_button, Current_floor}},
    case Assigned_order_floor of
        1                 -> communicator ! {order_finished, {down_button, Current_floor}};
        ?NUMBER_OF_FLOORS -> communicator ! {order_finished, {up_button, Current_floor}};
        Current_floor     -> communicator ! {order_finished, {Assigned_order_button_type, Current_floor}};
        _Destination      ->
            case Assigned_order_floor > Current_floor of
                true  when Assigned_order_button_type == up_button ->
                    communicator ! {order_finished, {up_button, Current_floor}};

                false when Assigned_order_button_type == down_button ->
                    communicator ! {order_finished, {down_button, Current_floor}};
                
                _Else ->
                    continue
            end
    end.