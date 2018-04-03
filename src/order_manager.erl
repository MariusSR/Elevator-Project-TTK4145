-module(order_manager).
-export([start/0]).
-include("parameters.hrl").

-record(orders, {assigned_hall_orders = [], unassigned_hall_orders = [], cab_orders = []}).
-record(state,  {movement, floor}).

start() ->
    Existing_cab_orders = get_existing_cab_orders(),
    % legg til tilsvarende for states
    main_loop(Existing_cab_orders, dict:new()).

main_loop(Orders, Elevator_states) ->
    case (Orders#orders.assigned_hall_orders =/= []) or (Orders#orders.unassigned_hall_orders =/= []) or (Orders#orders.cab_orders =/= []) of
        true ->
            io:format("Orders: ~p         ~p         ~p~n", [Orders#orders.assigned_hall_orders, Orders#orders.unassigned_hall_orders, Orders#orders.cab_orders]);
        false ->
            ok
    end, 
    %io:format("States: ~p\n", [Elevator_states]),
    receive
        %----------------------------------------------------------------------------------------------
        % Acknowledge the order and append it to correspoding list of 'Orders' if not already present
        %----------------------------------------------------------------------------------------------
        % Cab orders
        {add_order, {cab_button, Floor}, From_node} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_atom(From_node) ->
            %io:format("Received add order in Order Manager:~p\n", [{cab_button, Floor}]),
            case lists:member({cab_button, Floor}, Orders#orders.cab_orders) of
                true ->
                    main_loop(Orders, Elevator_states);
                false ->
                    Updated_orders = Orders#orders{cab_orders = Orders#orders.cab_orders ++ [{cab_button, Floor}]},
                    node_communicator ! {set_order_button_LED, on, {cab_button, Floor}},
                    write_cab_order_to_file(Floor),
                    main_loop(Updated_orders, Elevator_states)
            end;
        % Hall orders
        {add_order, {Hall_button, Floor}, From_node} when is_atom(Hall_button) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_atom(From_node) ->
            %io:format("Received add order in Order Manager:~p\n", [{Hall_button, Floor}]),
            Hall_order = {Hall_button, Floor},
            case From_node == node() of
                true ->
                    ok; % Should not send acknowledge as it is to be added locally only
                false ->
                    node_communicator ! {order_added, Hall_order, From_node}
            end,
            Assigned_hall_orders = lists:map(fun({Assigned_order, _Node}) -> Assigned_order end, Orders#orders.assigned_hall_orders), % creats a list of the orders only. GI DENNE BVARIABLENE BEDRE NAVN?
            case lists:member(Hall_order, Assigned_hall_orders ++ Orders#orders.unassigned_hall_orders) of
                true ->
                    main_loop(Orders, Elevator_states);
                false ->
                    Updated_orders = Orders#orders{unassigned_hall_orders = Orders#orders.unassigned_hall_orders ++ [Hall_order]},
                    main_loop(Updated_orders, Elevator_states)
            end;

        %----------------------------------------------------------------------------------------------
        % Mark 'Hall_order' as assigned, moving it from unassigned to assigned of 'Orders'
        %----------------------------------------------------------------------------------------------
        {mark_order_assigned, Hall_order, Node} ->
            %io:format("Received mark order assigned in Order Manager:~p\n", [Hall_order]),
            case lists:member({Hall_order, Node}, Orders#orders.assigned_hall_orders) of
                true ->
                    main_loop(Orders, Elevator_states);
                false ->
                    Updated_assigned_hall_orders   = Orders#orders.assigned_hall_orders ++ [{Hall_order, Node}],
                    Updated_unassigned_hall_orders = [X || X <- Orders#orders.unassigned_hall_orders,   X /= Hall_order], %% endre til å bruke lists:fitler?
                    Updated_orders                 = Orders#orders{unassigned_hall_orders = Updated_unassigned_hall_orders,
                                                                     assigned_hall_orders = Updated_assigned_hall_orders},
                    watchdog ! {start_watching, Hall_order},
                    main_loop(Updated_orders, Elevator_states)
            end;
        
        %----------------------------------------------------------------------------------------------
        % Move 'Hall_order' from being assigned back to the list of unassigned orders
        %----------------------------------------------------------------------------------------------
        {unassign_hall_order, Hall_order} ->
            Is_not_same_order              = fun(Assigned_hall_order) -> element(1, Assigned_hall_order) /= Hall_order end,
            Updated_assigned_hall_orders   = lists:filter(Is_not_same_order, Orders#orders.assigned_hall_orders),
            Updated_unassigned_hall_orders = [Hall_order] ++ Orders#orders.unassigned_hall_orders,
            Updated_orders                 = Orders#orders{unassigned_hall_orders = Updated_unassigned_hall_orders,
                                                             assigned_hall_orders = Updated_assigned_hall_orders},
            main_loop(Updated_orders, Elevator_states);

        %----------------------------------------------------------------------------------------------
        % Removes all occourences of an order from Orders
        %----------------------------------------------------------------------------------------------
        % Cab orders
        {remove_order, {cab_button, Floor}} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            %io:format("Received remove order in Order Manager:~p\n", [{cab_button, Floor}]),
            Updated_cab_orders = [X || X <- Orders#orders.cab_orders, X /= {cab_button, Floor}], %bruke filter istedet?
            Updated_orders     = Orders#orders{cab_orders = Updated_cab_orders},
            remove_cab_order_from_file(Floor),
            main_loop(Updated_orders, Elevator_states);
        % Hall orders
        {remove_order, {Hall_button, Floor}} when is_atom(Hall_button) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            %io:format("Received remove order in Order Manager:~p\n", [{Hall_button, Floor}]),
            Hall_order = {Hall_button, Floor},
            Updated_unassigned_hall_orders = lists:filter(fun(Order) -> Order /= Hall_order end, Orders#orders.unassigned_hall_orders), %[X || X <- Orders#orders.unassigned_hall_orders, X /= Hall_order], %update these to use filter?
            Updated_assigned_hall_orders   = lists:filter(fun({Order, _Node}) -> Order /= Hall_order end, Orders#orders.assigned_hall_orders), % [X || X <- Orders#orders.assigned_hall_orders,   X /= {Hall_order, _}],
            Updated_orders                 = Orders#orders{unassigned_hall_orders = Updated_unassigned_hall_orders,
                                                             assigned_hall_orders = Updated_assigned_hall_orders},
            watchdog ! {stop_watching, Hall_order},
            main_loop(Updated_orders, Elevator_states);

        %----------------------------------------------------------------------------------------------
        % Update state of 'Node' in 'Elevator_states', adding it if not present
        %----------------------------------------------------------------------------------------------
        {update_state, Node, New_state} when is_atom(Node) andalso is_record(New_state, state) ->
            %io:format("Received update state in Order Manager, NODE: ~p, STATE: ~w\n", [Node, New_state]),
            Updated_states = dict:store(Node, New_state, Elevator_states),
            main_loop(Orders, Updated_states);

        %----------------------------------------------------------------------------------------------
        % Checks is the elevator should stop at 'Floor' when moving in the specified direction
        %----------------------------------------------------------------------------------------------
        % Moving up
        {should_elevator_stop, Floor, up_dir, PID} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
            PID ! lists:member({cab_button, Floor}, Orders#orders.cab_orders) or
                  lists:member({up_button,  Floor}, Orders#orders.unassigned_hall_orders), % ++ Orders#orders.assigned_hall_orders),
            main_loop(Orders, Elevator_states);
        % Moving down
        {should_elevator_stop, Floor, down_dir, PID} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
            PID ! lists:member({cab_button,  Floor}, Orders#orders.cab_orders) or
                  lists:member({down_button, Floor}, Orders#orders.unassigned_hall_orders), % ++ Orders#orders.assigned_hall_orders),
            main_loop(Orders, Elevator_states);
        % Idle elevator
        {should_elevator_stop, Floor, stop_dir, PID} when is_pid(PID) ->
            PID ! lists:member({cab_button, Floor}, Orders#orders.cab_orders),
            main_loop(Orders, Elevator_states);

        %----------------------------------------------------------------------------------------------
        % Returns an order to be assigned if there exists a suitable one, prioritizing cab orders
        %----------------------------------------------------------------------------------------------
        {get_unassigned_order, PID} when is_pid(PID) ->            
            case Orders#orders.cab_orders of
                [Order|_Remaining_orders] ->
                    PID ! Order;
                [] ->
                    case scheduler:get_most_efficient_order(Orders#orders.unassigned_hall_orders, Elevator_states) of
                        no_orders_available ->
                            PID ! no_orders_available;
                        Order ->
                            node_communicator ! {new_order_assigned, Order},
                            PID ! Order
                    end
            end,
            main_loop(Orders, Elevator_states);

        %----------------------------------------------------------------------------------------------
        % Moves orders assigned to 'Node' to the the fron of the list of unassigend orders
        %----------------------------------------------------------------------------------------------
        {node_down, Node} ->
            Orders_assigned_to_offline_node = lists:filter(fun({_Order, Assigned_node}) -> Assigned_node == Node end, Orders#orders.assigned_hall_orders),
            Hall_orders_extracted           = lists:map(fun({Order, _Node}) -> Order end, Orders_assigned_to_offline_node),
            Updated_assigned_hall_orders    = lists:filter(fun({_Order, Assigned_node}) -> Assigned_node /= Node end, Orders#orders.assigned_hall_orders),
            Updated_unassigned_hall_orders  = Hall_orders_extracted ++ Orders#orders.unassigned_hall_orders,
            Updated_orders                  = Orders#orders{unassigned_hall_orders = Updated_unassigned_hall_orders,
                                                              assigned_hall_orders = Updated_assigned_hall_orders},
            main_loop(Updated_orders, Elevator_states);
        
        %----------------------------------------------------------------------------------------------
        % Sends a copy of existing hall orders to the newly connected node
        %----------------------------------------------------------------------------------------------
        {node_up, New_node} ->
            node_communicator ! {sync_hall_orders_with_new_node, New_node, Orders#orders.assigned_hall_orders, Orders#orders.unassigned_hall_orders},
            main_loop(Orders, Elevator_states);

        {existing_hall_orders, Updated_assigned_hall_orders, Updated_unassigend_hall_orders} ->
            % Denne beskjeden vil komme 1 gang per eksisterende annen node. Kanskje vi skal ta unionen av dem alle? Burde være like though. Nå vinner den siste.
            Updated_orders = Orders#orders{assigned_hall_orders = Updated_assigned_hall_orders, unassigned_hall_orders = Updated_unassigend_hall_orders},
            main_loop(Updated_orders, Elevator_states);

        Unexpected ->
            io:format("Unexpected message in order_manager: ~p~n", [Unexpected])
    end.


get_existing_cab_orders() ->
    Cab_orders = get_existing_cab_orders_from_file(),
    lists:foreach(fun(Cab_order) -> node_communicator ! {set_order_button_LED, on, Cab_order} end, Cab_orders),
    #orders{cab_orders = Cab_orders}.

get_existing_cab_orders_from_file() ->
    File_name = list_to_atom("elevator@" ++ node_connection:get_IP()),
    dets:open_file(File_name, [{type, bag}]),
    Cab_orders = dets:lookup(File_name, cab_button),
    dets:close(File_name),
    Cab_orders.

write_cab_order_to_file(Floor) ->
    File_name = node(),
    dets:open_file(File_name, [{type, bag}]),
    dets:insert(File_name, {cab_button, Floor}),
    dets:close(File_name).

remove_cab_order_from_file(Floor) ->
    File_name = node(),
    dets:open_file(File_name, [{type, bag}]),
    dets:delete_object(File_name, {cab_button, Floor}),
    dets:close(File_name).