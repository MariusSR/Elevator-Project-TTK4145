%%=================================================================================================
%% This module stores and maintains known orders and states.
%%=================================================================================================

-module(order_manager).
-export([start/0]).
-include("parameters.hrl").

-define(RETRY_ASSIGNING_PERIOD, 300). % GI BEDRE NAVN OG TUNE
-record(orders, {assigned_hall_orders = [], unassigned_hall_orders = [], cab_orders = []}).
-record(state,  {movement, floor}).

%%%%%%%%%%% KAN IKKE HETE ORDER MANAGER DA DEN OGSÅ HAR STATES

start() ->
    timer:sleep(200),
    Existing_cab_orders = get_existing_cab_orders(),
    fsm ! {update_order_list, Existing_cab_orders#orders.cab_orders},
    main_loop(Existing_cab_orders, dict:new()).

main_loop(Orders, Elevator_states) ->
    io:format("Assigned: ~p      Unassigned: ~p      Cab orders: ~p\n\n", [Orders#orders.assigned_hall_orders, Orders#orders.unassigned_hall_orders, Orders#orders.cab_orders]),
    receive

        %----------------------------------------------------------------------------------------------
        % Acknowledge the order and append it to correspoding list of 'Orders' if not already present.
        %----------------------------------------------------------------------------------------------
        {add_order, {cab_button, Floor}} ->
            case lists:member({cab_button, Floor}, Orders#orders.cab_orders) of
                true ->
                    main_loop(Orders, Elevator_states);
                false ->
                    Updated_orders = Orders#orders{cab_orders = Orders#orders.cab_orders ++ [{cab_button, Floor}]},
                    fsm ! {update_order_list, Updated_orders#orders.cab_orders ++ Updated_orders#orders.unassigned_hall_orders},
                    node_communicator ! {set_order_button_LED, on, {cab_button, Floor}},
                    write_cab_order_to_file(Floor),
                    main_loop(Updated_orders, Elevator_states)
            end;

        {add_order, Hall_order, From_node} ->
            case From_node == node() of
                true  -> no_ack;    % Should not send acknowledge as it is to be added locally only
                false -> node_communicator ! {order_added, Hall_order, From_node}
            end,

            All_hall_orders = Orders#orders.unassigned_hall_orders ++
                              lists:map(fun({Assigned_order, _Node}) -> Assigned_order end, Orders#orders.assigned_hall_orders),

            case lists:member(Hall_order, All_hall_orders) of
                true ->
                    main_loop(Orders, Elevator_states);
                false ->
                    Updated_orders = Orders#orders{unassigned_hall_orders = Orders#orders.unassigned_hall_orders ++ [Hall_order]},
                    fsm ! {update_order_list, Updated_orders#orders.cab_orders ++ Updated_orders#orders.unassigned_hall_orders},
                    main_loop(Updated_orders, Elevator_states)
            end;



        %----------------------------------------------------------------------------------------------
        % Assigns an order to 'fsm'. If none orders are available it tries again periodically.
        %----------------------------------------------------------------------------------------------
        assign_order_to_fsm ->
            case Orders#orders.cab_orders of
                [Cab_order|Remaining_cab_orders] ->
                    fsm ! {assigned_order, Cab_order, Remaining_cab_orders ++ Orders#orders.unassigned_hall_orders};
                [] ->
                    case scheduler:get_most_efficient_order(Orders#orders.unassigned_hall_orders, Elevator_states) of
                        no_orders_available ->
                            spawn(fun() -> timer:sleep(?RETRY_ASSIGNING_PERIOD), order_manager ! assign_order_to_fsm end);
                        Hall_order ->
                            node_communicator ! {new_order_assigned, Hall_order},
                            fsm               ! {assigned_order, Hall_order, Orders#orders.cab_orders ++
                                                Orders#orders.unassigned_hall_orders -- [Hall_order]}
                    end
            end,
            main_loop(Orders, Elevator_states);



        %----------------------------------------------------------------------------------------------
        % Mark 'Hall_order' as assigned, moving it from unassigned to assigned of 'Orders'.
        %----------------------------------------------------------------------------------------------
        {mark_order_assigned, Hall_order, Node} ->
            case lists:member({Hall_order, Node}, Orders#orders.assigned_hall_orders) of
                true ->
                    main_loop(Orders, Elevator_states);
                false ->
                    Updated_assigned_hall_orders   = Orders#orders.assigned_hall_orders   ++ [{Hall_order, Node}],
                    Updated_unassigned_hall_orders = Orders#orders.unassigned_hall_orders -- [{Hall_order, Node}],
                    Updated_orders                 = Orders#orders{unassigned_hall_orders = Updated_unassigned_hall_orders,
                                                                     assigned_hall_orders = Updated_assigned_hall_orders},
                    fsm      ! {update_order_list, Orders#orders.cab_orders ++ Updated_unassigned_hall_orders},
                    watchdog ! {start_watching, Hall_order},
                    main_loop(Updated_orders, Elevator_states)
            end;
        


        %----------------------------------------------------------------------------------------------
        % Move 'Hall_order' from being assigned back to the list of unassigned 'Orders'.
        %----------------------------------------------------------------------------------------------
        {unmark_order_assigned, Hall_order} ->
            Should_keep_order_in_list      = fun(Assigned_hall_order) -> element(1, Assigned_hall_order) /= Hall_order end,
            Updated_assigned_hall_orders   = lists:filter(Should_keep_order_in_list, Orders#orders.assigned_hall_orders),
            Updated_unassigned_hall_orders = [Hall_order] ++ Orders#orders.unassigned_hall_orders,
            Updated_orders                 = Orders#orders{unassigned_hall_orders = Updated_unassigned_hall_orders,
                                                             assigned_hall_orders = Updated_assigned_hall_orders},
            fsm ! {update_order_list, Orders#orders.cab_orders ++ Updated_unassigned_hall_orders},
            main_loop(Updated_orders, Elevator_states);



        %----------------------------------------------------------------------------------------------
        % Removes an order from 'Orders'.
        %----------------------------------------------------------------------------------------------
        {remove_order, {cab_button, Floor}} ->
            Updated_cab_orders = Orders#orders.cab_orders -- [{cab_button, Floor}],
            Updated_orders     = Orders#orders{cab_orders = Updated_cab_orders},
            fsm ! {update_order_list, Updated_cab_orders ++ Updated_orders#orders.unassigned_hall_orders},
            remove_cab_order_from_file(Floor),
            main_loop(Updated_orders, Elevator_states);

        {remove_order, Hall_order} ->
            Updated_unassigned_hall_orders = Orders#orders.unassigned_hall_orders -- [Hall_order],
            Updated_assigned_hall_orders   = Orders#orders.assigned_hall_orders   -- [Hall_order],
            Updated_orders                 = Orders#orders{unassigned_hall_orders = Updated_unassigned_hall_orders,
                                                             assigned_hall_orders = Updated_assigned_hall_orders},
            fsm      ! {update_order_list, Orders#orders.cab_orders ++ Updated_unassigned_hall_orders},
            watchdog ! {stop_watching, Hall_order},
            main_loop(Updated_orders, Elevator_states);



        %----------------------------------------------------------------------------------------------
        % Updates state of 'Node' in 'Elevator_states', adding it if not already present.
        %----------------------------------------------------------------------------------------------
        {update_state, Node, New_state} when Node == node() andalso New_state#state.movement == stop_dir ->
            Updated_states = dict:store(Node, New_state, Elevator_states),
            order_manager ! assign_order_to_fsm,
            main_loop(Orders, Updated_states);

        {update_state, Node, New_state} ->
            Updated_states = dict:store(Node, New_state, Elevator_states),
            main_loop(Orders, Updated_states);



        %----------------------------------------------------------------------------------------------
        % Moves orders assigned to 'Node' to the the front of the list of unassigend orders of 'Orders'
        %----------------------------------------------------------------------------------------------
        {node_down, Node} ->
            Orders_assigned_to_offline_node = lists:filter(fun({_Order, Assigned_node}) -> Assigned_node == Node end, Orders#orders.assigned_hall_orders),
            Hall_orders_extracted           = lists:map(fun({Order, _Node}) -> Order end, Orders_assigned_to_offline_node),
            Updated_assigned_hall_orders    = Orders#orders.assigned_hall_orders -- Hall_orders_extracted,
            Updated_unassigned_hall_orders  = Hall_orders_extracted ++ Orders#orders.unassigned_hall_orders,
            Updated_orders                  = Orders#orders{unassigned_hall_orders = Updated_unassigned_hall_orders,
                                                              assigned_hall_orders = Updated_assigned_hall_orders},
            fsm ! {update_order_list, Orders#orders.cab_orders ++ Updated_unassigned_hall_orders},
            main_loop(Updated_orders, Elevator_states);
        


        %----------------------------------------------------------------------------------------------
        % Sends (and receives) a copy of existing hall orders and current states to the newly connected
        %----------------------------------------------------------------------------------------------
        {node_up, New_node} ->
            node_communicator ! {sync_hall_orders_and_states, New_node, Orders#orders.assigned_hall_orders, Orders#orders.unassigned_hall_orders, Elevator_states},
            main_loop(Orders, Elevator_states);

        {existing_hall_orders_and_states, Updated_assigned_hall_orders, Updated_unassigned_hall_orders, Updated_elevator_states} ->
            Updated_orders = Orders#orders{assigned_hall_orders = Updated_assigned_hall_orders, unassigned_hall_orders = Updated_unassigned_hall_orders},
            fsm ! {update_order_list, Orders#orders.cab_orders ++ Updated_unassigned_hall_orders},
            main_loop(Updated_orders, Updated_elevator_states);



        Unexpected ->
            io:format("Unexpected message in order_manager: ~p~n", [Unexpected]),
            main_loop(Orders, Elevator_states)

    end.



%----------------------------------------------------------------------------------------------
% File I/O functions storing/reading local cab orders to a dets file
%----------------------------------------------------------------------------------------------
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