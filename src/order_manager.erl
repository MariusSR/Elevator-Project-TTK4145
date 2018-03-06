-module(order_manager).
-export([start/0]).
-include("parameters.hrl").

-record(orders, {assigned_hall_orders = [], unassigned_hall_orders = [], cab_orders = []}).
-record(state, {is_idle, direction, floor}).

start() ->
    main_loop([], []). % temporary to avoid green comoplains



main_loop(Orders, Global_states) ->
    io:format("Main loop of order_manager/scheduler started\n"),
    receive
        %% Acknowledge the cab order and append it to 'cab_orders' of Orders if not already known
        {add_order, {cab_button, Floor}, PID} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
            PID ! {ack_order, {cab_button, Floor}},
            case lists:member(Floor, Orders#orders.cab_orders) of
                true ->
                    main_loop(Orders, Global_states);
                false ->
                    Updated_order_list = Orders#orders{cab_orders = Orders#orders.cab_orders ++ [Floor]},
                    main_loop(Updated_order_list, Global_states)
            end;

        %% Acknowledge the hall order and append it to 'unassigned' of Orders if not already known
        {add_order, {Button_type, Floor}, PID} when is_atom(Button_type) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
            Hall_order = {Button_type, Floor},
            PID ! {ack_order, Hall_order},
            case lists:member(Hall_order, Orders#orders.assigned_hall_orders ++ Orders#orders.unassigned_hall_orders) of
                true ->
                    main_loop(Orders, Global_states);
                false ->
                    Updated_order_list = Orders#orders{unassigned_hall_orders = Orders#orders.unassigned_hall_orders ++ [Hall_order]},
                    main_loop(Updated_order_list, Global_states)
            end;

        %% Removes all occourences of a cab order from 'cab_orders' of Orders
        {remove_order, {cab_button, Floor}} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            Updated_cab_orders = [X || X <- Orders#orders.cab_orders, X /= Floor],
            Updated_order_list = Orders#orders{cab_orders = Updated_cab_orders},
            main_loop(Updated_order_list, Global_states);

        %% Removes all occourences of a hall order from 'unassigned' and 'assigned' of Orders
        {remove_order, {Button_type, Floor}} when is_atom(Button_type) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            Updated_unassigned_hall_orders = [X || X <- Orders#orders.unassigned_hall_orders, X /= {Button_type, Floor}],
            Updated_assigned_hall_orders   = [X || X <- Orders#orders.assigned_hall_orders,   X /= {Button_type, Floor}],
            Updated_order_list = Orders#orders{unassigned_hall_orders = Updated_unassigned_hall_orders, assigned_hall_orders = Updated_assigned_hall_orders},
            main_loop(Updated_order_list, Global_states)

    end.