-module(order_manager).
-export([start/0]).
-include("parameters.hrl").

-record(orders, {assigned_hall_orders = [], unassigned_hall_orders = [], cab_orders = []}).
-record(state,  {is_idle, direction, floor}).

start() ->
    main_loop(#orders{}, dict:new()). % temporary to avoid green comoplains



main_loop(Orders, Elevator_states) ->
    io:format("Main loop of order_manager/scheduler started\n"),
    receive
        %% Acknowledge the cab order and append it to 'cab_orders' of Orders if not already known
        {add_order, {cab_button, Floor}, PID} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
            PID ! {ack_order, {cab_button, Floor}},
            case lists:member(Floor, Orders#orders.cab_orders) of
                true ->
                    main_loop(Orders, Elevator_states);
                false ->
                    Updated_orders = Orders#orders{cab_orders = Orders#orders.cab_orders ++ [Floor]},
                    main_loop(Updated_orders, Elevator_states)
            end;

        %% Acknowledge the hall order and append it to 'unassigned' of Orders if not already known
        {add_order, {Button_type, Floor}, PID} when is_atom(Button_type) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
            Hall_order = {Button_type, Floor},
            PID ! {ack_order, Hall_order},
            case lists:member(Hall_order, Orders#orders.assigned_hall_orders ++ Orders#orders.unassigned_hall_orders) of
                true ->
                    main_loop(Orders, Elevator_states);
                false ->
                    Updated_orders = Orders#orders{unassigned_hall_orders = Orders#orders.unassigned_hall_orders ++ [Hall_order]},
                    main_loop(Updated_orders, Elevator_states)
            end;

        %% Removes all occourences of a cab order from 'cab_orders' of Orders
        {remove_order, {cab_button, Floor}} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            Updated_cab_orders = [X || X <- Orders#orders.cab_orders, X /= Floor],
            Updated_orders     = Orders#orders{cab_orders = Updated_cab_orders},
            main_loop(Updated_orders, Elevator_states);

        %% Removes all occourences of a hall order from 'unassigned' and 'assigned' of Orders
        {remove_order, {Button_type, Floor}} when is_atom(Button_type) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            Updated_unassigned_hall_orders = [X || X <- Orders#orders.unassigned_hall_orders, X /= {Button_type, Floor}],
            Updated_assigned_hall_orders   = [X || X <- Orders#orders.assigned_hall_orders,   X /= {Button_type, Floor}],
            Updated_orders = Orders#orders{unassigned_hall_orders = Updated_unassigned_hall_orders,
                                             assigned_hall_orders = Updated_assigned_hall_orders},
            main_loop(Updated_orders, Elevator_states);

        %% Update state of Node in Elevator_states, adding it if non-existing
        {update_state, Node, New_state} when is_atom(Node) andalso is_record(New_state, state) ->
            Updated_states = dict:store(Node, New_state, Elevator_states),
            main_loop(Orders, Updated_states);

        %% Checks is the elevator should stop at Floor when moving up
        {should_i_stop_at, Floor, up_dir}   when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            lists:member({up_button, Floor},   Orders#orders.unassigned_hall_orders) or lists:member(Floor, Orders#orders.cab_orders);
        
        %% Checks is the elevator should stop at Floor when moving down
        {should_i_stop_at, Floor, down_dir} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            lists:member({down_button, Floor}, Orders#orders.unassigned_hall_orders) or lists:member(Floor, Orders#orders.cab_orders);
        
        %% Asked to stop when already stopped; should never happen, but return true just in case
        {should_i_stop_at, _Floor, stop_dir} ->
            io:format("Error in order_manager, asked should_i_stop when already in state stoped\n"),
            true

    end.
