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
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Acknowledge the order and append it to correspoding list of 'Orders' if not already present
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Cab orders
        {add_order, {cab_button, Floor}, PID} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
            PID ! {ack_order, {cab_button, Floor}},
            case lists:member(Floor, Orders#orders.cab_orders) of
                true ->
                    main_loop(Orders, Elevator_states);
                false ->
                    Updated_orders = Orders#orders{cab_orders = Orders#orders.cab_orders ++ [Floor]},
                    main_loop(Updated_orders, Elevator_states)
            end;
        %% Hall orders
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

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Removes all occourences of an order from Orders
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Cab orders
        {remove_order, {cab_button, Floor}} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            Updated_cab_orders = [X || X <- Orders#orders.cab_orders, X /= Floor],
            Updated_orders     = Orders#orders{cab_orders = Updated_cab_orders},
            main_loop(Updated_orders, Elevator_states);
        %% Hall orders
        {remove_order, {Button_type, Floor}} when is_atom(Button_type) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            Updated_unassigned_hall_orders = [X || X <- Orders#orders.unassigned_hall_orders, X /= {Button_type, Floor}],
            Updated_assigned_hall_orders   = [X || X <- Orders#orders.assigned_hall_orders,   X /= {Button_type, Floor}],
            Updated_orders = Orders#orders{unassigned_hall_orders = Updated_unassigned_hall_orders,
                                             assigned_hall_orders = Updated_assigned_hall_orders},
            main_loop(Updated_orders, Elevator_states);

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Update state of 'Node' in 'Elevator_states', adding it if not present
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        {update_state, Node, New_state} when is_atom(Node) andalso is_record(New_state, state) ->
            Updated_states = dict:store(Node, New_state, Elevator_states),
            main_loop(Orders, Updated_states);

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Checks is the elevator should stop at 'Floor' when moving in the specified direction
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Moving up
        {should_elevator_stop, Floor, up_dir, PID}   when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
            PID ! lists:member({up_button, Floor},   Orders#orders.unassigned_hall_orders) or lists:member(Floor, Orders#orders.cab_orders),
            main_loop(Orders, Elevator_states);
        %% Moving down
        {should_elevator_stop, Floor, down_dir, PID} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
            PID ! lists:member({down_button, Floor}, Orders#orders.unassigned_hall_orders) or lists:member(Floor, Orders#orders.cab_orders),
            main_loop(Orders, Elevator_states);
        %% Already stoped, should never happen
        {should_elevator_stop, _Floor, stop_dir, PID} when is_pid(PID) ->
            io:format("Unexpected behaviour in order_manager, asked should_stop when already in state stoped\n"),
            PID ! true,
            main_loop(Orders, Elevator_states);

        {distribute_unassigned_order} ->
            do_them_magic(),
            main_loop(Orders, Elevator_states)
    end.

    do_them_magic() ->
        io:format("Magic").
