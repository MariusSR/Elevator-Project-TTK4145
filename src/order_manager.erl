-module(order_manager).
-export([start/0]).
-include("parameters.hrl").

-record(orders, {assigned_hall_orders = [], unassigned_hall_orders = [], cab_orders = []}).
-record(state,  {movement, floor}).

start() ->
    main_loop(#orders{}, dict:new()). % temporary to avoid green comoplains



main_loop(Orders, Elevator_states) ->
    io:format("Main loop of order_manager/scheduler started\n"),
    receive
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Acknowledge the order and append it to correspoding list of 'Orders' if not already present
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Cab orders
        {add_order, {cab_button, Floor}, From_node} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_atom(From_node) ->
            case From_node == node() of
                true ->
                    ok; % Only added locally, no acknowledge needed
                false ->
                    node_communicator ! {order_added, {cab_button, Floor}, From_node}
            end,
            case lists:member(Floor, Orders#orders.cab_orders) of
                true ->
                    main_loop(Orders, Elevator_states);
                false ->
                    Updated_orders = Orders#orders{cab_orders = Orders#orders.cab_orders ++ [Floor]},
                    main_loop(Updated_orders, Elevator_states)
            end;
        %% Hall orders
        {add_order, {Hall_button, Floor}, From_node} when is_atom(Hall_button) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_atom(From_node) ->
            Hall_order = {Hall_button, Floor},
            case From_node == node() of
                true ->
                    ok; % Only added locally, no acknowledge needed
                false ->
                    node_communicator ! {order_added, Hall_order}
            end,
            case lists:member(Hall_order, Orders#orders.assigned_hall_orders ++ Orders#orders.unassigned_hall_orders) of
                true ->
                    main_loop(Orders, Elevator_states);
                false ->
                    Updated_orders = Orders#orders{unassigned_hall_orders = Orders#orders.unassigned_hall_orders ++ [Hall_order]},
                    main_loop(Updated_orders, Elevator_states)
            end;

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Mark 'Hall_order' as assigned, moving it from unassigned to assigned of 'Orders'
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        {mark_order_assigned, Hall_order} ->
            case lists:member(Hall_order, Orders#orders.assigned_hall_orders) of
                true ->
                    main_loop(Orders, Elevator_states);
                false ->
                    Updated_assigned_hall_orders   = Orders#orders.assigned_hall_orders ++ [Hall_order],
                    Updated_unassigned_hall_orders = [X || X <- Orders#orders.unassigned_hall_orders,   X /= Hall_order],
                    Updated_orders = Orders#orders{unassigned_hall_orders = Updated_unassigned_hall_orders,
                                                     assigned_hall_orders = Updated_assigned_hall_orders},
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
        {remove_order, {Hall_button, Floor}} when is_atom(Hall_button) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            Updated_unassigned_hall_orders = [X || X <- Orders#orders.unassigned_hall_orders, X /= {Hall_button, Floor}],
            Updated_assigned_hall_orders   = [X || X <- Orders#orders.assigned_hall_orders,   X /= {Hall_button, Floor}],
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
        {should_elevator_stop, Floor, up_dir, PID} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
            PID ! lists:member(Floor, Orders#orders.cab_orders) or
                  lists:member({up_button, Floor}, Orders#orders.unassigned_hall_orders ++ Orders#orders.unassigned_hall_orders),
            main_loop(Orders, Elevator_states);
        %% Moving down
        {should_elevator_stop, Floor, down_dir, PID} when Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
            PID ! lists:member(Floor, Orders#orders.cab_orders) or
                  lists:member({up_button, Floor}, Orders#orders.unassigned_hall_orders ++ Orders#orders.unassigned_hall_orders),
            main_loop(Orders, Elevator_states);
        %% Already stoped, should never happen
        {should_elevator_stop, _Floor, stop_dir, PID} when is_pid(PID) ->
            io:format("Unexpected behaviour in order_manager, asked should_stop when already in state stoped\n"),
            PID ! true,
            main_loop(Orders, Elevator_states);

        {get_unassigned_order, PID} when is_pid(PID) ->
            Oldest_unassigned_hall_order = hd(Orders#orders.unassigned_hall_orders),
            node_communicator ! {new_order_assigned, Oldest_unassigned_hall_order},
            PID ! {order_assigned, Oldest_unassigned_hall_order},
            main_loop(Orders, Elevator_states)
    end.

    % do_them_magic(Orders, Elevator_states) ->
    %     io:format("Magic"),
    %     Oldest_unassigned_hall_order = hd(Orders#orders.unassigned_hall_orders),
    %     Idle_elevators = get_idle_elevator(Elevator_states).
    %     % [X] Stopp ved alle floors der det er en order.
    %     % [ ] Ta alltid den eldste ordren når idle.
    %     % [ ] Si i fra om at du ranet en ordre, slik at den som mistet ordren sin må få beskjed om å finne seg en ny ordre.
    %     % [ ] Hvis brødhuer stiger poå, dvs de trykker på cab order i feil retning, blir det nedprioritert ifht assigned orders og om det er noen ordre av typen unasssigend over seg.
    
    % get_idle_elevator(Elevator_states) ->
    %     Is_idle = fun(_Key, Dictionary_value) -> Dictionary_value#state.movement == idle end,
    %     dict:filter(Is_idle, Elevator_states).