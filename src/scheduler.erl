-module(scheduler).
-export([get_most_efficient_order/2]).
-include("parameters.hrl").

-record(state,  {movement, floor}).

%-------------------------------------------------------------------------------------------------
% Searches for an order that is best served by this local node. Returns this order if found, else
% returns 'no_order_available'. This is done by recursively iterating over all unassgned orders.
%-------------------------------------------------------------------------------------------------
get_most_efficient_order([], _Elevator_states) ->
    no_orders_available;
get_most_efficient_order([Order|Remaining_orders_to_evaluate], Elevator_states) ->
    %This_node = node(),
    case node() == get_optmial_elevator_for_order([node()|nodes()], Order, Elevator_states, {node(), -1}) of
        true ->
            Order;
        false ->
            get_most_efficient_order(Remaining_orders_to_evaluate, Elevator_states)
    end.

%-------------------------------------------------------------------------------------------------
% For each node in the node cluster, gets its figure of suitability (FS). returns the elevator
% whos FS value is the gratest. This is done by recurcively iterating over all elevators.
%-------------------------------------------------------------------------------------------------
get_optmial_elevator_for_order([], _Order, _Elevator_states, {Node, _FS}) ->
    io:format("I WON: ~p\n", [Node]),
    Node;
get_optmial_elevator_for_order([Node|Remaining_nodes_to_evaluate], Order, Elevator_states, Best) ->
    case dict:find(Node, Elevator_states) of
        error ->
            ok;
        {ok, State_of_elevator} ->
            FS_for_this_node = calculate_FS(Order, State_of_elevator),
            case FS_for_this_node > element(2, Best) of
                true ->
                    get_optmial_elevator_for_order(Remaining_nodes_to_evaluate, Order, Elevator_states, {Node, FS_for_this_node});
                false ->
                    get_optmial_elevator_for_order(Remaining_nodes_to_evaluate, Order, Elevator_states, Best)
            end
    end.

%-------------------------------------------------------------------------------------------------
% Caclulates the FS value for an elevator. FS = 1 if elevator is moving away from the order. Else
% FS = number of floors + X - distance to order, where X = 1 if order is in the same direction as
% the elevator is moving and X = 0 if the order is in the oposite direcation as the elevator.
%-------------------------------------------------------------------------------------------------
calculate_FS({Button_type, Floor}, State_of_elevator) -> %when is_integer(Floor) andalso is_record(State_of_elevator, state) ->
    Distance = abs(Floor - State_of_elevator#state.floor),
    case is_elevator_moving_towards_order(Floor, State_of_elevator) of
        true ->
            case is_order_in_same_direction_as_elevator_is_moving(Button_type, State_of_elevator) of
                true ->
                    FS = ?NUMBER_OF_FLOORS + 1 - Distance,
                    io:format("        FSa: ~p ~p ~p\n", [FS, Button_type, Floor]),
                    FS;
                false ->
                    FS = ?NUMBER_OF_FLOORS - Distance,
                    io:format("        FSb: ~p ~p ~p\n", [FS, Button_type, Floor]),
                    FS
            end;
        false ->
            FS = 1,
            io:format("        FSc: ~p ~p ~p\n", [FS, Button_type, Floor]),
            FS
    end.

%-------------------------------------------------------------------------------------------------
% Boolean help function. Returns true if the elevator is moving towards the order or is idle.
%-------------------------------------------------------------------------------------------------
is_elevator_moving_towards_order(Order_floor, State_of_elevator) ->
    (State_of_elevator#state.movement == up_dir)   and (Order_floor >= State_of_elevator#state.floor) or
    (State_of_elevator#state.movement == down_dir) and (Order_floor =< State_of_elevator#state.floor) or
    (State_of_elevator#state.movement == stop_dir).

%-------------------------------------------------------------------------------------------------
% Boolean help function. Returns true if the order and elevator is in the same direction.
%-------------------------------------------------------------------------------------------------
is_order_in_same_direction_as_elevator_is_moving(up_button, State_of_elevator) ->
    (State_of_elevator#state.movement == up_dir) or (State_of_elevator#state.movement == stop_dir);
is_order_in_same_direction_as_elevator_is_moving(down_button, State_of_elevator) ->
    (State_of_elevator#state.movement == down_dir) or (State_of_elevator#state.movement == stop_dir).