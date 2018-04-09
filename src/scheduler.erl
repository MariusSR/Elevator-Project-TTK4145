-module(scheduler).
-export([get_most_efficient_order/2]).

-include("parameters.hrl").
-record(state,  {movement, floor}).

%-------------------------------------------------------------------------------------------------
% Finds an order that is best served by the requesting node. Returns the order if found, else
% returns 'no_orders_available'. This is done by recursively iterating over all unassgned orders.
%-------------------------------------------------------------------------------------------------
get_most_efficient_order([], _Elevator_states) ->
    no_orders_available;

get_most_efficient_order([Order|Remaining_orders_to_evaluate], Elevator_states) ->
    case node() == get_optmial_elevator_for_order([node()|nodes()], Order, Elevator_states, {node(), -1}) of
        true  -> Order;
        false -> get_most_efficient_order(Remaining_orders_to_evaluate, Elevator_states)
    end.



%-------------------------------------------------------------------------------------------------
% For each node in the node cluster; gets its figure of suitability (FS). Returns the elevator
% whos FS value is the gratest. This is done by recurcively iterating over all elevators.
%-------------------------------------------------------------------------------------------------
get_optmial_elevator_for_order([], _Order, _Elevator_states, {Best_node, _FS}) ->
    Best_node;   % All elevators evaluated, returns the best node to serve 'Order'

get_optmial_elevator_for_order([Node|Remaining_nodes_to_evaluate], Order, Elevator_states, Best) ->
    case dict:find(Node, Elevator_states) of
        {ok, State_of_elevator} ->
            FS_for_this_node = calculate_FS(Order, State_of_elevator),
            case FS_for_this_node > element(2, Best) of
                true  -> get_optmial_elevator_for_order(Remaining_nodes_to_evaluate, Order, Elevator_states, {Node, FS_for_this_node});
                false -> get_optmial_elevator_for_order(Remaining_nodes_to_evaluate, Order, Elevator_states, Best)
            end;
        
        error ->
            io:format("Error in scheduler: states for ~p not present in Elevator_states", [Node]) % gjør denne printen rød!
    end.



%-------------------------------------------------------------------------------------------------
% Caclulates the FS value for an elevator. If the elevator is
%   - idle at the same floor as the order:                    FS = Number of floors + 2
%   - moving towards an order in the same direction:          FS = Number of floors - distance + 1
%   - moving towards an order in the opposite direction:      FS = Number of floors - Distance
%   - moving away from the order:                             FS = 1
%-------------------------------------------------------------------------------------------------
calculate_FS({Button_type, Floor}, State_of_elevator) -> 
    io:format("~s,   ~p,   ~p,   ~p\n", [color:magenta("ABCD Button_type, floor, state_of_elevator"), Button_type, Floor, State_of_elevator]),
    Distance = abs(Floor - State_of_elevator#state.floor),
    case (Floor == State_of_elevator#state.floor) and (State_of_elevator#state.movement == stop_dir) of
        true ->
            _FS = ?NUMBER_OF_FLOORS + 2;
            
        false ->
            case is_elevator_moving_towards_order(Floor, State_of_elevator) of
                true ->
                    case is_order_in_same_direction_as_elevator_is_moving(Floor, Button_type, State_of_elevator) of
                        true  -> _FS = ?NUMBER_OF_FLOORS - Distance + 1;
                        false -> _FS = ?NUMBER_OF_FLOORS - Distance
                    end;

                false ->
                    _FS = 1 - Distance
            end
    end.



%-------------------------------------------------------------------------------------------------
% Returns true if the elevator is moving towards the order or is idle, else false.
%-------------------------------------------------------------------------------------------------
is_elevator_moving_towards_order(Order_floor, State_of_elevator) ->
    (State_of_elevator#state.movement == up_dir)   and (Order_floor > State_of_elevator#state.floor) or
    (State_of_elevator#state.movement == down_dir) and (Order_floor < State_of_elevator#state.floor) or
    (State_of_elevator#state.movement == stop_dir).



%-------------------------------------------------------------------------------------------------
% Returns true if the order and elevator is in the same direction.
%-------------------------------------------------------------------------------------------------
is_order_in_same_direction_as_elevator_is_moving(1, up_button, _State_of_elevator) ->
    true;

is_order_in_same_direction_as_elevator_is_moving(?NUMBER_OF_FLOORS, down_button, _State_of_elevator) ->
    true;

is_order_in_same_direction_as_elevator_is_moving(_Floor, up_button, State_of_elevator) ->
    (State_of_elevator#state.movement == up_dir);

is_order_in_same_direction_as_elevator_is_moving(_Floor, down_button, State_of_elevator) ->
    (State_of_elevator#state.movement == down_dir).