-module(cost_function).
-export([get_most_efficient_order/2]).

-include("parameters.hrl").
-record(state,  {movement, floor, assigned_order}).

%-------------------------------------------------------------------------------------------------
% Finds an order that is best served by the requesting node. Returns the order if found, else
% returns 'no_orders_available'. This is done by recursively iterating over all unassgned orders.
%-------------------------------------------------------------------------------------------------
get_most_efficient_order([], _States) ->
    no_orders_available;

get_most_efficient_order([Order|Remaining_orders_to_evaluate], States) ->
    case node() == get_optmial_elevator_for_order([node()|nodes()], Order, States, {node(), -1}) of
        true  -> Order;
        false -> get_most_efficient_order(Remaining_orders_to_evaluate, States)
    end.



%-------------------------------------------------------------------------------------------------
% For each node in the node cluster; gets its figure of suitability (FS). Returns the elevator
% whos FS value is the gratest. This is done by recurcively iterating over all elevators.
%-------------------------------------------------------------------------------------------------
get_optmial_elevator_for_order([], _Order, _States, {Best_node, _FS}) ->
    Best_node;  % All elevators evaluated, returns the best node to serve 'Order'

get_optmial_elevator_for_order([Node|Remaining_nodes_to_evaluate], Order, States, Best) ->
    case dict:find(Node, States) of
        {ok, State} ->
            FS_for_this_node = calculate_FS(Order, State),
            case FS_for_this_node > element(2, Best) of
                true  -> get_optmial_elevator_for_order(Remaining_nodes_to_evaluate, Order, States, {Node, FS_for_this_node});
                false -> get_optmial_elevator_for_order(Remaining_nodes_to_evaluate, Order, States, Best)
            end;
        
        error ->
            io:format("~s States for ~p not present in States.\n", [color:red("Cost_function:"), Node]),
            io:format("    -> Node, Remaining_nodes, Order, Elevator_stat, Best:  ~p,   ~p,   ~p,   ~p,   ~p\n", [Node, Remaining_nodes_to_evaluate, Order, States, Best]), %REMOVE THIS BEFORE DELIVERY
            get_optmial_elevator_for_order(Remaining_nodes_to_evaluate, Order, States, Best)
    end.



%-------------------------------------------------------------------------------------------------
% Caclulates the FS value (a meassure of gain) for an elevator. If the elevator is
%   - uninitialized:                                          FS = 1 - 2 * number of floors (worst)
%   - idle at the same floor as the order:                    FS = number of floors + 2     (best)
%   - about to become idle at the same floor as the order:    FS = number of floors + 1
%   - else moving towards an order in the same direction:     FS = number of floors - distance + 1
%   - else moving towards an order in the opposite direction: FS = number of floors - distance
%   - else moving away from the order:                        FS = 1 - distance
%-------------------------------------------------------------------------------------------------
calculate_FS({Button_type, Floor}, State) ->
    case State#state.movement == uninitialized of
        true  ->
            _FS = 1 - 2 * ?NUMBER_OF_FLOORS;

        false ->
            case abs(Floor - State#state.floor) of      % Calculates the distance between an order and an eevator
                0 when State#state.movement == idle ->  % Idle elevator at the same floor as the order
                    _FS = ?NUMBER_OF_FLOORS + 2;
                
                _Distance when (element(2, State#state.assigned_order) == Floor) and
                ((Floor == 1) or (Floor == ?NUMBER_OF_FLOORS) or (element(1, State#state.assigned_order) == cab_button)) ->
                    _FS = ?NUMBER_OF_FLOORS + 1;        % About to become idle at the same floor

                Distance ->
                    case is_elevator_moving_towards_order(Floor, State) of
                        true ->
                            case is_order_in_same_direction_as_elevator_is_moving(Floor, Button_type, State) of
                                true  -> _FS = ?NUMBER_OF_FLOORS - Distance + 1;
                                false -> _FS = ?NUMBER_OF_FLOORS - Distance
                            end;

                        false ->
                            _FS = 1 - Distance
                    end
            end
    end.



%-------------------------------------------------------------------------------------------------
% Returns true if the elevator is moving towards the order or is idle, else false.
%-------------------------------------------------------------------------------------------------
is_elevator_moving_towards_order(Order_floor, State) ->
    (State#state.movement == up_dir)   and (Order_floor > State#state.floor) or
    (State#state.movement == down_dir) and (Order_floor < State#state.floor) or
    (State#state.movement == idle).



%-------------------------------------------------------------------------------------------------
% Returns true if the order and elevator is in the same direction.
%-------------------------------------------------------------------------------------------------
is_order_in_same_direction_as_elevator_is_moving(_Floor, _Buton_type, State) when State#state.movement == idle -> true;
is_order_in_same_direction_as_elevator_is_moving(1, up_button, _State)                                         -> true;
is_order_in_same_direction_as_elevator_is_moving(?NUMBER_OF_FLOORS, down_button, _State)                       -> true;

is_order_in_same_direction_as_elevator_is_moving(Floor, up_button, State) ->
    (State#state.movement == up_dir) or ((Floor == 1) and (State#state.movement == down_dir));
    
is_order_in_same_direction_as_elevator_is_moving(Floor, down_button, State) ->
    (State#state.movement == down_dir) or ((Floor == ?NUMBER_OF_FLOORS) and (State#state.movement == up_dir)).