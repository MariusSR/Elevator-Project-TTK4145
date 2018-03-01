%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This module handles all communication between seperate nodes, i.e. every message  %%
%% from node A to node B is sent from this module on node A and received in the very %%
%% same module on node B. It is then locally routed to the correct module.           %%
%%    The module consists in essence of only one funciton which operates as a main   %%
%% loop taking a list as argument. The list contains all locally known orders. The   %%
%% list is reguallarly shared (and taken the union of) between different nodes to    %%
%% avoid missing orders.                                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(order_manager).
-export([node_communication/0]).

% TODO: spawn from gloabl spawner, remember there is only coments for LED now
% TODO: remove unnecessary comments

node_communication() ->
    node_communication([]).

node_communication(LocalOrderList) ->
    io:format("LocalOrderList: ~p\n", [LocalOrderList]),

    receive
        {new_order, Order} when is_tuple(Order) ->
            io:format("Received: new_order\n"),
            case lists:member(Order, LocalOrderList) of
                true  -> node_communication(LocalOrderList);
                false -> 
                    lists:foreach(fun(Node) -> {order_manager, Node} ! {add_order, Order, LocalOrderList, node()} end, nodes()),
                    node_communication(LocalOrderList)     
            end;

        {add_order, Order, ExternalOrderList, ExternalElevator}
        when is_tuple(Order) andalso is_list(ExternalOrderList) andalso is_pid(ExternalElevator) ->
            io:format("Received: add_order\n"),
            {order_manager, ExternalElevator} ! {ack_order, Order, LocalOrderList, node()},
            MissingOrders = ExternalOrderList -- LocalOrderList,
            node_communication(LocalOrderList ++ MissingOrders ++ [Order]);

        {ack_order, Order, ExternalOrderList, ExternalElevator} 
        when is_tuple(Order) andalso is_list(ExternalOrderList) andalso is_pid(ExternalElevator) ->
            io:format("Received: ack_order\n"),
            {order_manager, ExternalElevator} ! {led_on, Order},
            {Button_type, Floor} = Order,
            driver ! {set_order_button_LED, Button_type, Floor, 1},
            io:format("LED turned ON for order ~p\n", [Order]),
            MissingOrders = ExternalOrderList -- LocalOrderList,
            node_communication(LocalOrderList ++ MissingOrders ++ [Order]);
        
        {order_finished, Order} when is_tuple(Order) ->
            io:format("Received: order_finished\n"),
            lists:foreach(fun(Node) -> {order_manager, Node} ! {remove_order, Order, LocalOrderList} end, [node()|nodes()]),
            node_communication(LocalOrderList);

        {remove_order, Order, ExternalOrderList} when is_tuple(Order) andalso is_list(ExternalOrderList) ->
            io:format("Received: remove_order\n"),
            {Button_type, Floor} = Order,
            driver ! {set_order_button_LED, Button_type, Floor, 0},
            io:format("LED turned OFF for order ~p\n", [Order]),
            MissingOrders = ExternalOrderList -- LocalOrderList,
            %%%%%% TODO: REMEMBER TO REMOVE ALL ORDERS AT THAT FLOOR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            node_communication([X || X <- LocalOrderList ++ MissingOrders, X /= Order]);  % removes all instances of Order
        
        {led_on, Order} when is_tuple(Order) ->
            io:format("Received: led_on\n"),
            {Button_type, Floor} = Order,
            driver ! {set_order_button_LED, Button_type, Floor, 1},
            io:format("LEDs turned ON for order ~p\n", [Order]),
            node_communication(LocalOrderList);

        {get_orderlist, PID} when is_pid(PID) ->
            io:format("Received: get_orderList\n"),
            PID ! LocalOrderList,
            node_communication(LocalOrderList);

        % test "functions"
        {clear_queue} ->
            lists:foreach(fun(Order) -> order_manager ! {order_finished, Order}, LocalOrderList end) 
        
    end.