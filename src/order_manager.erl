-module(order_manager).
-export([node_communication/0]).

% TODO: spawn from gloabl spawner, remember only coments for LED now

node_communication() ->
    register(order_manager, self()),
    node_communication([]).

node_communication(LocalOrderList) ->
    receive
        {new_order, Order} ->
            AlreadyExists = lists:members(Order, LocalOrderList),
            if AlreadyExists -> node_communication(LocalOrderList) end,
            lists:foreach(fun(Node) -> {order_manager, Node} ! {add_order, Order, LocalOrderList, node()} end, nodes()),
            node_communication(LocalOrderList);

        {add_order, Order, ExternalOrderList, ExternalElevator} ->
            {order_manager, ExternalElevator} ! {ack_order, Order, LocalOrderList, node()},
            MissingOrders = ExternalOrderList -- LocalOrderList,
            node_communication([LocalOrderList | MissingOrders] ++ Order);

        {ack_order, Order, ExternalOrderList, ExternalElevator} ->
            {order_manager, ExternalElevator} ! {led_on, Order},
            MissingOrders = ExternalOrderList -- LocalOrderList,
            node_communication([[LocalOrderList | MissingOrders] | Order]);
        
        {order_finished, Order} ->
            lists:foreach(fun(Node) -> {order_manager, Node} ! {remove_order, Order, LocalOrderList} end, [node()|nodes()]),
            node_communication(LocalOrderList);

        {remove_order, Order, ExternalOrderList} ->
            % CHANGE TO {elevator_controller, Node} ! {led_off, Order},
            io:format("LEDs turned OFF for order ~p\n", [Order]),
            MissingOrders = ExternalOrderList -- LocalOrderList,
            node_communication([X || X <- [LocalOrderList | MissingOrders], X /= Order]);  % removes all instances of the order
        
        {led_on, Order} ->
            % {elevator_controller, Node} ! {led_on, Order},
            io:format("LEDs turned ON for order ~p\n", [Order]),
            node_communication(LocalOrderList);

        {get_orderList, PID} ->
            PID ! LocalOrderList,
            node_communication(LocalOrderList)
    end.