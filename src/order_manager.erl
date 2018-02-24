-module(order_manager).
-export([node_communication/0]).

% Det er noe vi ikke har skjønt med [ | ] operasjonen
    % A ++ B returnerer feks [1, 2, 3, 4] der A = [1, 2, 3] og B = [4]
    % [A | B] returnerer da [[1, 2, 3], 4]....

%Skal vi sender Order som 5 eller [5]??

% TODO: spawn from gloabl spawner, remember only coments for LED now

node_communication() ->
    register(order_manager, self()),
    node_communication([]).

node_communication(LocalOrderList) ->
    io:format("LocalOrderList: ~p\n", [LocalOrderList]),

    receive
        {new_order, Order} ->
            io:format("Received: new_order\n"),
            % AlreadyExists = lists:member(Order, LocalOrderList),
            % if AlreadyExists -> node_communication(LocalOrderList) end,
            % lists:foreach(fun(Node) -> {order_manager, Node} ! {add_order, Order, LocalOrderList, node()} end, nodes()),
            % node_communication(LocalOrderList ++ Order);    % For debuging
            % %node_communication(LocalOrderList);

            case lists:member(hd(Order), LocalOrderList) of
                true -> node_communication(LocalOrderList);
                false -> 
                    lists:foreach(fun(Node) -> {order_manager, Node} ! {add_order, Order, LocalOrderList, node()} end, nodes()),
                    %node_communication(LocalOrderList ++ Order)    % For debuging
                    node_communication(LocalOrderList)
            end;


        {add_order, Order, ExternalOrderList, ExternalElevator} ->
            io:format("Received: add_order\n"),
            {order_manager, ExternalElevator} ! {ack_order, Order, LocalOrderList, node()},
            MissingOrders = ExternalOrderList -- LocalOrderList,
            node_communication(LocalOrderList ++ MissingOrders ++ Order);

        {ack_order, Order, ExternalOrderList, ExternalElevator} ->
            io:format("Received: ack_order\n"),
            {order_manager, ExternalElevator} ! {led_on, Order},
            MissingOrders = ExternalOrderList -- LocalOrderList,
            node_communication(LocalOrderList ++ MissingOrders ++ Order);
        
        {order_finished, Order} ->
            io:format("Received: order_finished\n"),
            lists:foreach(fun(Node) -> {order_manager, Node} ! {remove_order, Order, LocalOrderList} end, [node()|nodes()]),
            node_communication(LocalOrderList);

        {remove_order, Order, ExternalOrderList} ->
            io:format("Received: remove_order\n"),
            % CHANGE TO {elevator_controller, Node} ! {led_off, Order},
            io:format("LEDs turned OFF for order ~p\n", [Order]),
            MissingOrders = ExternalOrderList -- LocalOrderList,
            node_communication([X || X <- LocalOrderList ++ MissingOrders, X /= Order]);  % removes all instances of Order
        
        {led_on, Order} ->
            io:format("Received: led_on\n"),
            % CHANGE TO{elevator_controller, Node} ! {led_on, Order},
            io:format("LEDs turned ON for order ~p\n", [Order]),
            node_communication(LocalOrderList);

        {get_orderList, PID} ->                         %Skal det være stor L i get_orderList?
            io:format("Received: get_orderList\n"),
            PID ! LocalOrderList,
            node_communication(LocalOrderList)
    end.
