%%=================================================================================================
%% This module is responsable for communication accross different nodes. Other modules interface
%% this module by messages received in 'main_loop'. Data received is routed to 'communicator'
%% on one or multiple other nodes, or locally to either 'data_manager' or 'driver'.
%%=================================================================================================

-module(communication_interface).
-export([start/0]).

start() ->
    link(whereis(driver)), 
    main_loop().



main_loop() ->
    receive

        %--------------------------------------------------------------------------------------------------
        % A new order registered on a node is first sent sent to all other nodes. They add the order to
        % their order list and sends an acknowledge message. Upon receive of the acknowledge message, the
        % order is added locally and a message to turn on corresponding LED is sent to all nodes.
        %--------------------------------------------------------------------------------------------------
        {new_order, {cab_button, Floor}} ->
            data_manager ! {add_order, {cab_button, Floor}};

        {new_order, Hall_order} ->
            lists:foreach(fun(Node) -> {communicator, Node} ! {add_order, Hall_order, node()} end, nodes());

        {add_order, Order, From_node} ->
            data_manager ! {add_order, Order, From_node};

        {order_added, Order, From_node} ->
            {communicator, From_node} ! {ack_order, Order};
        
        {ack_order, {cab_button, Floor}} ->
            data_manager ! {add_order, {cab_button, Floor}, node()},
            communicator ! {set_order_button_LED, on, {cab_button, Floor}};

        {ack_order, Hall_order} ->
            data_manager ! {add_order, Hall_order, node()},
            lists:foreach(fun(Node) -> {communicator, Node} ! {set_order_button_LED, on, Hall_order} end, [node()|nodes()]);
        


        %--------------------------------------------------------------------------------------------------
        % When an order is assigned to an elevator, all nodes are notified and their order list updated.
        %--------------------------------------------------------------------------------------------------
        {new_order_assigned, Order} ->
            lists:foreach(fun(Node) -> {communicator, Node} ! {mark_order_assigned, Order, node()} end, [node()|nodes()]);

        {mark_order_assigned, Order, Node} ->
            data_manager ! {mark_order_assigned, Order, Node};

        

        %--------------------------------------------------------------------------------------------------
        % When a node serves an order, this module is notified by 'fsm' and sends a 'clear_order' message
        % to all nodes. Each node locally and independently then updates their order list.
        %--------------------------------------------------------------------------------------------------
        {order_served, {cab_button, Floor}} ->
            communicator ! {clear_order, {cab_button, Floor}};
        
        {order_served, Hall_order} ->
            lists:foreach(fun(Node) -> {communicator, Node} ! {clear_order, Hall_order} end, [node()|nodes()]);

        {clear_order, Order} ->
            data_manager ! {remove_order, Order},
            driver       ! {set_order_button_LED, off, Order};


        
        %--------------------------------------------------------------------------------------------------
        % Calls for LEDs to be turned on/off is sent to 'driver' to be handled independently on each node.
        %--------------------------------------------------------------------------------------------------
        {set_order_button_LED, on, Order} ->
            driver ! {set_order_button_LED, on, Order};

        {set_order_button_LED, off, Order} ->
            driver ! {set_order_button_LED, off, Order};



        %--------------------------------------------------------------------------------------------------
        % When a node changes state, this module is notified by 'fsm' and sends an 'update_state' message
        % to all nodes. Each node locally and independently then updates their dictionary of states.
        %--------------------------------------------------------------------------------------------------      
        {reached_new_state, State} ->
            lists:foreach(fun(Node) -> {communicator, Node} ! {update_state, node(), State} end, [node()|nodes()]);

        {update_state, Node, New_state} ->
            data_manager ! {update_state, Node, New_state};



        %--------------------------------------------------------------------------------------------------
        % When a new node is connected to the node cluster, all other nodes send a copy of their orders
        % and states to the new node. LEDs on the new node is then set accordingly.
        %--------------------------------------------------------------------------------------------------
        {sync_data_with_new_node, New_node, Assigned_hall_orders, Unassigned_hall_orders, Elevator_states} ->
            {communicator, New_node} ! {existing_data, Assigned_hall_orders, Unassigned_hall_orders, Elevator_states};
        
        {existing_data, Assigned_hall_orders, Unassigned_hall_orders, Elevator_states} ->
            driver       ! turn_off_all_leds,
            data_manager ! {existing_data, Assigned_hall_orders, Unassigned_hall_orders, Elevator_states};



        Unexpected ->
            io:format("~s Unexpected message in communicator: ~p\n", [color:red("Communication_interface:"), Unexpected])

    end,

    main_loop().