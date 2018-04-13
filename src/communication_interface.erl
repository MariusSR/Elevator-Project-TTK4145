%%=================================================================================================
%% This module is responsable for communication accross different nodes. Other modules interface
%% this module by messages received in 'main_loop'. Data received is routed to 'communicator'
%% on one or multiple other nodes, or locally to either 'data_manager' or 'driver'.
%%=================================================================================================

-module(communication_interface).
-export([start/0]).

-include("parameters.hrl").
%-record(state,  {movement, floor}).

% TODO: spawn from gloabl spawner, remember there is only coments for LED now
% TODO: remove unnecessary comments
% endre rekkefølgen på receivene, samle alle med order feks
% Sjekk at detvar trygt å kommentere ut staten over


start() ->
    main_loop().

    

main_loop() ->
    receive

        %--------------------------------------------------------------------------------------------------
        % A new order registered on a node is first sent sent to all nodes. They add the order to their
        % order list and sends an acknowledge message. Upon receive of the acknowledge message, the order
        % is added locally and a 'set_order_button_LED' 'on' message is sent to all nodes.
        %--------------------------------------------------------------------------------------------------
        {new_order, {cab_button, Floor}} ->
            data_manager ! {add_order, {cab_button, Floor}};

        {new_order, Order} ->
            lists:foreach(fun(Node) -> {communicator, Node} ! {add_order, Order, node()} end, nodes());

        {add_order, Order, From_node} ->
            data_manager ! {add_order, Order, From_node};

        {order_added, Order, From_node} ->
            {communicator, From_node} ! {ack_order, Order};
        
        {ack_order, {cab_button, Floor}} ->
            data_manager ! {add_order, {cab_button, Floor}, node()},
            communicator ! {set_order_button_LED, on, {cab_button, Floor}};

        {ack_order, Order} ->
            data_manager ! {add_order, Order, node()},
            lists:foreach(fun(Node) -> {communicator, Node} ! {set_order_button_LED, on, Order} end, [node()|nodes()]);
        


        %--------------------------------------------------------------------------------------------------
        % When an order is assigned to an elevator, all nodes are notified and their order list updated.
        %--------------------------------------------------------------------------------------------------
        {new_order_assigned, Order} ->
            lists:foreach(fun(Node) -> {communicator, Node} ! {mark_order_assigned, Order, node()} end, [node()|nodes()]);

        {mark_order_assigned, Order, Node} ->
            data_manager ! {mark_order_assigned, Order, Node};

        

        %--------------------------------------------------------------------------------------------------
        % When a node serves an order, this module is notified by 'fsm' and send a 'clear_order' message to
        % all nodes. Each node locally and independently then updates their order list.
        %--------------------------------------------------------------------------------------------------
        {order_finished, {cab_button, Floor}} ->
            communicator ! {clear_order, {cab_button, Floor}};
        
        {order_finished, {Hall_button, Floor}} ->
            lists:foreach(fun(Node) -> {communicator, Node} ! {clear_order, {Hall_button, Floor}} end, [node()|nodes()]);

        {clear_order, {Button_type, Floor}} ->
            data_manager ! {remove_order, {Button_type, Floor}},
            driver       ! {set_order_button_LED, Button_type, Floor, off};


        
        %--------------------------------------------------------------------------------------------------
        % Calls for LEDs to be turned on/off is sent to 'driver' to be handled independently on each node.
        %--------------------------------------------------------------------------------------------------
        {set_order_button_LED, on, {Button_type, Floor}} ->
            driver ! {set_order_button_LED, Button_type, Floor, on};

        {set_order_button_LED, off, {Button_type, Floor}} ->
            driver ! {set_order_button_LED, Button_type, Floor, off};



        %--------------------------------------------------------------------------------------------------
        % When a node changes state, this module is notified by 'fsm' and send a 'update_state' message to
        % all nodes. Each node locally and independently then updates their list of states.
        %--------------------------------------------------------------------------------------------------      
        {reached_new_state, State} ->
            lists:foreach(fun(Node) -> {communicator, Node} ! {update_state, node(), State} end, [node()|nodes()]);

        {update_state, Node, New_state} ->
            data_manager ! {update_state, Node, New_state};



        %--------------------------------------------------------------------------------------------------
        % When a new node is connected to the node cluster, all other nodes sends a copy of their orders
        % and states to the new node. LEDs on the new node is then reset and set accordingly.
        %--------------------------------------------------------------------------------------------------
        {sync_hall_orders_and_states, New_node, Assigned_hall_orders, Unassigned_hall_orders, Elevator_states} ->
            {communicator, New_node} ! {existing_hall_orders_and_states, Assigned_hall_orders, Unassigned_hall_orders, Elevator_states};
        
        {existing_hall_orders_and_states, Assigned_hall_orders, Unassigned_hall_orders, Elevator_states} ->
            driver       ! turn_off_all_leds,
            data_manager ! {existing_hall_orders_and_states, Assigned_hall_orders, Unassigned_hall_orders, Elevator_states},
            timer:sleep(5),  % Sleep to ensure proper initialization of 'driver' and 'data_manager'
            Existing_orders = lists:map(fun({Assigned_order, _Node}) -> Assigned_order end, Assigned_hall_orders) ++ Unassigned_hall_orders,
            lists:foreach(fun({Button_type, Floor}) -> driver ! {set_order_button_LED, Button_type, Floor, on} end, Existing_orders);



        Unexpected ->
            io:format("Unexpected message in communicator: ~p~n", [Unexpected])

    end,

    main_loop().