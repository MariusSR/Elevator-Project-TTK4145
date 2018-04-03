%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This module handles all communication between seperate nodes, i.e. every message  %%
%% from node A to node B is sent from this module on node A and received in the very %%
%% same module on node B. It is then locally routed to the correct module.           %%
%%    The module consists in essence of only one funciton which operates as a main   %%
%% loop taking a list as argument. The list contains all locally known orders. The   %%
%% list is reguallarly shared (and taken the union of) between different nodes to    %%
%% avoid missing orders.                                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(node_communicator).
-export([start/0]).
-include("parameters.hrl").
-record(state,  {movement, floor}).

% TODO: spawn from gloabl spawner, remember there is only coments for LED now
% TODO: remove unnecessary comments
% endre rekkefølgen på receivene, samle alle med order feks

start() ->
    io:format("Main loop of node_communicator started\n"),
    main_loop().

main_loop() ->
    receive
        {new_order, {cab_button, Floor}} ->
            node_communicator ! {add_order, {cab_button, Floor}, node()};

        {new_order, Order} when is_tuple(Order) ->
            lists:foreach(fun(Node) -> {node_communicator, Node} ! {add_order, Order, node()} end, nodes());

        {add_order, Order, From_node} when is_tuple(Order) andalso is_atom(From_node) ->
            order_manager ! {add_order, Order, From_node};

        {order_added, Order, From_node} when is_tuple(Order) andalso is_atom(From_node)  ->
            {node_communicator, From_node} ! {ack_order, Order};
        
        {ack_order, {cab_button, Floor}} ->
            order_manager ! {add_order, {cab_button, Floor}, node()},
            node_communicator ! {set_order_button_LED, on, {cab_button, Floor}};

        {ack_order, Order} when is_tuple(Order) ->
            order_manager ! {add_order, Order, node()},
            lists:foreach(fun(Node) -> {node_communicator, Node} ! {set_order_button_LED, on, Order} end, [node()|nodes()]);
        
        {new_order_assigned, Order} when is_tuple(Order) ->
            lists:foreach(fun(Node) -> {node_communicator, Node} ! {mark_order_assigned, Order, node()} end, [node()|nodes()]);

        {mark_order_assigned, Order, Node} when is_tuple(Order) ->
            order_manager ! {mark_order_assigned, Order, Node};
        
        {set_order_button_LED, on, {Button_type, Floor}} when is_atom(Button_type) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            driver ! {set_order_button_LED, Button_type, Floor, on};

        {set_order_button_LED, off, {Button_type, Floor}} when is_atom(Button_type) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            driver ! {set_order_button_LED, Button_type, Floor, off};
        
        {order_finished, Order} when is_tuple(Order) ->
            % FORVENTER NÅ Å MOTTA ALLE ORDRE SOM ER FULLFØRT I TUR OG ORDEN, DVS FSM MÅ BÅDE SENDE CAB OG HALL SOM FULLFØRT ETTER HVERANDRE
            lists:foreach(fun(Node) -> {node_communicator, Node} ! {clear_order, Order} end, [node()|nodes()]);

        {clear_order, {Button_type, Floor}} when is_atom(Button_type) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            order_manager ! {remove_order, {Button_type, Floor}},
            driver ! {set_order_button_LED, Button_type, Floor, off};
        
        {reached_new_state, State} when is_record(State, state) ->
            lists:foreach(fun(Node) -> {node_communicator, Node} ! {update_state, node(), State} end, [node()|nodes()]);

        {update_state, Node, New_state} when is_atom(Node) andalso is_record(New_state, state) ->
            order_manager ! {update_state, Node, New_state};

        {sync_hall_orders_with_new_node, New_node, Assigned_hall_orders, Unassigned_hall_orders} ->
            {node_communicator, New_node} ! {existing_hall_orders, Assigned_hall_orders, Unassigned_hall_orders};
        
        {existing_hall_orders, Assigned_hall_orders, Unassigned_hall_orders} ->
            order_manager ! {existing_hall_orders, Assigned_hall_orders, Unassigned_hall_orders},
            driver ! turn_off_all_leds,
            Existing_orders = lists:map(fun({Assigned_order, _Node}) -> Assigned_order end, Assigned_hall_orders) ++ Unassigned_hall_orders,
            foreach(fun({Button_type, Floor}) -> driver ! {set_order_button_LED, Button_type, Floor, on} end, Existing_orders);

        %%% FOR DEBUG ONLY %%%
        reset ->
            lists:foreach(fun(Node) -> {node_communicator, Node} ! reset_queue_and_button_leds end, [node()|nodes()]);

        reset_queue_and_button_leds ->
            driver ! turn_off_all_leds,
            order_manager ! reset_queue;

        Unexpected ->
            io:format("Unexpected message in node_communicator: ~p~n", [Unexpected])
    end,

    main_loop().