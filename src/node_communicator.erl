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
-export([node_communicator/0]).
-include("parameters.hrl").
-record(state,  {movement, floor}).

% TODO: spawn from gloabl spawner, remember there is only coments for LED now
% TODO: remove unnecessary comments

node_communicator() ->
    driver ! turn_off_all_leds, % should perhaps not be done here?
    main_loop().

main_loop() ->
    io:format("Main loop of node_communicator\n"),

    receive
        {new_order, Order} when is_tuple(Order) ->
            io:format("Received: new_order~p\n", [Order]),
            lists:foreach(fun(Node) -> {node_communicator, Node} ! {add_order, Order, node()} end, nodes()),
            main_loop();

        {add_order, Order, From_node} when is_tuple(Order) andalso is_atom(From_node) ->
            io:format("Received: add_order~p\n", [Order]),
            order_manager ! {add_order, Order, From_node},
            main_loop();

        {order_added, Order, From_node} when is_tuple(Order) andalso is_atom(From_node)  ->
            io:format("Received: order_added\n"),
            {node_communicator, From_node} ! {ack_order, Order},
            main_loop();
        
        {ack_order, Order} when is_tuple(Order) ->
            io:format("Received: ack_order\n"),
            order_manager ! {add_order, Order, node()},
            lists:foreach(fun(Node) -> {node_communicator, Node} ! {set_order_button_LED, on, Order, node()} end, [nodes()|nodes()]),
            main_loop();
        
        {new_order_assigned, Order} when is_tuple(Order) ->
            io:format("Received: new_order_assigned\n"),
            lists:foreach(fun(Node) -> {node_communicator, Node} ! {mark_order_assigned, Order, node()} end, [node()|nodes()]),
            main_loop();

        {mark_order_assigned, Order} when is_tuple(Order) ->
            io:format("Received: mark_order_assigned\n"),
            order_manager ! {mark_order_assigned, Order},
            main_loop();
        
        {led_on, {Button_type, Floor}} when is_atom(Button_type) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            io:format("Received: order_on\n"),
            driver ! {set_order_button_LED, Button_type, Floor, on},
            main_loop();
        {led_off, {Button_type, Floor}} when is_atom(Button_type) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            io:format("Received: order_off\n"),
            driver ! {set_order_button_LED, Button_type, Floor, off},
            main_loop();
        
        {order_finished, Order} when is_tuple(Order) ->
            % FORVENTER NÅ Å MOTTA ALLE ORDRE SOM ER FULLFØRT I TUR OG ORDEN, DVS FSM MÅ BÅDE SENDE CAB OG HALL SOM FULLFØRT ETTER HVERANDRE
            io:format("Received: order_finished\n"),
            lists:foreach(fun(Node) -> {node_communicator, Node} ! {clear_order, Order} end, [node()|nodes()]),
            main_loop();

        {clear_order, {Button_type, Floor}} when is_atom(Button_type) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
            io:format("Received: clear_order\n"),
            order_manager ! {remove_order, {Button_type, Floor}},
            driver ! {set_order_button_LED, Button_type, Floor, off},
            main_loop();
        
        {reached_new_state, State} when is_record(State, state) ->
            io:format("Received: new_state\n"),
            lists:foreach(fun(Node) -> {node_communicator, Node} ! {update_state, node(), State} end, [node()|nodes()]),
            main_loop();

        {update_state, Node, New_state} when is_atom(Node) andalso is_record(New_state, state) ->
            io:format("Received: update_state\n"),
            order_manager ! {update_state, Node, New_state},
            main_loop();

        %%% FOR DEBUG ONLY %%%
        reset ->
            lists:foreach(fun(Node) -> {node_communicator, Node} ! reset_queue_and_button_leds end, [node()|nodes()]),
            main_loop();
        reset_queue_and_button_leds ->
            driver ! turn_off_all_leds,
            order_manager ! reset_queue,
            main_loop();

        Unexpected ->
            io:format("Unexpected message in node_communicator: ~p~n", [Unexpected]),
            main_loop()
    end.