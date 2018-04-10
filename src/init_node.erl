-module(init_node).
-export([start/0]).

start() ->
    timer:sleep(100),
    register(driver, spawn(fun() -> driver_interface:start() end)),
    io:format("\n~s~p\n", [color:cyan("Driver PID:              "), whereis(driver)]),
    timer:sleep(100), % Wait for driver to finish its initialization

    register(fsm, spawn(fun()-> fsm:start() end)),
    io:format("~s~p\n", [color:cyan("FSM PID:                 "), whereis(fsm)]),

    register(node_communicator, spawn(fun()-> node_communicator:start() end)),
    io:format("~s~p\n", [color:cyan("Node_communicator PID:   "), whereis(node_communicator)]),

    register(order_manager, spawn(fun() -> order_manager:start() end)),
    io:format("~s~p\n", [color:cyan("Ordermanager PID:        "), whereis(order_manager)]),

    register(watchdog, spawn(fun() -> watchdog:start() end)),
    io:format("~s~p\n", [color:cyan("Watchdog PID:            "), whereis(watchdog)]),

    register(node_connection, spawn(fun() -> node_connection:start() end)),
    io:format("~s~p\n", [color:cyan("Node_connection PID:     "), whereis(node_connection)]),
    timer:sleep(100), % Wait for node cluster to be started on this node

    button_reader:start(),

    io:format("\n\n").