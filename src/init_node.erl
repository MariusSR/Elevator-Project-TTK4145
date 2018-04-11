-module(init_node).
-export([start/0]).

start() ->
    timer:sleep(100),
    register(driver, spawn(fun() -> driver_interface:start() end)),
    io:format("\n~s~p\n", [color:cyan("Driver PID:              "), whereis(driver)]),
    timer:sleep(100), % Wait for driver to finish its initialization

    register(fsm, spawn(fun()-> fsm:start() end)),
    io:format("~s~p\n", [color:cyan("FSM PID:                 "), whereis(fsm)]),

    register(communicator, spawn(fun()-> communication_interface:start() end)),
    io:format("~s~p\n", [color:cyan("communicator PID:   "), whereis(communicator)]),

    register(data_manager, spawn(fun() -> orders_and_states:start() end)),
    io:format("~s~p\n", [color:cyan("Ordermanager PID:        "), whereis(data_manager)]),

    register(watchdog, spawn(fun() -> watchdog:start() end)),
    io:format("~s~p\n", [color:cyan("Watchdog PID:            "), whereis(watchdog)]),

    register(node_connection, spawn(fun() -> node_connection:start() end)),
    io:format("~s~p\n", [color:cyan("Node_connection PID:     "), whereis(node_connection)]),
    timer:sleep(100), % Wait for node cluster to be started on this node

    hardware_reader:start(),

    io:format("\n\n").