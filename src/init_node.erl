-module(init_node).
-export([start/0, start_local/1]).

start() ->
    register(driver, spawn(fun() -> driver_interface:start() end)),
    io:format("Driver PID: ~p\n", [whereis(driver)]),
    timer:sleep(100), % wait for driver to finish its initialization

    register(node_communicator, spawn(fun()-> node_communicator:start() end)),
    io:format("Node_communicator PID: ~p\n", [whereis(node_communicator)]),

    register(order_manager, spawn(fun() -> order_manager:start() end)),
    io:format("Ordermanager PID: ~p\n", [whereis(order_manager)]),

    register(watchdog, spawn(fun() -> watchdog:start() end)),
    io:format("Watchdog PID: ~p\n", [whereis(watchdog)]),

    register(node_connection, spawn(fun() -> node_connection:start() end)),
    io:format("Node_connection PID: ~p\n", [whereis(node_connection)]),
    timer:sleep(100), % wait for node cluster to be started on this node

    register(fsm, spawn(fun()-> fsm:start() end)),
    io:format("FSM PID: ~p\n", [whereis(fsm)]),

    button_reader:start(),

    io:format("\n\n").


start_local(Port) ->     
    register(driver, spawn(fun()-> driver_interface:start(Port) end)),
    io:format("~s~p\n", [color:cyan("Driver PID:              "), whereis(driver)]),

    register(node_communicator, spawn(fun()-> node_communicator:start() end)),
    io:format("~s~p\n", [color:cyan("Node_communicator PID:   "), whereis(node_communicator)]),

    register(order_manager, spawn(fun()-> order_manager:start() end)),
    io:format("~s~p\n", [color:cyan("Ordermanager PID:        "), whereis(order_manager)]),

    register(watchdog, spawn(fun() -> watchdog:start() end)),
    io:format("~s~p\n", [color:cyan("Watchdog PID:            "), whereis(watchdog)]),

    register(fsm, spawn(fun()-> fsm:start() end)),
    io:format("~s~p\n", [color:cyan("FSM PID:                 "), whereis(fsm)]),

    button_reader:start(),
    
    io:format("\n").