-module(init_node).
-export([start/0, start_local/1]).

start() ->
    register(driver, spawn(fun() -> driver_interface:start() end)),
    io:format("driver PID: ~p\n", [whereis(driver)]),
    timer:sleep(100), % wait for driver to finish its initialization

    register(node_communicator, spawn(fun()-> node_communicator:start() end)),
    io:format("Node_communicator PID: ~p\n", [whereis(node_communicator)]),

    register(order_manager, spawn(fun() -> order_manager:start() end)),
    io:format("ordermanager PID: ~p\n", [whereis(order_manager)]),

    register(watchdog, spawn(fun() -> watchdog:start() end)),
    io:format("watchdog PID: ~p\n", [whereis(watchdog)]),

    register(node_connection, spawn(fun() -> node_connection:start() end)),
    io:format("node_connection PID: ~p\n", [whereis(node_connection)]),

    register(fsm, spawn(fun()-> fsm:start() end)),
    io:format("FSM PID: ~p\n", [whereis(fsm)]),

    A = spawn(fun()-> button_reader:read_button_loop(1) end),
    io:format("button_reader PID: ~p\n", [A]),

    io:format("Start completed\n\n").


start_local(Port) ->     
    register(driver, spawn(fun()-> driver_interface:start(Port) end)),
    io:format("driver PID: ~p\n", [whereis(driver)]),

    register(node_communicator, spawn(fun()-> node_communicator:start() end)),
    io:format("Node_communicator PID: ~p\n", [whereis(node_communicator)]),

    register(order_manager, spawn(fun()-> order_manager:start() end)),
    io:format("ordermanager PID: ~p\n", [whereis(order_manager)]),

    register(watchdog, spawn(fun() -> watchdog:start() end)),
    io:format("watchdog PID: ~p\n", [whereis(watchdog)]),

    %node_connection:start(),

    register(fsm, spawn(fun()-> fsm:start() end)),
    io:format("FSM PID: ~p\n", [whereis(fsm)]),

    A = spawn(fun()-> button_reader:read_button_loop(1) end),
    io:format("button_reader PID: ~p\n", [A]),

    io:format("Start completed\n\n").