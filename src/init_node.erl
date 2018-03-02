-module(init_node).
-export([start/0, start_local/0, start_local/1]).

start() -> 
    node_connection:start(),
     
    register(driver, spawn(fun()-> driver_communication:start() end)),
    io:format("driver PID: ~p\n", [whereis(driver)]),

    register(order_manager, spawn(fun()-> order_manager:node_communication() end)),
    io:format("ordermanager PID: ~p\n", [whereis(order_manager)]),

    A = spawn(fun()-> button_reader:read_button_loop(1) end),
    io:format("button_reader PID: ~p\n", [A]),

    io:format("Start completed\n\n").

start_local() ->
     %node_connection:start(),
     
    register(driver, spawn(fun()-> driver_communication:start() end)),
    io:format("driver PID: ~p\n", [whereis(driver)]),

    register(order_manager, spawn(fun()-> order_manager:node_communication() end)),
    io:format("ordermanager PID: ~p\n", [whereis(order_manager)]),

    A = spawn(fun()-> button_reader:read_button_loop(1) end),
    io:format("button_reader PID: ~p\n", [A]),

    io:format("Start completed\n\n").

start_local(Port) ->
     %node_connection:start(),
     
    register(driver, spawn(fun()-> driver_communication:start(Port) end)),
    io:format("driver PID: ~p\n", [whereis(driver)]),

    register(order_manager, spawn(fun()-> order_manager:node_communication() end)),
    io:format("ordermanager PID: ~p\n", [whereis(order_manager)]),

    A = spawn(fun()-> button_reader:read_button_loop(1) end),
    io:format("button_reader PID: ~p\n", [A]),

    io:format("Start completed\n\n").