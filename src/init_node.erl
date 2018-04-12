%%=================================================================================================
%% This module spawns every other module and prints the corresponding PIDs. The depencies are as
%% follows:
%% 
%% This module                 Communicates with these modules
%% ________________________________________________________________________________________________
%%   init_node                :  N/A
%%   driver_interface         :  Receives msgs from communication_interface and hardware_reader
%%   hardware_reader          :  Communcates with driver_interface using PIDs.
%%   node_connection          :  Sends node_up/down to orders_and_states, receives from fsm if
%%                            :  it should disconnect.
%%   communication_interface  :  Sends/receives msgs to orders_and_states and driver. Also
%%                            :  sends msgs to the same module on anoteher node.
%%   orders_and_states        :  Sends/receives msgs to communcation_interface, scheduler, fsm
%%                            :  and wathcdog.
%%   watchdog                 :  Communcates with orders_and_states and fsm.
%% ________________________________________________________________________________________________
%% The color module is forked from [INSERT SOURCE] and used extensively in most moduels in prints.
%%=================================================================================================

-module(init_node).
-export([start/0]).

start() ->
    timer:sleep(100), % KA FARSKEN GJØR DENNE SLEEPEN?%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    register(driver, spawn(fun() -> driver_interface:start() end)),
    io:format("\n~s~p\n", [color:cyan("Driver PID:              "), whereis(driver)]),
    timer:sleep(100), % Wait for driver to finish its initialization

    register(fsm, spawn(fun()-> fsm:start() end)),
    io:format("~s~p\n", [color:cyan("FSM PID:                 "), whereis(fsm)]),

    register(communicator, spawn(fun()-> communication_interface:start() end)),
    io:format("~s~p\n", [color:cyan("Communicator PID:        "), whereis(communicator)]),

    register(data_manager, spawn(fun() -> orders_and_states:start() end)),
    io:format("~s~p\n", [color:cyan("Ordermanager PID:        "), whereis(data_manager)]),

    register(watchdog, spawn(fun() -> watchdog:start() end)),
    io:format("~s~p\n", [color:cyan("Watchdog PID:            "), whereis(watchdog)]),

    register(node_connection, spawn(fun() -> node_connection:start() end)),
    io:format("~s~p\n", [color:cyan("Node_connection PID:     "), whereis(node_connection)]),
    timer:sleep(100), % Wait for node cluster to be started properly on this node

    hardware_reader:start(),

    io:format("\n\n").