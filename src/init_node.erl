%%==============================================================================================================================================
%% This module spawns every other module and prints the corresponding PIDs. Registered moduels are assigned the following names:
%%
%%            Module              Registered name
%% _________________________________________________
%%   driver_interface          :   driver
%%   node_connection           :   node_connector
%%   communication_interface   :   communicator
%%   orders_and_states         :   data_manager
%%   fsm                       :   fsm
%%   watchdog                  :   watchdog
%% _________________________________________________
%%
%% The depencies between modules can then be stated as follows:
%% 
%%       This module            Receives msgs/function calls from                             Sends msgs/function calls to
%% _____________________________________________________________________________________________________________________________________________
%%   init_node         :   N/A                                          :   N/A
%%   driver            :   hardware_reader, communicator, fsm           :   hardware_reader (answer query)
%%   hardware_reader   :   driver                                       :   driver, communicator, fsm
%%   node_connector    :   fsm (upon errors only)                       :   data_manager
%%   communicator      :   hardware_reader, data_manager, fsm, itself   :   driver, data_manager, itself
%%   data_manager      :   communicator, scheduler, watchdog            :   communicator, scheduler, fsm, watchdog
%%   fsm               :   hardware_reader, data_manager, watchdog      :   driver, communicator, watchdog, node_connector (upon errors only)
%%   watchdog          :   data_manager, fsm                            :   data_manager, fsm
%%   scheduler         :   data_manager                                 :   data_manager
%% _____________________________________________________________________________________________________________________________________________
%%
%% The color module is forked from [INSERT SOURCE] and used extensively in prints throughout the project:
%%   - Red     prints indicate an error or an unexpected message/behaviour
%%   - Cyan    prints indicate PIDs of processes spawned from this module
%%   - Green   prints indicate a node connecting
%%   - Magenta prints indicate timeouts and nodes disconnecting
%%   - Yellow  prints indicate change of state in fsm
%%==============================================================================================================================================

-module(init_node).
-export([start/0]).

start() ->
    timer:sleep(100), % KA FARSKEN GJØR DENNE SLEEPEN?%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    register(driver, spawn(fun() -> driver_interface:start() end)),
    io:format("\n~s~p", [color:cyan("Driver PID:              "), whereis(driver)]),
    timer:sleep(100), % Wait for driver to finish its initialization

    register(fsm, spawn(fun()-> fsm:start() end)),
    io:format("\n~s~p", [color:cyan("FSM PID:                 "), whereis(fsm)]),

    register(communicator, spawn(fun()-> communication_interface:start() end)),
    io:format("\n~s~p", [color:cyan("Communicator PID:        "), whereis(communicator)]),

    register(data_manager, spawn(fun() -> orders_and_states:start() end)),
    io:format("\n~s~p", [color:cyan("Data Manager PID:        "), whereis(data_manager)]),

    register(watchdog, spawn(fun() -> watchdog:start() end)),
    io:format("\n~s~p", [color:cyan("Watchdog PID:            "), whereis(watchdog)]),

    register(node_connector, spawn(fun() -> node_connection:start() end)),
    io:format("\n~s~p", [color:cyan("Node Connector PID:      "), whereis(node_connector)]),
    timer:sleep(100), % Wait for node cluster to be started properly on this node

    hardware_reader:start(),

    io:format("\n\n").