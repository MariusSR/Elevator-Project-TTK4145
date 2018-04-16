%%=================================================================================================
%% This module sets up the node cluster and adds new nodes to it as they get online. This is done
%% by broadcasting its node name - correspondingly listen for other node names - on the network.
%%=================================================================================================

-module(node_connection).
-export([start/0, get_IP/0]).

-define(RECEIVE_PORT,    5679).
-define(BROADCAST_PORT,  5678).
-define(BROADCAST_SLEEP, 5000).
-define(OFFLINE_SLEEP,   5000).
-define(LISTEN_TIMEOUT,  2000).
-define(TICKTIME,        1000).
-define(COOKIE,  'top_secret').


start() ->
	link(whereis(data_manager)),
	init_node_cluster(),
	Broadcast_PID  = spawn_link(fun() -> broadcast_self() end),
	Listen_PID     = spawn_link(fun() -> listen_for_nodes() end),
	Monitoring_PID = spawn_link(fun() -> start_node_monitoring() end),
	loop([Broadcast_PID, Listen_PID, Monitoring_PID]).



loop(PIDs) ->
	receive
		disconnect_node ->
			lists:foreach(fun(PID) -> PID ! suspend end, PIDs),		 % Suspends reconnection of node
			rpc:eval_everywhere(erlang, disconnect_node, [node()]),  % Discconnect node() everywhere
			loop(PIDs);

		Unexpected ->
			io:format("~s Unexpected message in loop: ~p.\n", [color:red("Node_connection:"), Unexpected]),
			loop(PIDs)
	end.



%--------------------------------------------------------------------------------------------------
% Start node cluster with node name 'elevator@ip_adress'.
%--------------------------------------------------------------------------------------------------
init_node_cluster() ->
	os:cmd("epmd -daemon"),  % Spawns the the name server required by distributed Erlang
	Node_name  = list_to_atom("elevator@" ++ get_IP()),
	{ok, _Pid} = net_kernel:start([Node_name, longnames, ?TICKTIME]),
	erlang:set_cookie(Node_name, ?COOKIE).



%--------------------------------------------------------------------------------------------------
% Get local IP address on format 123.456.789.012.
%--------------------------------------------------------------------------------------------------
get_IP() ->
	{ok, Network_interfaces} = inet:getifaddrs(),
	case proplists:get_value("eno1", Network_interfaces, undefined) of
		undefined ->  % Non-Linux (personal computers)
			{ok, Addresses} = inet:getif(), 			 % Undocumented function returning all local IPs
			inet_parse:ntoa(element(1, hd(Addresses)));  % Chooses the first IP and parses it to a string

		Interface ->  % Linux (at realtime lab computers)
			IP_address = proplists:get_value(addr, Interface),
			inet_parse:ntoa(IP_address)
	end.



%--------------------------------------------------------------------------------------------------
% Broadcast its own node name.
%--------------------------------------------------------------------------------------------------
broadcast_self() ->
	{ok, Broadcast_socket} = gen_udp:open(?BROADCAST_PORT, [list, {broadcast, true}]),
	gen_udp:send(Broadcast_socket, {255, 255, 255, 255}, ?RECEIVE_PORT, atom_to_list(node())),
	broadcast_self(Broadcast_socket).

broadcast_self(Broadcast_socket) ->
	receive suspend -> timer:sleep(?OFFLINE_SLEEP) after ?BROADCAST_SLEEP -> resume end,
	gen_udp:send(Broadcast_socket, {255, 255, 255, 255}, ?RECEIVE_PORT, atom_to_list(node())),
	broadcast_self(Broadcast_socket).



%--------------------------------------------------------------------------------------------------
% Listen for new nodes to connect.
%--------------------------------------------------------------------------------------------------
listen_for_nodes() ->
	{ok, Receive_socket} = gen_udp:open(?RECEIVE_PORT, [list, {active, false}]),
	listen_for_nodes(Receive_socket).

listen_for_nodes(Receive_socket) ->
	receive suspend -> timer:sleep(?OFFLINE_SLEEP) after 50 -> resume end,

	case gen_udp:recv(Receive_socket, 0, ?LISTEN_TIMEOUT) of
		{ok, {_Address, _Port, Node_name}} ->
			Node = list_to_atom(Node_name),
			case lists:member(Node, [node()|nodes()]) of
				true  -> ignore;
				false -> net_kernel:connect_node(Node)
			end;

		{error, timeout} -> ok;
		{error, Reason}  -> io:format("~s Error in listen_for_nodes: ~p.\n", [color:red("Node_connection:"), Reason]);
		Unexpected       -> io:format("~s Unexpected message in listen_for_nodes: ~p.\n", [color:red("Node_connection:"), Unexpected])
	end,

	listen_for_nodes(Receive_socket).



%--------------------------------------------------------------------------------------------------
% Start monitoring of node cluster. Notifies 'data_manager' whenever a node connects/disconnects.
%--------------------------------------------------------------------------------------------------
start_node_monitoring() ->
	timer:sleep(500), 				 % Prevent this node discovering existing nodes as new nodes
	net_kernel:monitor_nodes(true),  % Enable monitoring of other nodes on this node
	node_monitoring_loop().

node_monitoring_loop() ->
	receive
		{nodeup, New_node} ->
			io:format("~s ~p.\n", [color:green("New node connected:"), New_node]),
			data_manager ! {node_up, New_node};

		{nodedown, Node} ->
			io:format("~s ~p.\n", [color:magenta("Node disconnected:"), Node]),
			data_manager ! {node_down, Node};
		
		suspend ->
			net_kernel:monitor_nodes(false),
			timer:sleep(?OFFLINE_SLEEP),
			start_node_monitoring();

		Unexpected ->
			io:format("~s Unexpected message in node_monitoring: ~p.\n", [color:red("Node_connection:"), Unexpected])
	end,
	node_monitoring_loop().
