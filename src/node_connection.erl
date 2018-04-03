%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This module takes care of setting up the node cluster as well as adding new nodes %%
%% as they get online. This is done with the functions broadcast_self() and          %%
%% listen_for_nodes(). The former broadcasts its own node name over UDP to other     %%
%% nodes, while the latter looks for new nodes to connect. The initial local cluster %%
%% setup with longnames is done in init_node_cluster().                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%% TODO: endre rekkefølgen på funksjonene, typ skal get_IP nederst?

-module(node_connection).
-export([start/0, get_IP/0]).

-define(RECEIVE_PORT,    5679).
-define(BROADCAST_PORT,  5678).
-define(BROADCAST_SLEEP, 5000).
-define(OFFLINE_SLEEP,  10000).
-define(TIMEOUT,         2000).
-define(TICKTIME,        1000).
-define(COOKIE,  'top_secret').

start() ->
	init_node_cluster(),
	Broadcast_PID  = spawn(fun() -> broadcast_self() end),
	Listen_PID     = spawn(fun() -> listen_for_nodes() end),
	Monitoring_PID = spawn(fun() -> start_node_monitoring() end),
	io:format("Node cluster initialized, now searching for friends.~n"),
	loop([Broadcast_PID, Listen_PID, Monitoring_PID]).

loop(PIDs) ->
	receive
		disconnect_node ->
			lists:foreach(fun(PID) -> PID ! wait end, PIDs),
			erlang:disconnect_node(node()),
			io:format("Disconnected !!!!!!! ~p\n\n\n", [node()]),
			timer:sleep(?OFFLINE_SLEEP),
			loop([Broadcast_PID, Listen_PID, Monitoring_PID]);

		Unexpected ->
			io:format("Unexpected message in node_connection loop: ~p~n", [Unexpected]),
			loop(PIDs)
	end.

%--------------------------------------------------------------------------------------------------
% Start node cluster with coockie
%--------------------------------------------------------------------------------------------------
init_node_cluster() ->
	os:cmd("epmd -daemon"),				% Spawns the the name server required by distributed Erlang
	This_node = list_to_atom("elevator@" ++ get_IP()),
	{ok, _Pid} = net_kernel:start([This_node, longnames, ?TICKTIME]),
	erlang:set_cookie(This_node, ?COOKIE).

%--------------------------------------------------------------------------------------------------
% Get local IP address on format 123.456.789.012
%--------------------------------------------------------------------------------------------------
get_IP() ->
	{ok, Addresses} = inet:getif(), 			   % Undocumented function returning all local IPs
	inet_parse:ntoa(element(1, hd(Addresses))).    % Chooses the first IP and parses it to a string

%--------------------------------------------------------------------------------------------------
% Broadcast its own node name
%--------------------------------------------------------------------------------------------------
broadcast_self() ->
	{ok, Broadcast_socket} = gen_udp:open(?BROADCAST_PORT, [list, {broadcast, true}]),
	gen_udp:send(Broadcast_socket, {255, 255, 255, 255}, ?RECEIVE_PORT, atom_to_list(node())),
	broadcast_self(Broadcast_socket).

broadcast_self(Broadcast_socket) ->
	receive wait -> timer:sleep(?OFFLINE_SLEEP)
	after ?BROADCAST_SLEEP -> ok
	end,
	gen_udp:send(Broadcast_socket, {255, 255, 255, 255}, ?RECEIVE_PORT, atom_to_list(node())),
	broadcast_self(Broadcast_socket).

%--------------------------------------------------------------------------------------------------
% Listen for new nodes to connect
%--------------------------------------------------------------------------------------------------
listen_for_nodes() ->
	{ok, Receive_socket} = gen_udp:open(?RECEIVE_PORT, [list, {active, false}]),
	listen_for_nodes(Receive_socket).

listen_for_nodes(Receive_socket) ->
	receive wait -> timer:sleep(?OFFLINE_SLEEP)
	after 50 -> ok
	end,
	case gen_udp:recv(Receive_socket, 0, ?TIMEOUT) of
		{ok, {_Address, _Port, Node_name}} ->
			Node = list_to_atom(Node_name),
			case lists:member(Node, [node()|nodes()]) of
				false ->
					net_kernel:connect_node(Node);
				true -> 
					ok
			end;

		{error, timeout} ->
			ok;
		{error, Reason} ->
			io:format("ERROR: receiving node failed due to: ~s~n", [Reason]);

		Unexpected ->
			io:format("Unexpected message in listen for nodes: ~p~n", [Unexpected])
	end,
	listen_for_nodes(Receive_socket).

%--------------------------------------------------------------------------------------------------
% HER SKAL NODEDØD OPPDAGES
%--------------------------------------------------------------------------------------------------
start_node_monitoring() ->
	timer:sleep(500), % Sleep to prevent this node from registring existing nodes as "new nodes"
	net_kernel:monitor_nodes(true),
	node_monitoring_loop().

node_monitoring_loop() ->
	receive
		{nodeup, New_node} ->
			io:format("New node connected: ~p\n", [New_node]),
			order_manager ! {node_up, New_node};

		{nodedown, Node} ->
			io:format("Node disconnected: ~p\n", [Node]),
			order_manager ! {node_down, Node};
		
		wait ->
			timer:sleep(?OFFLINE_SLEEP);
		
		Unexpected ->
			io:format("Unexpected message in node_monitoring_loop: ~p~n", [Unexpected])
	end,
	node_monitoring_loop().