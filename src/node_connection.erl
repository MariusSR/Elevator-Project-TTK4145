%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This module takes care of setting up the node cluster as well as adding new nodes %%
%% as they get online. This is done with the functions broadcast_self() and          %%
%% listen_for_nodes(). The former broadcasts its own node name over UDP to other     %%
%% nodes, while the latter looks for new nodes to connect. The initial local cluster %%
%% setup with longnames is done in init_node_cluster().                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%% TODO: endre rekkefølgen på funksjonene, typ skal get_IP nederst?

-module(node_connection).
-export([start/0]).

-define(RECEIVE_PORT,    5679).
-define(BROADCAST_PORT,  5678).
-define(BROADCAST_SLEEP, 5000).
-define(TIMEOUT,         2000).
-define(TICKTIME,        1000).
-define(COOKIE,  'top_secret').

start() ->
	init_node_cluster(),
	spawn(fun() -> broadcast_self() end),
	spawn(fun() -> listen_for_nodes() end),
	spawn(fun() -> start_node_monitoring() end),
	io:format("Node cluster initialized, now searching for friends.~n").

%--------------------------------------------------------------------------------------------------
% Start node cluster with coockie
%--------------------------------------------------------------------------------------------------
init_node_cluster() ->
	os:cmd("epmd -daemon"),				% Spawns the the name server required by distributed Erlang
	ThisNode = list_to_atom("elevator@" ++ get_IP()),
	{ok, _Pid} = net_kernel:start([ThisNode, longnames, ?TICKTIME]),
	erlang:set_cookie(ThisNode, ?COOKIE).

%--------------------------------------------------------------------------------------------------
% Get local IP address on format 123.456.789.012
%--------------------------------------------------------------------------------------------------
get_IP() ->
	{ok, Addresses} = inet:getif(), 				% Undocumented function returning all local IPs
	inet_parse:ntoa(element(1, hd(Addresses))).     % Choose the first IP and parses it to a string

%--------------------------------------------------------------------------------------------------
% Broadcast its own node name
%--------------------------------------------------------------------------------------------------
broadcast_self() ->
	{ok, BroadcastSocket} = gen_udp:open(?BROADCAST_PORT, [list, {broadcast, true}]),
	broadcast_self(BroadcastSocket).

broadcast_self(BroadcastSocket) ->
	gen_udp:send(BroadcastSocket, {255, 255, 255, 255}, ?RECEIVE_PORT, atom_to_list(node())),
	timer:sleep(?BROADCAST_SLEEP),
	broadcast_self(BroadcastSocket).

%--------------------------------------------------------------------------------------------------
% Listen for new nodes to connect
%--------------------------------------------------------------------------------------------------
listen_for_nodes() ->
	{ok, ReceiveSocket} = gen_udp:open(?RECEIVE_PORT, [list, {active, false}]),
	listen_for_nodes(ReceiveSocket).

listen_for_nodes(ReceiveSocket) ->
	case gen_udp:recv(ReceiveSocket, 0, ?TIMEOUT) of
		{ok, {_Address, _Port, NodeName}} ->
			Node = list_to_atom(NodeName),
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
			io:format("unexpected message in listen for nodes: ~p~n", [Unexpected])
	end,
	listen_for_nodes(ReceiveSocket).

%--------------------------------------------------------------------------------------------------
% HER SKAL NODEDØD OPPDAGES
%--------------------------------------------------------------------------------------------------
start_node_monitoring() ->
	net_kernel:monitor_nodes(true),
	node_monitoring_loop().

node_monitoring_loop() ->
	receive
		{nodeup, Node} ->
			io:format("New node connected: ~p\n", [Node]);

		{nodedown, Node} ->
			io:format("Node disconnected: ~p\n", [Node]),
			order_manager ! {node_down, Node}
	end,
	node_monitoring_loop().