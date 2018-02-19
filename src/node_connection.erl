%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This module takes care of setting up the node cluster as well %%
%% as adding new nodes as they get online. This is done with the %%
%% functions broadcast_self() and listen_for_nodes(). The former %%
%% broadcasts its own node name over UDP to other nodes, while   %%
%% the latter looks for new nodes to connect. The initial local  %%
%% cluster setup with longnames is done in init_node_cluster().  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(node_connection).
-export([start/0]).

-define(RECEIVE_PORT, 5679).
-define(BROADCAST_PORT, 5678).
-define(TIMEOUT, 2000).
-define(BROADCAST_SLEEP, 5000).
-define(TICKTIME, 1000).
-define(COOKIE, 'top_secret').

start() ->
	init_node_cluster(),
	spawn(fun() -> broadcast_self() end),
	spawn(fun() -> listen_for_nodes() end),
	io:format("Node cluster initialized, now searching for friends.~n").

%% Start node cluster with coockie
init_node_cluster() ->
	%os:cmd("epmd -daemon"), %% check what this is
	%timer:sleep(100), % perhaps needed
	ThisNode = list_to_atom("elevator@" ++ get_IP()),
	{ok, _Pid} = net_kernel:start([ThisNode, longnames, ?TICKTIME]),
	erlang:set_cookie(ThisNode, ?COOKIE).

%% Get local IP address on format 123.456.789.012
get_IP() ->
	{ok, Addresses} = inet:getif(), 			% Undocumented function returning all local IPs
	inet_parse:ntoa(element(1, hd(Addresses))). % Choose the first IP and parses it to a string

%% Broadcast its own node name
broadcast_self() ->
	{ok, BroadcastSocket} = gen_udp:open(?BROADCAST_PORT, [list, {broadcast, true}]),
	broadcast_self(BroadcastSocket).

broadcast_self(BroadcastSocket) ->
	gen_udp:send(BroadcastSocket, {255, 255, 255, 255}, ?RECEIVE_PORT, atom_to_list(node())),
	timer:sleep(?BROADCAST_SLEEP),
	broadcast_self(BroadcastSocket).

%% Listen for new nodes to connect
listen_for_nodes() ->
	{ok, ReceiveSocket} = gen_udp:open(?RECEIVE_PORT, [list, {active, false}]),
	listen_for_nodes(ReceiveSocket).

listen_for_nodes(ReceiveSocket) ->
	case gen_udp:recv(ReceiveSocket, 0, ?TIMEOUT) of
		{ok, {_Address, _Port, NodeName}} ->
			Node = list_to_atom(NodeName),			%Burde en ikke gjøre om Nodename til en atom?
			case lists:member(Node, [node()|nodes()]) of
				false ->
					net_kernel:connect_node(Node),
					io:format("New node connected: ~w~n", [Node]);
				_ -> 
					ok
			end;

		{error, timeout} ->
			ok;

		{error, Reason} ->
			io:format("ERROR: receiving node failed due to: ~s~n", [Reason])
	end,
	listen_for_nodes(ReceiveSocket).
