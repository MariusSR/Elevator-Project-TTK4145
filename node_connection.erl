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

-define(RECEIVE_PORT, 1234).
-define(BROADCAST_PORT, 5678).
-define(TIMEOUT, 2000).

start() ->
	init_node_cluster(),
	spawn(fun() -> broadcast_self() end),
	spawn(fun() -> listen_for_nodes() end),
	io:format("Now finding new nodes to add to the cluster."),
	ok.

%%% Start node cluster with coockie
init_node_cluster() ->
	ThisNode = list_to_atom("elevator@" ++ get_IP()),
	{ok, pid} = net_kernel:start(ThisNode, longnames),
	erlang:set_cookie(ThisNode, 'top_secret').

%%% Get local IP address on format 123.456.789.012
get_IP() ->
	{A, B, C, D} = inet:ip4_address(),
	A ++ "." ++ B ++ "." ++ C ++ "." ++ D.

%%% Broadcast its own node name
broadcast_self() ->
	{ok, BroadcastSocket} = gen_udp:open(?BROADCAST_PORT, [binary, {broadcast, true}]),
	broadcast_self(BroadcastSocket).

broadcast_self(BroadcastSocket) ->
	gen_udp:send(BroadcastSocket, {255, 255, 255, 255}, ?RECEIVE_PORT, node()),
	timer:sleep(5000),
	broadcast_self(BroadcastSocket).

%%% Listen for new nodes to connect
listen_for_nodes() ->
	{ok, ReceiveSocket} = gen_udp:open(?RECEIVE_PORT, [binary, {active, false}]),
	listen_for_nodes(ReceiveSocket).

listen_for_nodes(ReceiveSocket) ->
	case gen_udp:recv(ReceiveSocket, 0, ?TIMEOUT) of
		{ok, {_Address, _Port, Node}} ->
			case lists:member(Node, nodes()) of
				false ->
					net_kernel:connect_node(Node),
					io:format("New node connected: ~w~n", [Node])
			end;

		{error, Reason} ->
			io:format("ERROR: ~s~n", [Reason])
	end,
	listen_for_nodes(ReceiveSocket).
