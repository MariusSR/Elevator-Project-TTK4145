-module(order_manager).
-export([start/0]).

start() ->
    node_connection:start(),
    A = spawn(fun() -> send_msg() end),
	B = spawn(fun() -> recv_msg() end),
    register(sender, A),
    register(recver, B),
    io:format("Helllo, can you here me?\n"),
    node().

send_msg() ->
    case nodes() of
        [] ->
            timer:sleep(500);
        _ ->
            {recver, hd(nodes())} ! "Hey there"
    end,
    timer:sleep(400),
    send_msg().


recv_msg() ->
    receive
		{Msg} ->
			io:format("~w~n", [Msg]);
	
		Unexpected ->
			io:format("unexpected message: ~p~n", [Unexpected])
	end.