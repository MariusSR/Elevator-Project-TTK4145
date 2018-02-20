-module(order_manager).
-export([start/0]).

start() ->
    node_connection:start(),
    register(shell, self()),
    spawn(fun() -> send_msg() end),
	spawn(fun() -> recv_msg() end),
    io:format("Helllo, can you here me?\n"),
    node().

send_msg() ->
    case nodes() of
        [] ->
            timer:sleep(500);
        _ ->
            io:format("HER")
    end,
    timer:sleep(400),
    send_msg().


recv_msg() ->
    receive
		{hello, from, OtherShell} ->
			io:format("~w~n", [OtherShell]);
	
		Unexpected ->
			io:format("unexpected message: ~p~n", [Unexpected])
	end.