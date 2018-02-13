-module(driver_communication).
-export([start/0]).
-define(NumberOfFloors, 4).

start() ->
	{ok, Socket} = gen_tcp:connect({127,0,0,1}, 15657, [binary, {active, false}]),
	PID = spawn(fun()-> loop(Socket) end),

	io:format("Spawn succesfull"),
	timer:sleep(2000),

	test(PID),

	terminated_fine.


test(PID) ->
	PID ! {self(), get_stop_button, 0, 0, 0},
	test(PID).

loop(Socket) ->
	receive
		{motor_dir, up} ->
			gen_tcp:send(Socket, [1, 1, 0, 0]);
		{motor_dir, down} ->
			gen_tcp:send(Socket, [1, 255, 0, 0]);
		{motor_dir, stop} ->
			gen_tcp:send(Socket, [1, 0, 0, 0]);

		{order_button_LED, Button_type, Floor, Value} -> %when
		%lists:member(Button_type, [0, 1, 2]),
		%lists:member(Floor, lists:seq(0, ?NumberOfFloors)),
		%lists:member(Value, [0, 1]) ->
			gen_tcp:send(Socket, [2, Button_type, Floor, Value]);

		{door_open_LED, Value} when Value =:= 0 ; Value =:= 1 ->
			gen_tcp:send(Socket, [4, Value, 0, 0]);

		{stop_button_LED, Value} when Value =:= 0 ; Value =:= 1 ->
			gen_tcp:send(Socket, [5, Value, 0, 0]);

		{PID, get_order_button, Button_type, Floor} -> %add guard
			gen_tcp:send(Socket, [6, Button_type, Floor, 0]),
			case gen_tcp:recv(Socket, 4, 2000) of
				{ok, Answer} ->
					io:format("~w", [Answer]),
					io:format("~s", [Answer]);
				{error, Reason} ->
					io:format("~s", [Reason])
			end;

		{PID, get_floor} -> %add guard
			gen_tcp:send(Socket, [7, 0, 0, 0]),
			case gen_tcp:recv(Socket, 4, 2000) of
				{ok, Answer} ->
					io:format("~w", [Answer]),
					io:format("~s", [Answer]);
				{error, Reason} ->
					io:format("~s", [Reason])
			end;
		
		{PID, get_stop_button} -> %add guard
			gen_tcp:send(Socket, [8, 0, 0, 0]),
			case gen_tcp:recv(Socket, 4, 2000) of
				{ok, Answer} ->
					io:format("~w", [Answer]),
					io:format("~s", [Answer]);
				{error, Reason} ->
					io:format("~s", [Reason])
			end

	after
		2000 ->
			io:format("Timeout~n")
	end, 
	loop(Socket).