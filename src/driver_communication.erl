%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This module takes care of communication with the elevator driver over TCP. A      %%
%% loop-function is spawned, which is always available for receiving requests about  %%
%% reading from and writing to the elevator hardware. The function loop runs forever %%
%% whereas start returns the PID for loop.              			     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(driver_communication).
-export([start/0]).
-include("parameters.hrl").

start() ->
	{ok, Socket} = gen_tcp:connect(localhost, 15657, [binary, {active, false}]),
	spawn(fun()-> loop(Socket) end).

loop(Socket) ->
	receive
		{motor_dir, Dir} ->
			set_motor_dir_func(Socket, Dir);

		{order_button_LED, Button_type, Floor, Value} -> %when %lists:member(Button_type, [0, 1, 2]), %lists:member(Floor, lists:seq(0, ?NumberOfFloors)), %lists:member(Value, [0, 1]) ->
			gen_tcp:send(Socket, [2, Button_type, Floor, Value]);

		{door_open_LED, Value} when Value =:= 0 ; Value =:= 1 ->
			gen_tcp:send(Socket, [4, Value, 0, 0]);

		{stop_button_LED, Value} when Value =:= 0 ; Value =:= 1 ->
			gen_tcp:send(Socket, [5, Value, 0, 0]);

		{PID, get_order_button, Button_type, Floor} -> %add guard
			get_order_button_func(Socket, PID, Button_type, Floor);

		{PID, get_floor} -> %add guard
			get_floor_func(Socket, PID);
		
		{PID, get_stop_button} -> %add guard
			get_stop_button_func(Socket, PID);
		
		Unexpected ->
			io:format("unexpected message: ~p~n", [Unexpected])
	end, 
	loop(Socket).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Help functions taking care og the actual communication with the elevator driver.  %%
%% Where applicable it also sends in return the corresponding data to the procces    %%
%% asking for information.						     	     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_motor_dir_func(Socket, up) ->
	gen_tcp:send(Socket, [1, 1, 0, 0]);
set_motor_dir_func(Socket, down) ->
	gen_tcp:send(Socket, [1, 255, 0, 0]);
set_motor_dir_func(Socket, stop) ->
	gen_tcp:send(Socket, [1, 0, 0, 0]);
set_motor_dir_func(_, _) ->
	io:format("ERROR: invalid direction!"),
	error.

get_order_button_func(Socket, PID, Button_type, Floor) ->
	gen_tcp:send(Socket, [6, Button_type, Floor, 0]),
		case gen_tcp:recv(Socket, 4, 2000) of
			{ok, Answer} ->
				PID ! {get_order_button, Button_type, Floor, Answer};
			{error, Reason} ->
				PID ! {error, Reason}
		end.

get_floor_func(Socket, PID) ->
	gen_tcp:send(Socket, [7, 0, 0, 0]),
		case gen_tcp:recv(Socket, 4, 2000) of
			{ok, Answer} ->
				PID ! {get_floor, Answer};
			{error, Reason} ->
				PID ! {error, Reason}
		end.

get_stop_button_func(Socket, PID) ->
	gen_tcp:send(Socket, [8, 0, 0, 0]),
		case gen_tcp:recv(Socket, 4, 2000) of
			{ok, Answer} ->
				PID ! {get_stop_button, Answer};
			{error, Reason} ->
				PID ! {error, Reason}
		end.
