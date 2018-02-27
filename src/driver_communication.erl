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
	{ok, Socket} = gen_tcp:connect(localhost, 15657, [list, {active, false}]),
	main_loop(Socket).

main_loop(Socket) ->
	receive
		{set_motor_dir, up_dir} ->
			gen_tcp:send(Socket, [1, 1, 0, 0]);
		{set_motor_dir, stop_dir} ->
			gen_tcp:send(Socket, [1, 0, 0, 0]);
		{set_motor_dir, down_dir} ->
			gen_tcp:send(Socket, [1, 255, 0, 0]);

		{set_order_button_LED, up_button, Floor, Value}   when Floor >= 1 , Floor =< ?NUMBER_OF_FLOORS ->
			gen_tcp:send(Socket, [2, 0, Floor - 1, Value]);
		{set_order_button_LED, down_button, Floor, Value}  when Floor >= 1 , Floor =< ?NUMBER_OF_FLOORS ->
			gen_tcp:send(Socket, [2, 1, Floor - 1, Value]);
		{set_order_button_LED, cab_button, Floor, Value} when Floor >= 1 , Floor =< ?NUMBER_OF_FLOORS ->
			gen_tcp:send(Socket, [2, 2, Floor - 1, Value]);

		{set_door_open_LED, Value}   when Value =:= 0 ; Value =:= 1 ->
			gen_tcp:send(Socket, [4, Value, 0, 0]);

		{set_stop_button_LED, Value} when Value =:= 0 ; Value =:= 1 ->
			gen_tcp:send(Socket, [5, Value, 0, 0]);

		{get_order_button_status, up_button, Floor, PID} ->
			get_order_button_func(Socket, PID, 0, Floor - 1);
		{get_order_button_status, down_button, Floor, PID} ->
			get_order_button_func(Socket, PID, 1, Floor - 1);
		{get_order_button_status, cab_button, Floor, PID} ->
			get_order_button_func(Socket, PID, 2, Floor - 1);
		
		{get_stop_button_status, PID} ->
			get_stop_button_func(Socket, PID);

		{get_floor, PID} ->
			get_floor_func(Socket, PID);
		
		Unexpected ->
			io:format("unexpected message: ~p~n", [Unexpected])
	end, 
	main_loop(Socket).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Help functions taking care og the communication with the elevator driver that	 %%
%% sends data back to the procces asking for information.  						     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_order_button_func(Socket, PID, Button_type, Floor) ->
	gen_tcp:send(Socket, [6, Button_type, Floor, 0]),
		case gen_tcp:recv(Socket, 4, 2000) of
			{ok, [6, Is_pressed, 0, 0]} ->
				Button_type_atom = element(Button_type + 1, {up_button, down_button, cab_button}),
				PID ! {order_button_status, Button_type_atom, Floor + 1, Is_pressed};
			{error, Reason} ->
				PID ! {error, Reason}
		end.

get_floor_func(Socket, PID) ->
	gen_tcp:send(Socket, [7, 0, 0, 0]),
		case gen_tcp:recv(Socket, 4, 2000) of
			{ok, [7, 1, _Latest_floor, 0]} ->
				PID ! {between_floors};
			{ok, [7, 0, Latest_floor, 0]} ->
				PID ! {floor, Latest_floor + 1};
			{error, Reason} ->
				PID ! {error, Reason}
		end.

get_stop_button_func(Socket, PID) ->
	gen_tcp:send(Socket, [8, 0, 0, 0]),
		case gen_tcp:recv(Socket, 4, 2000) of
			{ok, [8, Is_pressed, 0, 0]} ->
				PID ! {stop_button_status, Is_pressed};
			{error, Reason} ->
				PID ! {error, Reason}
		end.
