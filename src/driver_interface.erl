%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This module takes care of communication with the elevator driver over TCP. A      %%
%% loop-function is spawned, which is always available for receiving requests about  %%
%% reading from and writing to the elevator hardware. The function loop runs forever %%
%% whereas start returns the PID for loop.              			   			     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(driver_communication).
-export([start/0, start/1]).
-include("parameters.hrl").
-define(TIMEOUT, 2000).
-define(MSG_LENGTH, 4).

start() ->
	{ok, Socket} = gen_tcp:connect(localhost, 15657, [list, {active, false}]),
	initialize_LED(Socket),
	main_loop(Socket).

start(Port) ->
	{ok, Socket} = gen_tcp:connect(localhost, Port, [list, {active, false}]),
	initialize_LED(Socket),
	main_loop(Socket).

main_loop(Socket) ->
	receive
		{set_motor_dir, up_dir} ->
			gen_tcp:send(Socket, [1, 1, 0, 0]);
		{set_motor_dir, stop_dir} ->
			gen_tcp:send(Socket, [1, 0, 0, 0]);
		{set_motor_dir, down_dir} ->
			gen_tcp:send(Socket, [1, 255, 0, 0]);
		
		{set_order_button_LED, up_button,   Floor, on}  when is_integer(Floor) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
			gen_tcp:send(Socket, [2, 0, Floor - 1, 1]);
		{set_order_button_LED, up_button,   Floor, off} when is_integer(Floor) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
			gen_tcp:send(Socket, [2, 0, Floor - 1, 0]);
		{set_order_button_LED, down_button, Floor, on}  when is_integer(Floor) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
			gen_tcp:send(Socket, [2, 1, Floor - 1, 1]);
		{set_order_button_LED, down_button, Floor, off} when is_integer(Floor) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
			gen_tcp:send(Socket, [2, 1, Floor - 1, 0]);
		{set_order_button_LED, cab_button,  Floor, on}  when is_integer(Floor) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
			gen_tcp:send(Socket, [2, 2, Floor - 1, 1]);
		{set_order_button_LED, cab_button,  Floor, off} when is_integer(Floor) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
			gen_tcp:send(Socket, [2, 2, Floor - 1, 0]);

		{set_door_open_LED, on} ->
			gen_tcp:send(Socket, [4, 1, 0, 0]);
		{set_door_open_LED, off} ->
			gen_tcp:send(Socket, [4, 0, 0, 0]);

		{set_stop_button_LED, on} ->
			gen_tcp:send(Socket, [5, 1, 0, 0]);
		{set_stop_button_LED, off} ->
			gen_tcp:send(Socket, [5, 0, 0, 0]);

		{set_floor_LED, Floor} ->
			gen_tcp:send(Socket, [3, Floor - 1, 0, 0]);

		{get_order_button_status, up_button, Floor, PID}   when is_integer(Floor) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
			return_order_button_status(Socket, PID, 0, Floor - 1);
		{get_order_button_status, down_button, Floor, PID} when is_integer(Floor) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
			return_order_button_status(Socket, PID, 1, Floor - 1);
		{get_order_button_status, cab_button, Floor, PID}  when is_integer(Floor) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_pid(PID) ->
			return_order_button_status(Socket, PID, 2, Floor - 1);
		
		{get_stop_button_status, PID} when is_pid(PID) ->
			return_stop_button_status(Socket, PID);

		{get_floor, PID} when is_pid(PID) ->
			return_floor_status(Socket, PID);

		turn_off_all_leds ->
			turn_off_leds_at_floor(1);
		
		Unexpected ->
			io:format("Unexpected message in main_loop of driver module: ~p~n", [Unexpected])

	end, 
	main_loop(Socket).


%--------------------------------------------------------------------------------------------------
% Help functions taking care of communication with the elevator driver.
% The return values are sent back to the process asking for information.
%--------------------------------------------------------------------------------------------------

return_order_button_status(Socket, PID, Button_type, Floor) when is_pid(PID) andalso is_integer(Button_type) andalso is_integer(Floor) andalso Floor >= 0 andalso Floor < ?NUMBER_OF_FLOORS ->
	gen_tcp:send(Socket, [6, Button_type, Floor, 0]),
		case gen_tcp:recv(Socket, ?MSG_LENGTH, ?TIMEOUT) of
			{ok, [6, Is_pressed, 0, 0]} ->
				% The elevator server returns button type as a zero indexed integer but we want an atom
				Button_type_atom = element(Button_type + 1, {up_button, down_button, cab_button}),
				PID ! {order_button_status, Button_type_atom, Floor + 1, Is_pressed};
			{error, Reason} ->
				PID ! {error, Reason}
		end.

return_floor_status(Socket, PID) when is_pid(PID) ->
	gen_tcp:send(Socket, [7, 0, 0, 0]),
		case gen_tcp:recv(Socket, ?MSG_LENGTH, ?TIMEOUT) of
			{ok, [7, 0, _Latest_floor, 0]} ->
				PID ! between_floors;
			{ok, [7, 1, Latest_floor, 0]} ->
				PID ! {floor, Latest_floor + 1};
			{error, Reason} ->
				PID ! {error, Reason}
		end.

return_stop_button_status(Socket, PID) when is_pid(PID) ->
	gen_tcp:send(Socket, [8, 0, 0, 0]),
		case gen_tcp:recv(Socket, ?MSG_LENGTH, ?TIMEOUT) of
			{ok, [8, Is_pressed, 0, 0]} ->
				PID ! {stop_button_status, Is_pressed};
			{error, Reason} ->
				PID ! {error, Reason}
		end.


turn_off_leds_at_floor(1) ->
    lists:foreach(fun(Button_type) -> driver ! {set_order_button_LED, Button_type, 1, off} end, [up_button, cab_button]),
    turn_off_leds_at_floor(2);

turn_off_leds_at_floor(?NUMBER_OF_FLOORS) -> 
    lists:foreach(fun(Button_type) -> driver ! {set_order_button_LED, Button_type, ?NUMBER_OF_FLOORS, off} end, [down_button, cab_button]);

turn_off_leds_at_floor(Floor) when is_integer(Floor) andalso Floor > 1 andalso Floor < ?NUMBER_OF_FLOORS ->
    lists:foreach(fun(Button_type) -> driver ! {set_order_button_LED, Button_type, Floor, off} end, [up_button, down_button, cab_button]),
    turn_off_leds_at_floor(Floor + 1).


initialize_LED(Socket) ->
	gen_tcp:send(Socket, [4, 0, 0, 0]),		% turn off door open LED
	gen_tcp:send(Socket, [5, 0, 0, 0]),		% turn off stop button LED
	turn_off_leds_at_floor(1).