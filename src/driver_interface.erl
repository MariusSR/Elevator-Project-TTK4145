%%=================================================================================================
%% This module interfaces with the HW driver over TCP. The module is interfaced by messages sent to
%% 'driver' received in the 'main_loop'. Some calls are handled by help functions found in the end.
%%=================================================================================================

-module(driver_interface).
-export([start/0, start/1]).

-include("parameters.hrl").
%-define(TIMEOUT, 2000).
-define(TIMEOUT, 5000).
-define(MSG_LENGTH, 4).

start() ->
	{ok, Socket} = gen_tcp:connect(localhost, 15657, [list, {active, false}]),
	initialize_LED(Socket),
	main_loop(Socket).

start(Port) ->
	{ok, Socket} = gen_tcp:connect(localhost, Port, [list, {active, false}]),
	initialize_LED(Socket),
	main_loop(Socket).



%--------------------------------------------------------------------------------------------------
% Loop receiving calls to 'driver'
%--------------------------------------------------------------------------------------------------
main_loop(Socket) ->
	receive

		{set_motor_dir, up_dir}   -> gen_tcp:send(Socket, [1, 1, 0, 0]);
		{set_motor_dir, stop_dir} -> gen_tcp:send(Socket, [1, 0, 0, 0]);
		{set_motor_dir, down_dir} -> gen_tcp:send(Socket, [1, 255, 0, 0]);
		
		{set_order_button_LED, up_button,   Floor, on}  -> gen_tcp:send(Socket, [2, 0, Floor - 1, 1]);
		{set_order_button_LED, up_button,   Floor, off} -> gen_tcp:send(Socket, [2, 0, Floor - 1, 0]);
		{set_order_button_LED, down_button, Floor, on}  -> gen_tcp:send(Socket, [2, 1, Floor - 1, 1]);
		{set_order_button_LED, down_button, Floor, off} -> gen_tcp:send(Socket, [2, 1, Floor - 1, 0]);
		{set_order_button_LED, cab_button,  Floor, on}  -> gen_tcp:send(Socket, [2, 2, Floor - 1, 1]);
		{set_order_button_LED, cab_button,  Floor, off} -> gen_tcp:send(Socket, [2, 2, Floor - 1, 0]);

		{set_floor_LED, Floor} -> gen_tcp:send(Socket, [3, Floor - 1, 0, 0]);

		{set_door_open_LED, on}  -> gen_tcp:send(Socket, [4, 1, 0, 0]);
		{set_door_open_LED, off} -> gen_tcp:send(Socket, [4, 0, 0, 0]);

		{set_stop_button_LED, on}  -> gen_tcp:send(Socket, [5, 1, 0, 0]);
		{set_stop_button_LED, off} -> gen_tcp:send(Socket, [5, 0, 0, 0]);

		{get_order_button_status, up_button, Floor, PID}   when is_pid(PID) -> return_order_button_status(Socket, PID, 0, Floor - 1);
		{get_order_button_status, down_button, Floor, PID} when is_pid(PID) -> return_order_button_status(Socket, PID, 1, Floor - 1);
		{get_order_button_status, cab_button, Floor, PID}  when is_pid(PID) -> return_order_button_status(Socket, PID, 2, Floor - 1);

		{get_floor_status, PID} when is_pid(PID) -> return_floor_status(Socket, PID);

		turn_off_all_leds -> turn_off_all_order_LEDs(1);
		
		Unexpected -> io:format("~s message in main_loop of driver module: ~p~n", [color:red("Unexpected:"), Unexpected])

	end, 
	main_loop(Socket).



%--------------------------------------------------------------------------------------------------
% Checks if the button 'Button_type' at floor 'Floor' is pressed and sends the response to 'PID'
%--------------------------------------------------------------------------------------------------
return_order_button_status(Socket, PID, Button_type, Floor) ->
	gen_tcp:send(Socket, [6, Button_type, Floor, 0]),
		case gen_tcp:recv(Socket, ?MSG_LENGTH, ?TIMEOUT) of
			{ok, [6, Is_pressed, 0, 0]} ->
				% Buttoon type must be converted from (zero-based) integer to atom used elsewhere
				Button_type_atom = element(Button_type + 1, {up_button, down_button, cab_button}),
				PID ! {order_button_status, Button_type_atom, Floor + 1, Is_pressed};
			{error, Reason} ->
				PID ! {error, Reason};
			Unexpected ->
                    io:format("~s pattern match in driver, return_order_button_status: ~p\n", [color:red("Unexpected:"), Unexpected])
		end.



%--------------------------------------------------------------------------------------------------
% Samples the floor sensor and sends its status to 'PID'
%--------------------------------------------------------------------------------------------------
return_floor_status(Socket, PID) ->
	gen_tcp:send(Socket, [7, 0, 0, 0]),
		case gen_tcp:recv(Socket, ?MSG_LENGTH, ?TIMEOUT) of
			{ok, [7, 0, _Latest_floor, 0]} ->
				PID ! between_floors;
			{ok, [7, 1, Latest_floor, 0]} ->
				PID ! {floor, Latest_floor + 1};
			{error, Reason} ->
				PID ! {error, Reason};
			Unexpected ->
                    io:format("~s pattern match in driver, return_floor_status: ~p\n", [color:red("Unexpected:"), Unexpected])
		end.



%--------------------------------------------------------------------------------------------------
% Initializes LEDs by turning all off
%--------------------------------------------------------------------------------------------------
initialize_LED(Socket) ->
	gen_tcp:send(Socket, [4, 0, 0, 0]),		% turn off door open LED
	gen_tcp:send(Socket, [5, 0, 0, 0]),		% turn off stop button LED
	turn_off_all_order_LEDs(1).				% starts on first floor looping upwards



%--------------------------------------------------------------------------------------------------
% Turns off all order button LEDs
%--------------------------------------------------------------------------------------------------
turn_off_all_order_LEDs(1) ->
    lists:foreach(fun(Button_type) -> driver ! {set_order_button_LED, Button_type, 1, off} end, [up_button, cab_button]),
    turn_off_all_order_LEDs(2);

turn_off_all_order_LEDs(?NUMBER_OF_FLOORS) -> 
    lists:foreach(fun(Button_type) -> driver ! {set_order_button_LED, Button_type, ?NUMBER_OF_FLOORS, off} end, [down_button, cab_button]);

turn_off_all_order_LEDs(Floor) when is_integer(Floor) andalso Floor > 1 andalso Floor < ?NUMBER_OF_FLOORS ->
    lists:foreach(fun(Button_type) -> driver ! {set_order_button_LED, Button_type, Floor, off} end, [up_button, down_button, cab_button]),
    turn_off_all_order_LEDs(Floor + 1).
