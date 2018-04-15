%%=================================================================================================
%% This module interfaces with the HW driver over TCP. The module is interfaced by messages sent to
%% 'driver' received in the 'main_loop'. Some calls are handled by help functions found in the end.
%%=================================================================================================

-module(driver_interface).
-export([start/0, start/1]).

-include("parameters.hrl").
-define(TCP_PORT,       15657).
-define(RECEIVE_TIMEOUT, 2000).
-define(MSG_LENGTH,         4).

start() ->
	Socket = start_tcp_connection(),
	initialize_LEDs(Socket),
	main_loop(Socket).

start(Port) ->
	Socket = start_tcp_connection(Port),
	initialize_LEDs(Socket),
	main_loop(Socket).



%--------------------------------------------------------------------------------------------------
% Loop receiving calls to 'driver'.
%--------------------------------------------------------------------------------------------------
main_loop(Socket) ->
	receive

		{set_motor_dir, up_dir}    -> gen_tcp:send(Socket, [1, 1, 0, 0]);
		{set_motor_dir, stop_dir}  -> gen_tcp:send(Socket, [1, 0, 0, 0]);
		{set_motor_dir, down_dir}  -> gen_tcp:send(Socket, [1, 255, 0, 0]);

		{set_floor_LED, Floor}     -> gen_tcp:send(Socket, [3, Floor - 1, 0, 0]);

		{set_door_open_LED, on}    -> gen_tcp:send(Socket, [4, 1, 0, 0]);
		{set_door_open_LED, off}   -> gen_tcp:send(Socket, [4, 0, 0, 0]);

		{set_stop_button_LED, on}  -> gen_tcp:send(Socket, [5, 1, 0, 0]);
		{set_stop_button_LED, off} -> gen_tcp:send(Socket, [5, 0, 0, 0]);
		
		{set_order_button_LED, on,  {up_button,   Floor}} -> gen_tcp:send(Socket, [2, 0, Floor - 1, 1]);
		{set_order_button_LED, off, {up_button,   Floor}} -> gen_tcp:send(Socket, [2, 0, Floor - 1, 0]);
		{set_order_button_LED, on,  {down_button, Floor}} -> gen_tcp:send(Socket, [2, 1, Floor - 1, 1]);
		{set_order_button_LED, off, {down_button, Floor}} -> gen_tcp:send(Socket, [2, 1, Floor - 1, 0]);
		{set_order_button_LED, on,  {cab_button,  Floor}} -> gen_tcp:send(Socket, [2, 2, Floor - 1, 1]);
		{set_order_button_LED, off, {cab_button,  Floor}} -> gen_tcp:send(Socket, [2, 2, Floor - 1, 0]);

		{get_order_button_status, {up_button,   Floor}, PID} when is_pid(PID) -> return_order_button_status(Socket, PID, 0, Floor - 1);
		{get_order_button_status, {down_button, Floor}, PID} when is_pid(PID) -> return_order_button_status(Socket, PID, 1, Floor - 1);
		{get_order_button_status, {cab_button,  Floor}, PID} when is_pid(PID) -> return_order_button_status(Socket, PID, 2, Floor - 1);

		{get_floor_status, PID} when is_pid(PID) -> return_floor_status(Socket, PID);

		turn_off_all_leds -> turn_off_all_order_LEDs(1);

		reconnect_tcp -> New_socket = restart_tcp_connection(Socket), flush(), main_loop(New_socket);
		
		Unexpected -> io:format("~s Unexpected message in main_loop of driver: ~p\n", [color:red("Driver_interface:"), Unexpected])

	end, 
	main_loop(Socket).



%--------------------------------------------------------------------------------------------------
% Checks if the button 'Button_type' at floor 'Floor' is pressed and sends the response to 'PID'.
%--------------------------------------------------------------------------------------------------
return_order_button_status(Socket, PID, Button_type_int, Floor) ->
	gen_tcp:send(Socket, [6, Button_type_int, Floor, 0]),
		case gen_tcp:recv(Socket, ?MSG_LENGTH, ?RECEIVE_TIMEOUT) of
			{ok, [6, Is_pressed, 0, 0]} -> % Is_pressed = 1 or 0
				% Buttoon type must be converted from (zero-based) integer back  to atom used elsewhere
				Button_type_atom = element(Button_type_int + 1, {up_button, down_button, cab_button}),
				PID ! {order_button_status, {Button_type_atom, Floor + 1}, Is_pressed};

			{error, Reason} ->
				driver ! reconnect_tcp,
				PID ! {error, Reason};
				
			Unexpected ->
				driver ! reconnect_tcp,
				io:format("~s Unexpected message in return_order_button_status: ~p\n", [color:red("Driver_interface:"), Unexpected])
		end.



%--------------------------------------------------------------------------------------------------
% Samples the floor sensor and sends its status to 'PID'.
%--------------------------------------------------------------------------------------------------
return_floor_status(Socket, PID) ->
	gen_tcp:send(Socket, [7, 0, 0, 0]),
		case gen_tcp:recv(Socket, ?MSG_LENGTH, ?RECEIVE_TIMEOUT) of
			{ok, [7, 0, _Latest_floor, 0]} ->
				PID ! between_floors;

			{ok, [7, 1, Latest_floor, 0]} ->
				PID ! {floor, Latest_floor + 1};

			{error, Reason} ->
				driver ! reconnect_tcp,
				PID ! {error, Reason};

			Unexpected ->
				driver ! reconnect_tcp,
				io:format("~s Unexpected message in return_floor_status: ~p\n", [color:red("Driver_interface:"), Unexpected])
		end.



%--------------------------------------------------------------------------------------------------
% Initializes LEDs by turning all off.
%--------------------------------------------------------------------------------------------------
initialize_LEDs(Socket) ->
	gen_tcp:send(Socket, [4, 0, 0, 0]),		% Turn off door open LED
	gen_tcp:send(Socket, [5, 0, 0, 0]),		% Turn off stop button LED
	turn_off_all_order_LEDs(1).				% Starts on first floor looping upwards



%--------------------------------------------------------------------------------------------------
% Turns off all order button LEDs.
%--------------------------------------------------------------------------------------------------
turn_off_all_order_LEDs(1) ->
    lists:foreach(fun(Button_type) -> driver ! {set_order_button_LED, off, {Button_type, 1}} end, [up_button, cab_button]),
    turn_off_all_order_LEDs(2);

turn_off_all_order_LEDs(?NUMBER_OF_FLOORS) -> 
    lists:foreach(fun(Button_type) -> driver ! {set_order_button_LED, off, {Button_type, ?NUMBER_OF_FLOORS}} end, [down_button, cab_button]);

turn_off_all_order_LEDs(Floor) ->
    lists:foreach(fun(Button_type) -> driver ! {set_order_button_LED, off, {Button_type, Floor}} end, [up_button, down_button, cab_button]),
    turn_off_all_order_LEDs(Floor + 1).



%--------------------------------------------------------------------------------------------------
% Restarts a TCP connection by closing the existing socket and then call start_tcp_connection().
%--------------------------------------------------------------------------------------------------
restart_tcp_connection(Old_socket) ->
	gen_tcp:close(Old_socket),
	start_tcp_connection(),



%--------------------------------------------------------------------------------------------------
% Starts a TCP connection to the specified port.
%--------------------------------------------------------------------------------------------------
start_tcp_connection() ->
	case gen_tcp:connect(localhost, ?TCP_PORT, [list, {active, false}]) of
		{ok, Socket} ->
			Socket;
		{error, Reason} ->
			io:format("~s Could not start TCP connection in start_tcp_connection due to: ~p. Retries in 1 second.\n", [color:red("Driver_interface:"), Reason]),
			timer:sleep(1000),
			start_tcp_connection()
	end.

start_tcp_connection(Port) ->
	case gen_tcp:connect(localhost, Port, [list, {active, false}]) of
		{ok, Socket} ->
			Socket;
		{error, Reason} ->
			io:format("~s Could not start TCP connection in start_tcp_connection due to: ~p. Retries in 1 second.\n", [color:red("Driver_interface:"), Reason]),
			timer:sleep(1000),
			start_tcp_connection()
	end.


%--------------------------------------------------------------------------------------------------
% Flushes all queued messages. To be used if the TCP connection has to restart.
%--------------------------------------------------------------------------------------------------
flush() ->
	receive _Anything -> flush()
	after 0 -> finished_flushing
end.