%%=================================================================================================
%% This module periodically
%%     - samples the floor sensor and sends the result to 'fsm' and
%%     - checks if any button is pressed and sends the result to 'order_manager'.
%%=================================================================================================

-module(button_reader).
-export([start/0]).

-include("parameters.hrl").
-define(READ_BUTTON_SAMPLING_SLEEP, 100).
-define(RECEIVE_TIMEOUT, 100).
-define(READ_FLOOR_SENSOR_INTERVAL, 200).

start() ->
    Button_reader_PID = spawn(fun()-> read_button_loop(1) end),
    io:format("~s~p\n", [color:cyan("Button_reader PID:       "), Button_reader_PID]),

    Floor_sensor_reader_PID = spawn(fun()-> read_floor_sensor_loop() end),
    io:format("~s~p\n", [color:cyan("Floor_sensor_reader PID: "), Floor_sensor_reader_PID]).



%--------------------------------------------------------------------------------------------------
% Loop periodically sampling the floor sensor and sends its response to 'fsm'
%--------------------------------------------------------------------------------------------------
read_floor_sensor_loop() ->
    driver ! {get_floor_status, self()},
    receive
        between_floors ->
            fsm !  {floor_sensor, between_floors} ;
        {floor, Read_floor} ->
            fsm !  {floor_sensor, Read_floor};
        Unexpected ->
            io:format("Unexpected msg in read_floor_sensor: ~p\n", [Unexpected])
    after
        ?RECEIVE_TIMEOUT ->
            io:format("Timeout reading floor sensor in button reader module\n")
    end,
    timer:sleep(?READ_FLOOR_SENSOR_INTERVAL),
    read_floor_sensor_loop().




%--------------------------------------------------------------------------------------------------
% Loop iterating over all order buttons, periodically sampling/checking for new orders
%--------------------------------------------------------------------------------------------------
read_button_loop(1) ->
    timer:sleep(?READ_BUTTON_SAMPLING_SLEEP),
    lists:foreach(fun(Button_type) -> send_new_order_to_ordermanager(Button_type, 1) end, [up_button, cab_button]),
    read_button_loop(2);

read_button_loop(?NUMBER_OF_FLOORS) -> 
    lists:foreach(fun(Button_type) -> send_new_order_to_ordermanager(Button_type, ?NUMBER_OF_FLOORS) end, [down_button, cab_button]),
    read_button_loop(1);

read_button_loop(Floor) when is_integer(Floor) andalso Floor > 1 andalso Floor < ?NUMBER_OF_FLOORS ->
    lists:foreach(fun(Button_type) -> send_new_order_to_ordermanager(Button_type, Floor) end, [up_button, down_button, cab_button]),
    read_button_loop(Floor + 1).



%--------------------------------------------------------------------------------------------------
% Checks if 'Button_type' at 'Floor' is pressed. If pressed, sends order to 'order_manager'
%--------------------------------------------------------------------------------------------------
send_new_order_to_ordermanager(Button_type, Floor) ->
    driver ! {get_order_button_status, Button_type, Floor, self()},
    receive
        {order_button_status, Button_type, Floor, 1} ->
            node_communicator ! {new_order, {Button_type, Floor}};
        {order_button_status, _Button_type, _Floor, 0} ->
            ok;
        {error, Reason} ->
            io:format("ERROR: receiving button status for button type ~p on floor ~p failed due to: ~s~n", [Button_type, Floor, Reason]);
        Unexpected ->
            io:format("Unexpexted message received for button type ~p on floor ~p in the button_reader module: ~p\n", [Button_type, Floor, Unexpected])
    after
        ?RECEIVE_TIMEOUT ->
            io:format("Timeout asking for order button status in button reader module\n")
    end.