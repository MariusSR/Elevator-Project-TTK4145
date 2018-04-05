%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This module consecutively checks if a button is pressed, and if it is it passes   %%
%% the new order to Ordermanager. This is done with a loop called read_button_loop   %%
%% which iterates over all buttons and sleeps for SLEEP_TIME ms when all buttons are %%
%% sampled once.                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(button_reader).
-export([start/0]).
-include("parameters.hrl").
-define(SLEEP_TIME, 100).
-define(TIMEOUT, 100).
-define(READ_FLOOR_SENSOR_INTERVAL, 200).

start() ->
    Button_reader_PID = spawn(fun()-> read_button_loop(1) end),
    io:format("~s~p\n", [color:cyan("Button_reader PID:       "), Button_reader_PID]),

    Floor_sensor_reader_PID = spawn(fun()-> read_floor_sensor_loop() end),
    io:format("~s~p\n", [color:cyan("Floor_sensor_reader PID: "), Floor_sensor_reader_PID]).

%--------------------------------------------------------------------------------------------------
% Loop iterating over all order buttons to check for new orders.
%--------------------------------------------------------------------------------------------------
read_button_loop(1) ->
    timer:sleep(?SLEEP_TIME),
    lists:foreach(fun(Button_type) -> send_new_order_to_ordermanager(Button_type, 1) end, [up_button, cab_button]),
    read_button_loop(2);

read_button_loop(?NUMBER_OF_FLOORS) -> 
    lists:foreach(fun(Button_type) -> send_new_order_to_ordermanager(Button_type, ?NUMBER_OF_FLOORS) end, [down_button, cab_button]),
    read_button_loop(1);

read_button_loop(Floor) when is_integer(Floor) andalso Floor > 1 andalso Floor < ?NUMBER_OF_FLOORS ->
    lists:foreach(fun(Button_type) -> send_new_order_to_ordermanager(Button_type, Floor) end, [up_button, down_button, cab_button]),
    read_button_loop(Floor + 1).

%--------------------------------------------------------------------------------------------------
% Checks if 'Button_type' at 'Floor' is pressed. If pressed, sends order to order_manager.
%--------------------------------------------------------------------------------------------------
send_new_order_to_ordermanager(Button_type, Floor)
when is_integer(Floor) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS andalso is_atom(Button_type) ->
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
        ?TIMEOUT ->
            io:format("Timeout in button reader module\n")
    end.


read_floor_sensor_loop() ->
    driver ! {get_floor, self()},
    receive
        between_floors ->
            fsm !  {floor_sensor, between_floors} ;
        {floor, Read_floor} ->
            fsm !  {floor_sensor, Read_floor};
        Unexpected ->
            io:format("Unexpected msg in read_floor_sensor: ~p\n", [Unexpected])
    end,
    timer:sleep(?READ_FLOOR_SENSOR_INTERVAL),
    read_floor_sensor_loop().
