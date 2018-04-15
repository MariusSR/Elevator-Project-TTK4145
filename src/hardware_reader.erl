%%=================================================================================================
%% This module periodically
%%     - samples the floor sensor and sends the result to 'fsm' and
%%     - checks if any button is pressed and sends the result to 'data_manager'.
%%=================================================================================================

-module(hardware_reader).
-export([start/0]).

-include("parameters.hrl").
-define(RECEIVE_TIMEOUT, 200).
-define(READ_FLOOR_SENSOR_INTERVAL, 100).
-define(BUTTON_SAMPLE_PERIOD, 20).

start() ->
    Button_reader_PID = spawn(fun()-> read_button_loop(1) end),
    io:format("\n~s~p", [color:cyan("Button_reader PID:       "), Button_reader_PID]),
    
    Floor_sensor_reader_PID = spawn(fun()-> read_floor_sensor_loop() end),
    io:format("\n~s~p", [color:cyan("Floor_sensor_reader PID: "), Floor_sensor_reader_PID]).



%--------------------------------------------------------------------------------------------------
% Loop periodically sampling the floor sensor and sends its response to 'fsm'.
%--------------------------------------------------------------------------------------------------
read_floor_sensor_loop() ->
    driver ! {get_floor_status, self()},
    receive
        between_floors      -> fsm !  {floor_sensor, between_floors} ;
        {floor, Read_floor} -> fsm !  {floor_sensor, Read_floor};

        {error, Reason} ->
            io:format("~s Error message in read_floor: ~p\n", [color:red("Hardware_reader:"), Reason]),
            timer:sleep(500);

        Unexpected ->
            io:format("~s Unexpected message in read_floor: ~p\n", [color:red("Hardware_reader:"), Unexpected])

    after
        ?RECEIVE_TIMEOUT ->
            io:format("~s Timeout receiving floor sensor data in read_floor\n", [color:red("Hardware_reader:")])
    end,
    
    timer:sleep(?READ_FLOOR_SENSOR_INTERVAL),
    read_floor_sensor_loop().




%--------------------------------------------------------------------------------------------------
% Loop iterating over all order buttons, periodically sampling/checking for new orders.
%--------------------------------------------------------------------------------------------------
sample(Button) -> timer:sleep(?BUTTON_SAMPLE_PERIOD), send_new_order_to_ordermanager(Button).

read_button_loop(1) ->
    lists:foreach(fun sample/1, [{up_button, 1}, {cab_button, 1}]),
    read_button_loop(2);

read_button_loop(?NUMBER_OF_FLOORS) -> 
    lists:foreach(fun sample/1, [{down_button, ?NUMBER_OF_FLOORS}, {cab_button, ?NUMBER_OF_FLOORS}]),
    read_button_loop(1);

read_button_loop(Floor) ->
    lists:foreach(fun sample/1, [{up_button, Floor}, {down_button, Floor}, {cab_button, Floor}]),
    read_button_loop(Floor + 1).



%--------------------------------------------------------------------------------------------------
% Checks if 'Order'(-button) is pressed. If pressed, sends order to 'data_manager'.
%--------------------------------------------------------------------------------------------------
send_new_order_to_ordermanager(Order) ->
    driver ! {get_order_button_status, Order, self()},
    receive
        {order_button_status,  Order, 1} -> communicator ! {new_order, Order};
        {order_button_status, _Order, 0} -> button_not_pressed;

        {error, Reason} ->
            io:format("~s Error message in send_new_order: ~p\n", [color:red("Hardware_reader:"), Reason]),
            timer:sleep(500);

        Unexpected ->
            io:format("~s Unexpected message in send_new_order: ~p\n", [color:red("Hardware_reader:"), Unexpected])

    after
        ?RECEIVE_TIMEOUT ->
            io:format("~s Timeout receiving order button status in send_new_order\n", [color:red("Hardware_reader:")])
    end.