-module(button_reader).
-export([read_button_loop/1]).
-include("parameters.hrl").
-define(SLEEP_TIME, 100).
-define(TIMEOUT, 100).

%% Loop iterating over all order buttons to check for new orders
read_button_loop(1) ->
    timer:sleep(?SLEEP_TIME),
    lists:foreach(fun(Button_type) -> send_order_to_order_manager_if_button_is_pressed(Button_type, 1) end, [up_button, cab_button]),
    read_button_loop(2);

read_button_loop(?NUMBER_OF_FLOORS) -> 
    lists:foreach(fun(Button_type) -> send_order_to_order_manager_if_button_is_pressed(Button_type, ?NUMBER_OF_FLOORS) end, [down_button, cab_button]),
    read_button_loop(1);

read_button_loop(Floor) when is_integer(Floor) andalso Floor > 1 andalso Floor < ?NUMBER_OF_FLOORS ->
    lists:foreach(fun(Button_type) -> send_order_to_order_manager_if_button_is_pressed(Button_type, Floor) end, [up_button, down_button, cab_button]),
    read_button_loop(Floor + 1).

%% Checks if the button of type Button_type at floor Floor is pressed. If pressed, sends order to order_manager.
send_order_to_order_manager_if_button_is_pressed(Button_type, Floor)
when is_integer(Floor) andalso Floor > 1 andalso Floor < ?NUMBER_OF_FLOORS andalso is_atom(Button_type) ->
    driver ! {get_order_button_status, Button_type, Floor, self()},
    receive
        {order_button_status, Button_type, Floor, 1} ->
            order_manager ! {new_order, {Button_type, Floor}};
        {order_button_status, Button_type, Floor, 0} ->
            ok;
        {error, Reason} ->
            io:format("ERROR: receiving button status for button type ~p on floor ~p failed due to: ~s~n", [Button_type, Floor, Reason]);
        _ ->
            io:format("Unknown message received for button type ~p on floor ~p\n", [Button_type, Floor])
    after
        ?TIMEOUT ->
            io:format("Timeout in button reader module\n")
    end
