-module(order_manager).
-export([start/0]).
-include("parameters.hrl").

-record(orderlist, {assigned = [], unassigned = [], cab_orders = []}).
-record(state, {is_idle, direction, floor}).

start() ->
    main_loop([], []). % temporary to avoid green comoplains



main_loop(Orderlist, Global_states) ->
    io:format("Main loop of order_manager/scheduler started\n"),
    receive
        {add_order, {cab_button, Floor}, PID} when is_pid(PID) ->
            Order = {cab_button, Floor},
            case lists:member(Order, Orderlist#orderlist.cab_orders) of
                true ->
                    B = "HVA SKJED HVS DEN FINNES FRA FØR?";
                false ->
                    PID ! {order_added, Order},
                    New_order_list = create_updated_orderlist(Orderlist, Order),
                    main_loop(New_order_list, Global_states)
            end;

        {add_order, {Order}} ->
            ok
    end.


create_updated_orderlist(Orderlist, {cab_button, Floor})
when is_record(Orderlist, orderlist) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
    Cab_order = {cab_button, Floor},
    case lists:member(Cab_order, Orderlist#orderlist.cab_orders) of
        true ->
            Orderlist;
        false ->
            Orderlist#orderlist{cab_orders = Orderlist#orderlist.cab_orders ++ [Cab_order]}
    end;

create_updated_orderlist(Orderlist, {Button_type, Floor})
when is_record(Orderlist, orderlist) andalso is_atom(Button_type) andalso Floor >= 1 andalso Floor =< ?NUMBER_OF_FLOORS ->
    Hall_order = {Button_type, Floor},
    case lists:member(Hall_order, Orderlist#orderlist.assigned ++ Orderlist#orderlist.unassigned) of
        true ->
            Orderlist;
        false ->
            Orderlist#orderlist{unassigned = Orderlist#orderlist.unassigned ++ [Hall_order]}
    end.