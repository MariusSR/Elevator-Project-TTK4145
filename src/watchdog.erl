-module(watchdog).
-export([start/0]).
-define(TIME_LIMIT_ORDER, 10000).
-define(TIME_LIMIT_MOVING_BETWEEN_FLOORS, 3000).

start() ->
    main_loop([], {no_pid, false}).

main_loop(Watch_list, Movement_status) -> %watch_list er en liste av ordre med tilhørende PID for prosess som timer den, altå [{PID, Order}, ...]
    receive
        is_movement_timed_out ->
            fsm ! element(2, Movement_status),
            main_loop(Watch_list, Movement_status);

        {start_watching, Order} ->
            PID = spawn(fun() -> watchdog_timer(assigned_hall_order, Order) end),
            main_loop(Watch_list ++ [{PID, Order}], Movement_status);
        
        {stop_watching, Order} -> % tenker denne skal kalles når en ordre blir utført, uavhengig om den watches eller ikke. cab og hall sendes hver for seg, som 2 kall
            case lists:keyfind(Order, 2, Watch_list) of
                false ->
                    main_loop(Watch_list, Movement_status);
                {PID, Order} ->
                    PID ! order_finished,
                    main_loop(Watch_list -- [{PID, Order}], Movement_status)
            end;

        start_watching_movement ->
            PID = spawn(fun() -> watchdog_timer(between_floor) end),
            main_loop(Watch_list, {PID, false});
        
        stop_watching_movement ->
            {PID, _Is_timed_out} = Movement_status,
            case PID == no_pid of
                true ->
                    main_loop(Watch_list, {no_pid, false});
                false ->
                    PID ! reached_floor,
                    main_loop(Watch_list, {no_pid, false})
            end;
        
        {order_timed_out, PID, Order} ->
            main_loop(Watch_list -- [{PID, Order}], Movement_status);

        movement_timed_out ->
            main_loop(Watch_list, {no_pid, true});
        
        reset_movement_time_out ->
            main_loop(Watch_list, {no_pid, false})
    end.

watchdog_timer(assigned_hall_order, Order) ->
    receive
        order_finished ->
            ok
    after
        ?TIME_LIMIT_ORDER ->
            io:format("Order timed out!\n"),
            order_manager ! {unassign_hall_order, Order},
            watchdog ! {order_timed_out, self(), Order}
    end.
        
watchdog_timer(between_floor) ->
    receive
        reached_floor ->
            ok
    after
        ?TIME_LIMIT_MOVING_BETWEEN_FLOORS ->
            io:format("Used too much time moving between floors!\n"),
            watchdog ! movement_timed_out
    end.