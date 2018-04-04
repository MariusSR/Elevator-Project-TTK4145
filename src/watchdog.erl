-module(watchdog).
-export([start/0]).
-define(TIME_LIMIT_ORDER, 10000).
-define(TIME_LIMIT_MOVING_BETWEEN_FLOORS, 3000).

start() ->
    main_loop([], {no_pid, false}).

main_loop(Watch_list, Movement_watcher_PID) -> %watch_list er en liste av ordre med tilhørende PID for prosess som timer den, altå [{PID, Order}, ...]
    receive

        {start_watching, Order} -> % endre til start_watching_order
            PID = spawn(fun() -> watchdog_timer(assigned_hall_order, Order) end),
            main_loop(Watch_list ++ [{PID, Order}], Movement_watcher_PID);


        {stop_watching, Order} -> % tenker denne skal kalles når en ordre blir utført, uavhengig om den watches eller ikke. cab og hall sendes hver for seg, som 2 kall
            case lists:keyfind(Order, 2, Watch_list) of
                false ->
                    main_loop(Watch_list, Movement_watcher_PID);
                {PID, Order} ->
                    PID ! order_finished,
                    main_loop(Watch_list -- [{PID, Order}], Movement_watcher_PID)
            end;


        start_watching_movement ->
            PID = spawn(fun() -> watchdog_timer(between_floor) end),
            main_loop(Watch_list, {PID, false});


        stop_watching_movement when is_pid(Movement_watcher_PID)->
            Movement_watcher_PID ! reached_floor,
            main_loop(Watch_list, {no_pid, false});

        stop_watching_movement ->
            main_loop(Watch_list, {no_pid, false}).


        {order_timed_out, PID, Order} ->
            main_loop(Watch_list -- [{PID, Order}], Movement_watcher_PID);

        movement_timed_out ->
            fsm ! timeout_movement,
            main_loop(Watch_list, no_pid);
        
        reset_movement_time_out ->
            main_loop(Watch_list, no_pid)
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