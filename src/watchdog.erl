-module(watchdog).
-export([start/0]).
-define(TIME_LIMIT, 10000).

start() ->
    main_loop([]).

main_loop(Watch_list) -> %watch_list er en liste av ordre med tilhørende PID for prosess som timer den, altå [{PID, Order}, ...]
    receive
        {start_watching, Order} ->
            PID = spawn(fun() -> watchdog_timer(Order) end),
            main_loop(Watch_list ++ [{PID, Order}]);
        
        {stop_watching, Order} -> % tenker denne skal kalles når en ordre blir utført, uavhengig om den watches eller ikke. cab og hall sendes hver for seg, som 2 kall
            case lists:keyfind(Order, 2, Watch_list) of
                false ->
                    main_loop(Watch_list);
                {PID, Order} ->
                    PID ! order_finished,
                    main_loop(Watch_list -- [{PID, Order}])
            end;
        
        {timed_out, PID, Order} ->
            main_loop(watch_list -- [{PID, Order}])
    end.

watchdog_timer(Order) ->
    receive
        order_finished ->
            ok
    after
        ?TIME_LIMIT ->
            io:format("Order timed out!\n"),
            order_manager ! {unassign_hall_order, Order}, %denne må lages i order_manager
            watchdog ! {timed_out, self(), Order}
    end.
        
