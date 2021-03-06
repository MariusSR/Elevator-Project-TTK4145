%%=================================================================================================
%% This module is a watchdog timer. Both movement time between floors and completion of hall orders
%% can be monitored. A timed out order is moved back to the list of unassigend orders, whereas
%% movement timeouts forces the elevator to be suspended for a while before it reinitializes.
%%=================================================================================================

-module(watchdog).
-export([start/0]).
-include("global_parameters.hrl").

-define(TIME_LIMIT_ORDER_PER_FLOOR, 6000).
-define(TIME_LIMIT_MOVING_BETWEEN_FLOORS, 3000).


start() ->
    link(whereis(fsm)), link(whereis(data_manager)),
    main_loop([], no_pid).


main_loop(Watch_list, Movement_watcher_PID) ->

    receive

        %--------------------------------------------------------------------------------------------------
        % Starts watching 'Hall_order'. A timer is spawned whos PID is stored in 'Watch_list'.
        %--------------------------------------------------------------------------------------------------
        {start_watching_order, Hall_order} ->
            case lists:keyfind(Hall_order, 2, Watch_list) of
                false ->
                    PID = spawn_link(fun() -> watchdog_timer(assigned_hall_order, Hall_order) end),
                    main_loop(Watch_list ++ [{PID, Hall_order}], Movement_watcher_PID);
                _Already_watched  ->
                    main_loop(Watch_list, Movement_watcher_PID)
            end;



        %--------------------------------------------------------------------------------------------------
        % Stops watching 'Hall_order'. The timer corresponding to 'Hall_order' is stopped and removed from
        % 'Watch_list'. 
        %--------------------------------------------------------------------------------------------------
        {stop_watching_order, Hall_order} ->
            case lists:keyfind(Hall_order, 2, Watch_list) of
                {PID, Hall_order} when is_pid(PID) ->
                    PID ! order_served,
                    main_loop(Watch_list -- [{PID, Hall_order}], Movement_watcher_PID);
                _Else ->
                    main_loop(Watch_list, Movement_watcher_PID)
            end;



        %--------------------------------------------------------------------------------------------------
        % Starts watching the movement time between two floors of an elevator. A timer is spawned and
        % stored in 'Movement_watcher_PID'.
        %--------------------------------------------------------------------------------------------------
        start_watching_movement ->
            PID = spawn_link(fun() -> watchdog_timer(between_floor) end),
            main_loop(Watch_list, PID);



        %--------------------------------------------------------------------------------------------------
        % Stops watching movement between floors.
        %--------------------------------------------------------------------------------------------------
        stop_watching_movement when is_pid(Movement_watcher_PID)->
            Movement_watcher_PID ! reached_floor,
            main_loop(Watch_list, no_pid);

        stop_watching_movement ->
            main_loop(Watch_list, no_pid);



        %--------------------------------------------------------------------------------------------------
        % Handles timeouts and updates, respectively, 'Watch_list' and 'Movement_watcher_PID'.
        %--------------------------------------------------------------------------------------------------
        {order_timed_out, PID, Order} ->
            io:format("~s The following order timed out: ~p.\n", [color:magenta("Watchdog:"), Order]),
            data_manager ! {unmark_order_assigned, Order},
            main_loop(Watch_list -- [{PID, Order}], Movement_watcher_PID);


        movement_timed_out ->
            io:format("~s Movement between floors timed out.\n", [color:magenta("Watchdog:")]),
            fsm ! timeout_movement,
            lists:foreach(fun({PID, _Order}) -> PID ! order_served end, Watch_list),
            main_loop([], no_pid)

    end.



%--------------------------------------------------------------------------------------------------
% Timers for completion of order and movement between floors.
%--------------------------------------------------------------------------------------------------
watchdog_timer(assigned_hall_order, Order) ->
    receive order_served ->
        ok
    after (?TIME_LIMIT_ORDER_PER_FLOOR * ?NUMBER_OF_FLOORS) ->
        watchdog ! {order_timed_out, self(), Order}
    end.
        
watchdog_timer(between_floor) ->
    receive reached_floor ->
        ok
    after ?TIME_LIMIT_MOVING_BETWEEN_FLOORS ->
        watchdog ! movement_timed_out
    end.