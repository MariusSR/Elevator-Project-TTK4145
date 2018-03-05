-module(fsm).
-export([start/0]).

start() ->
    init_elevator().
%     main_loop([]).

% main_loop(State, Latest_floor, Order) ->
%     receive 
%         idle ->
%             io:format("Idle\n");
%             receive
%                 {goto_floor, dir} ->


%             end

%         moving ->
%             io:format("Moving\n");

%         door_open ->
%             io:format("Door open\n")
%     end.


init_elevator() ->
    io:format("Initialise elevator!\n"),
    driver ! {get_floor, self()},
    receive 
        between_floors ->
            driver ! {set_motor_dir, down_dir},
            init_elevator();
        {floor, Latest_floor} ->
            driver ! {set_motor_dir, stop_dir};
        Unexpected ->
            io:format("Unexpected msg received in fsm.erl:~p\n", [Unexpected])
    end.
    
