% Module is forked from https://github.com/julianduque/erlang-color and used for ANSI coloring of terminal output

-define(ESC,       <<"\e[">>).
-define(RST,         <<"0">>).
-define(BOLD,        <<"1">>).
-define(SEP,         <<";">>).
-define(END,         <<"m">>).

%% Colors
-define(BLACK,      <<"30">>).
-define(RED,        <<"31">>).
-define(GREEN,      <<"32">>).
-define(YELLOW,     <<"33">>).
-define(BLUE,       <<"34">>).
-define(MAGENTA,    <<"35">>).
-define(CYAN,       <<"36">>).
-define(WHITE,      <<"37">>).
-define(DEFAULT,    <<"39">>).

%% Background color
-define(DEFAULT_BG, <<"49">>).

%% RGB
-define(RGB_FG, [<<"38">>, ?SEP, <<"5">>]).
-define(RGB_BG, [<<"48">>, ?SEP, <<"5">>]).

%% True 24-bit colors
-define(TRUE_COLOR_FG, [<<"38">>, ?SEP, <<"2">>]).
-define(TRUE_COLOR_BG, [<<"48">>, ?SEP, <<"2">>]).