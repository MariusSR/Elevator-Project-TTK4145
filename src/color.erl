% Module is forked from https://github.com/julianduque/erlang-color and used for ANSI coloring of terminal output

-module(color).
-export([black/1, blackb/1, red/1, redb/1, green/1, greenb/1, blue/1, blueb/1]).
-export([yellow/1, yellowb/1, magenta/1, magentab/1, cyan/1, cyanb/1, white/1, whiteb/1]).
-export([rgb/2, on_rgb/2]).
-export([true/2, on_true/2]).
-include("color.hrl").

black(Text)      -> [color(?BLACK),      Text, reset()].
blackb(Text)     -> [colorb(?BLACK),     Text, reset()].
red(Text)        -> [color(?RED),        Text, reset()].
redb(Text)       -> [colorb(?RED),       Text, reset()].
green(Text)      -> [color(?GREEN),      Text, reset()].
greenb(Text)     -> [colorb(?GREEN),     Text, reset()].
yellow(Text)     -> [color(?YELLOW),     Text, reset()].
yellowb(Text)    -> [colorb(?YELLOW),    Text, reset()].
blue(Text)       -> [color(?BLUE),       Text, reset()].
blueb(Text)      -> [colorb(?BLUE),      Text, reset()].
magenta(Text)    -> [color(?MAGENTA),    Text, reset()].
magentab(Text)   -> [colorb(?MAGENTA),   Text, reset()].
cyan(Text)       -> [color(?CYAN),       Text, reset()].
cyanb(Text)      -> [colorb(?CYAN),      Text, reset()].
white(Text)      -> [color(?WHITE),      Text, reset()].
whiteb(Text)     -> [colorb(?WHITE),     Text, reset()].

rgb(RGB, Text) ->
  [?ESC, ?RGB_FG, ?SEP, rgb_color(RGB), ?END, Text, reset()].

on_rgb(RGB, Text) ->
  [?ESC, ?RGB_BG, ?SEP, rgb_color(RGB), ?END, Text, reset_bg()].

true(RGB, Text) ->
  [?ESC, ?TRUE_COLOR_FG, ?SEP, true_color(RGB), ?END, Text, reset()].

on_true(RGB, Text) ->
  [?ESC, ?TRUE_COLOR_BG, ?SEP, true_color(RGB), ?END, Text, reset()].

%% Internal
color(Color) ->
  <<?ESC/binary, Color/binary, ?END/binary>>.

colorb(Color) ->
  <<?ESC/binary, Color/binary, ?SEP/binary, ?BOLD/binary, ?END/binary>>.

rgb_color([R, G, B]) when R >= 0, R =< 5, G >= 0, G =< 5, B >= 0, B =< 5 ->
  integer_to_list(16 + (R * 36) + (G * 6) + B).

true_color([R1, R2, G1, G2, B1, B2]) ->
  R = erlang:list_to_integer([R1, R2], 16),
  G = erlang:list_to_integer([G1, G2], 16),
  B = erlang:list_to_integer([B1, B2], 16),
  true_color([R, G, B]);

true_color([R, G, B]) when R >= 0, R =< 255, G >= 0, G =< 255, B >= 0, B =< 255 ->
  [integer_to_list(R), ?SEP, integer_to_list(G), ?SEP, integer_to_list(B)].

reset() ->
  <<?ESC/binary, ?RST/binary, ?END/binary>>.

reset_bg() ->
  <<?ESC/binary, ?DEFAULT_BG/binary, ?END/binary>>.