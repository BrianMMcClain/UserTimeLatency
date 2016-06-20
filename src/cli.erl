-module(cli).
-export([get_flag/2]).

get_flag(Flag, [Flag, Arg | _]) ->
	Arg;
get_flag(Flag, [_ | Rest]) ->
	get_flag(Flag, Rest);
get_flag(_, []) ->
	error.