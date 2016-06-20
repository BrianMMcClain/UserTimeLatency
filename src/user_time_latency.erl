-module(user_time_latency).
-export([main/1]).

main(Args) ->
	
	code:add_path("./ebin"),
	code:add_paths(filelib:wildcard("./deps/*/ebin", "./")),

	Host = cli:get_flag("-h", Args),
	{KeyCount, _} = string:to_integer(cli:get_flag("-k", Args)),
	{UserCount, _} = string:to_integer(cli:get_flag("-u", Args)),
	{DayCount, _} = string:to_integer(cli:get_flag("-d", Args)),
	{Iterations, _} = string:to_integer(cli:get_flag("-i", Args)),

	tester:run_test(KeyCount, Host, UserCount, DayCount, Iterations).