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
	{Port, _} = string:to_integer(cli:get_flag("-p", Args)),

	case Port of
		error ->
			tester:run_test(KeyCount, Host, 8087, UserCount, DayCount, Iterations);
		_ ->
			tester:run_test(KeyCount, Host, Port, UserCount, DayCount, Iterations)
	end.