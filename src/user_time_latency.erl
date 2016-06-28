-module(user_time_latency).
-export([main/1]).

main(Args) ->
	
	code:add_path("./ebin"),
	code:add_paths(filelib:wildcard("./deps/*/ebin", "./")),

	Host = cli:get_flag("-h", Args),
	{UserCount, _} = string:to_integer(cli:get_flag("-u", Args)),
	{DayCount, _} = string:to_integer(cli:get_flag("-d", Args)),
	{QueryCount, _} = string:to_integer(cli:get_flag("-q", Args)),
	{OpCount, _} = string:to_integer(cli:get_flag("-c", Args)),
	{Port, _} = string:to_integer(cli:get_flag("-p", Args)),

	case Port of
		error ->
			tester:run_test(Host, 8087, UserCount, DayCount, QueryCount, OpCount);
		_ ->
			tester:run_test(Host, Port, UserCount, DayCount, QueryCount, OpCount)
	end.