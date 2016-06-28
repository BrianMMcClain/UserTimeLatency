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
	{PortA, _} = string:to_integer(cli:get_flag("-p", Args)),
	TableA = cli:get_flag("-t", Args),

	case PortA of
		error ->
			Port = 8087;
		_ ->
			Port = PortA
	end,

	case TableA of
		error ->
			Table = "action";
		_ ->
			Table = TableA
	end,

	tester:run_test(Host, Port, Table, UserCount, DayCount, QueryCount, OpCount).