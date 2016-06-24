-module(tester).
-export([run_test/1, run_test/2, run_test/3, run_test/4, run_test/5]).

-define(MS_PER_DAY, 86400000).
-define(KEYS_PER_USER_PER_MONTH, 17000).

run_test(KeyCount) ->
	run_test(KeyCount, "127.0.0.1").

run_test(KeyCount, Host) ->
	run_test(KeyCount, Host, 100).

run_test(KeyCount, Host, UserCount) ->
	run_test(KeyCount, Host, UserCount, 180).

run_test(KeyCount, Host, UserCount, DayCount) ->
	run_test(KeyCount, Host, UserCount, DayCount, 100).

run_test(KeyCount, Host, UserCount, DayCount, Iterations) ->
	io:format("Querying ~p keys from ~s for ~p users and ~p days of data~n", [KeyCount, Host, UserCount, DayCount]),
	
	StartTimestamp = 1420088400000,
	%TotalEndTimestamp = StartTimestamp + (?MS_PER_DAY * DayCount),
	MsPerKey = trunc((?MS_PER_DAY * 30) / (?KEYS_PER_USER_PER_MONTH * UserCount)),
	EndTimestamp = trunc(StartTimestamp + (MsPerKey * (KeyCount * UserCount))),

	random:seed(now()),
	{ok, Pid} = riakc_pb_socket:start_link(Host, 8087),

	{TotalTime, TotalKeys} = run_query_loop(Pid, StartTimestamp, EndTimestamp, UserCount, KeyCount, DayCount, Iterations),
	io:format("Avg Keys: ~p keys~nAvg Time: ~p ms~n", [TotalKeys / Iterations, TotalTime / Iterations]).

run_query_loop(Pid, StartTimestamp, EndTimestamp, UserCount, KeyCount, DayCount, Iterations) ->
	run_query_loop(Pid, StartTimestamp, EndTimestamp, UserCount, KeyCount, DayCount, Iterations, 0, 0).

run_query_loop(_Pid, _StartTimestamp, _EndTimestamp, _UserCount, _KeyCount, _DayCount, 0, TotalTime, TotalKeys) ->
	{TotalTime, TotalKeys};

run_query_loop(Pid, StartTimestamp, EndTimestamp, UserCount, KeyCount, DayCount, Iterations, TotalTime, TotalKeys) ->
	QueryDays = (KeyCount / ?KEYS_PER_USER_PER_MONTH) * 30,
	UserId = 100000000000000000 + random:uniform(UserCount) - 1,
	MaxEndTS = (StartTimestamp + (DayCount * ?MS_PER_DAY)) - (?MS_PER_DAY * QueryDays),
	TotalTimeSpan = MaxEndTS - StartTimestamp,
	GStartTimestamp = StartTimestamp + (random:uniform(round(TotalTimeSpan) - 1)),
	GEndTimestamp = round(GStartTimestamp + (?MS_PER_DAY * QueryDays)),

	Query = lists:flatten(io_lib:format("SELECT * FROM action WHERE UtcTime >= ~p AND UtcTime <= ~p AND UserId = '~p'", [GStartTimestamp, GEndTimestamp, UserId])),

	StartTime = now(),
	{ok, {_Schema, Data}} = riakc_ts:query(Pid, Query),
	EndTime = now(),
	TimeDiff = timer:now_diff(EndTime, StartTime) / 1000, % microseconds converted to milliseconds
	io:format("(~p) ~p: ~p keys~n", [Iterations, TimeDiff, length(Data)]),
	TimeRunningTotal = TotalTime + TimeDiff,
	KeysRunningTotal = TotalKeys + length(Data),
	run_query_loop(Pid, StartTimestamp, EndTimestamp, UserCount, KeyCount, DayCount, Iterations-1, TimeRunningTotal, KeysRunningTotal).