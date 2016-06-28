-module(tester).
-export([run_test/1, run_test/2, run_test/3, run_test/4, run_test/5, run_test/6, run_test/7]).

-define(MS_PER_DAY, 86400000).
-define(KEYS_PER_USER_PER_MONTH, 17000).

run_test(Host) ->
	run_test(Host, 8087).

run_test(Host, Port) ->
	run_test(Host, Port, "action").

run_test(Host, Port, Table) ->
	run_test(Host, Port, Table, 100).

run_test(Host, Port, Table, UserCount) ->
	run_test(Host, Port, Table, UserCount, 180).

run_test(Host, Port, Table, UserCount, DayCount) ->
	run_test(Host, Port, Table, UserCount, DayCount, 30).

run_test(Host, Port, Table, UserCount, DayCount, QueryCount) ->
	run_test(Host, Port, Table, UserCount, DayCount, QueryCount, 100).

run_test(Host, Port, Table, UserCount, DayCount, QueryCount, OpCount) ->
	io:format("Querying ~p days from ~s:~p for ~p users and ~p days of data~n", [QueryCount, Host, Port, UserCount, DayCount]),
	
	StartTimestamp = 1420088400000,
	MsPerKey = trunc((?MS_PER_DAY * 30) / (?KEYS_PER_USER_PER_MONTH * UserCount)),
	KeyCount = (?KEYS_PER_USER_PER_MONTH / (QueryCount / 30)),
	EndTimestamp = trunc(StartTimestamp + (MsPerKey * (KeyCount * UserCount))),

	random:seed(now()),
	{ok, Pid} = riakc_pb_socket:start_link(Host, Port),

	io:format("timestamp,latency,keys~n"),

	{TotalTime, TotalKeys} = run_query_loop(Pid, Table, StartTimestamp, EndTimestamp, UserCount, DayCount, QueryCount, OpCount),
	io:format("Avg Keys: ~p keys~nAvg Time: ~p ms~n", [TotalKeys / OpCount, TotalTime / OpCount]).

run_query_loop(Pid, Table, StartTimestamp, EndTimestamp, UserCount, DayCount, QueryCount, OpCount) ->
	run_query_loop(Pid, Table, StartTimestamp, EndTimestamp, UserCount, DayCount, QueryCount, OpCount, 0, 0).

run_query_loop(_Pid, _Table, _StartTimestamp, _EndTimestamp, _UserCount, _DayCount, _QueryCount, 0, TotalTime, TotalKeys) ->
	{TotalTime, TotalKeys};

run_query_loop(Pid, Table, StartTimestamp, EndTimestamp, UserCount, DayCount, QueryCount, OpCount, TotalTime, TotalKeys) ->
	UserId = 100000000000000000 + random:uniform(UserCount) - 1,
	MaxEndTS = (StartTimestamp + (DayCount * ?MS_PER_DAY)) - (?MS_PER_DAY * QueryCount),
	TotalTimeSpan = MaxEndTS - StartTimestamp,
	GStartTimestamp = StartTimestamp + (random:uniform(round(TotalTimeSpan) - 1)),
	GEndTimestamp = round(GStartTimestamp + (?MS_PER_DAY * QueryCount)),

	Query = lists:flatten(io_lib:format("SELECT * FROM ~p WHERE UtcTime >= ~p AND UtcTime <= ~p AND UserId = '~p'", [Table, GStartTimestamp, GEndTimestamp, UserId])),

	StartTime = now(),
	{ok, {_Schema, Data}} = riakc_ts:query(Pid, Query),
	EndTime = now(),
	TimeDiff = timer:now_diff(EndTime, StartTime) / 1000, % microseconds converted to milliseconds
	
	{Mega, Sec, Micro} = EndTime,
  	EndTimeInMS = (Mega*1000000 + Sec)*1000 + round(Micro/1000),

	io:format("~p,~p,~p~n", [EndTimeInMS, TimeDiff, length(Data)]),
	
	TimeRunningTotal = TotalTime + TimeDiff,
	KeysRunningTotal = TotalKeys + length(Data),
	run_query_loop(Pid, Table, StartTimestamp, EndTimestamp, UserCount, DayCount, QueryCount, OpCount-1, TimeRunningTotal, KeysRunningTotal).