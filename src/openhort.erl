%% -*- coding: utf-8 -*-

-module(openhort).
-export([fetch_temperatures/0]).
-include_lib("kernel/include/file.hrl").

-define(BUFSIZE, 1024).

fetch_temperatures() ->
	Log = filename:join(code:priv_dir(hacksense), "openhort.txt"),
	{ok, Info} = file:read_file_info(Log, [raw]),
	{ok, File} = file:open(Log, [read, raw, binary]),
	{ok, Buffer} = file:pread(File, Info#file_info.size - ?BUFSIZE, ?BUFSIZE),
	file:close(File),
	{match, Measurements} = re:run(Buffer, "uploadSensors&data=([^:]+):", [global, {capture, all_but_first, list}]),
	format_temperatures(lists:last(Measurements)).

format_temperatures(Temperatures) -> format_temperatures(Temperatures, 1, []).
format_temperatures([], _, Acc) -> lists:reverse(Acc);
format_temperatures([TempLine | Rest], Number, Acc) ->
	Value = list_to_float(TempLine),
	Name = iolist_to_binary(["sensor ", integer_to_list(Number)]),
	Formatted = #{value => Value, unit => <<"°C"/utf8>>, location => 'OpenHort', name => Name},
	format_temperatures(Rest, Number + 1, [Formatted | Acc]).
