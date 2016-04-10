%% -*- coding: utf-8 -*-

-module(openhort).
-export([load_model/0, fetch_temperatures/1, fetch_events/1]).
-record(openhort_model, {log_tail}).
-include_lib("kernel/include/file.hrl").

-define(BUFSIZE, 20480).

load_model() ->
	Log = filename:join(code:priv_dir(hacksense), "openhort.txt"),
	{ok, Info} = file:read_file_info(Log, [raw]),
	{ok, File} = file:open(Log, [read, raw, binary]),
	{ok, Buffer} = file:pread(File, Info#file_info.size - ?BUFSIZE, ?BUFSIZE),
	file:close(File),
	#openhort_model{log_tail=Buffer}.

fetch_events(#openhort_model{log_tail=Buffer}) ->
	case re:run(Buffer, "^(\\d+):.+Etetes:OK", [global, {capture, all_but_first, list}, multiline]) of
		{match, Feedings} ->
			[#{name => <<"J. Random Hacker">>, type => <<"feed-axolotl">>,
				timestamp => list_to_integer(Feeding)}
				|| [Feeding] <- Feedings];
		nomatch -> []
	end.

fetch_temperatures(#openhort_model{log_tail=Buffer}) ->
	{match, Measurements} = re:run(Buffer, "uploadSensors&data=([^:]+):", [global, {capture, all_but_first, list}]),
	format_temperatures(lists:last(Measurements)).

format_temperatures(Temperatures) -> format_temperatures(Temperatures, 1, []).
format_temperatures([], _, Acc) -> lists:reverse(Acc);
format_temperatures([TempLine | Rest], Number, Acc) ->
	Value = list_to_float(TempLine),
	Name = iolist_to_binary(["sensor ", integer_to_list(Number)]),
	Formatted = #{value => Value, unit => <<"°C"/utf8>>, location => 'OpenHort', name => Name},
	format_temperatures(Rest, Number + 1, [Formatted | Acc]).
