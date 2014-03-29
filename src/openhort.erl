-module(openhort).
-export([fetch_temperatures/0]).

fetch_temperatures() ->
	{ok, Contents} = file:read_file(filename:join(code:priv_dir(hacksense), "openhort.txt")),
	format_temperatures(binary:split(Contents, <<$\n>>, [trim, global])).

format_temperatures(Temperatures) -> format_temperatures(Temperatures, 1, []).
format_temperatures([], _, Acc) -> lists:reverse(Acc);
format_temperatures([TempLine | Rest], Number, Acc) ->
	Value = list_to_float(binary_to_list(TempLine)),
	Name = iolist_to_binary(["sensor ", integer_to_list(Number)]),
	Formatted = [{value, Value}, {unit, <<"°C">>}, {location, <<"OpenHort">>}, {name, Name}],
	format_temperatures(Rest, Number + 1, [Formatted | Acc]).
