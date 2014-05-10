-module(hacksense_json).
-export([encode/1]).

encode(Value) -> mochijson2:encode(encode_value(Value)).

encode_value(List) when is_list(List) ->
	lists:map(fun encode_value/1, List);
encode_value(Value) ->
	maps:to_list(Value).
