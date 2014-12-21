-module(hacksense_json).
-export([encode/1]).

encode(Value) -> mochijson2:encode(encode_value(Value)).

encode_value(List) when is_list(List) ->
	lists:map(fun encode_value/1, List);
encode_value(Value) ->
	lists:map(fun mangle_list_item/1, maps:to_list(Value)).

mangle_list_item({Key, Map}) when is_map(Map) ->
	{Key, encode_value(Map)};
mangle_list_item(Other) -> Other.
