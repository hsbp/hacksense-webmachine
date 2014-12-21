-module(hack2o).
-export([fetch_level/0]).

fetch_level() ->
	{ok, <<L>>} = file:read_file(filename:join(code:priv_dir(hacksense), "waterlevel.txt")),
	[[{value, (L - $0) / 8 * 100}, {unit, <<$%>>}, {location, <<"Bucket">>}]].
