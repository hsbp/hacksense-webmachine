-module(cash_buf).
-export([fetch_balance/0]).

-define(BALANCE_RE, "([1-9][0-9]*(?:\\.[0-9]*[1-9])?) ([A-Z]{3})").

fetch_balance() ->
	{ok, Contents} = file:read_file(filename:join(code:priv_dir(hacksense), "cash_buf.txt")),
    {match, Balances} = re:run(Contents, ?BALANCE_RE,
                               [global, {capture, all_but_first, binary}]),
    format_balance(Balances).

format_balance(Balances) -> format_balance(Balances, []).
format_balance([], Acc) -> Acc;
format_balance([[ValueBin, Unit] | Rest], Acc) ->
    Value = case binary:match(ValueBin, <<$.>>) of
        nomatch -> binary_to_integer(ValueBin);
        _ -> binary_to_float(ValueBin)
    end,
	Formatted = [{value, Value}, {unit, Unit}, {location, <<"cash buffer">>}],
	format_balance(Rest, [Formatted | Acc]).
