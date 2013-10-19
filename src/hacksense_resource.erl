-module(hacksense_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("hacksense_status.hrl").

-define(ISO_DATETIME_FMT, "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    Body = case wrq:path_info(base, ReqData) of
        "submit" ->
            handle_submit(wrq:disp_path(ReqData)), "OK";
        "status.txt" -> format_txt(get_status());
        "status" -> io_lib:format("~p", [get_status()]);
        A -> io_lib:format("~p", [A]) %% XXX debug
    end,
    {Body, ReqData, State}.

format_txt(Status) ->
    OpenClosed = status_to_open_closed(Status),
    Since = timestamp_to_isofmt(Status#hacksense_status.timestamp),
    ["H.A.C.K. is currently ", OpenClosed, " since ", Since, "\n"].

status_to_open_closed(Status) ->
    case Status#hacksense_status.status of
        1 -> "open";
        0 -> "closed"
    end.

timestamp_to_isofmt({{Y, Mo, D}, {H, Mn, S}}) ->
    io_lib:format(?ISO_DATETIME_FMT, [Y, Mo, D, H, Mn, S]).

get_status() ->
    {atomic, Row} = mnesia:transaction(fun() ->
        TH = mnesia:table(hacksense_status),
        Q = qlc:q([X || X <- TH]),
        QS = qlc:keysort(#hacksense_status.timestamp, Q, [{order,descending}]),
        QC = qlc:cursor(QS),
        case qlc:next_answers(QC, 1) of
			 [Top1] -> Top1;
			 [] -> undefined;
			 Error -> Error
        end
    end),
    Row.

handle_submit(SubmitData) ->
    [Id, Status, MAC] = string:tokens(SubmitData, "!"),
    Subject = lists:append([Id, "!", Status]),
    MAC = mochihex:to_hex(hmac:hmac256(get_key(), Subject)),
    Object = #hacksense_status{id=Id, timestamp=calendar:local_time(),
                               status=list_to_integer(Status)},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Object) end).

get_key() ->
    FileName = filename:join(code:priv_dir(hacksense), "hacksense.key"),
    {ok, Key} = file:read_file(FileName),
    Key.
