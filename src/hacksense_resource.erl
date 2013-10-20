-module(hacksense_resource).
-export([init/1, to_html/2, generate_etag/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("hacksense_status.hrl").

-define(ISO_DATETIME_FMT, "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B").

init([]) -> {ok, undefined}.

generate_etag(ReqData, State) ->
    {(get_status())#hacksense_status.id, ReqData, State}.

to_html(ReqData, State) ->
    {Body, ContentType} = case wrq:path_info(base, ReqData) of
        "submit" ->
            handle_submit(wrq:disp_path(ReqData)), {"OK", "text/plain"};
        "history.csv" -> {csv_history(), "text/csv"};
        "status.csv" -> {format_csv(get_status()), "text/csv"};
        "status.txt" -> {format_human(txt, get_status()), "text/plain"};
        "status" -> {format_human(html, get_status()), "text/html"};
        A -> {io_lib:format("~p", [A]), "text/plain"} %% XXX debug
    end,
    {Body, wrq:set_resp_header("Content-Type", ContentType, ReqData), State}.

csv_history() ->
    lists:map(fun format_csv/1, get_date_ordered_statuses(ascending, all_remaining)).

format_human(Format, Status) ->
    OpenClosed = status_to_open_closed(Status),
    Since = timestamp_to_isofmt(Status#hacksense_status.timestamp),
    format_human(Format, OpenClosed, Since).

format_human(html, OpenClosed, Since) ->
    {ok, Content} = status_html_dtl:render(
                      [{open_closed, OpenClosed}, {since, Since}]),
    Content;
format_human(txt, OpenClosed, Since) ->
    ["H.A.C.K. is currently ", OpenClosed, " since ", Since, "\n"].

format_csv(Status) ->
    Since = timestamp_to_isofmt(Status#hacksense_status.timestamp),
    io_lib:format("~s;~s;~B\n",
        [Status#hacksense_status.id, Since, Status#hacksense_status.status]).

status_to_open_closed(Status) ->
    case Status#hacksense_status.status of
        1 -> "open";
        0 -> "closed"
    end.

timestamp_to_isofmt({{Y, Mo, D}, {H, Mn, S}}) ->
    io_lib:format(?ISO_DATETIME_FMT, [Y, Mo, D, H, Mn, S]).

get_status() ->
    case get_date_ordered_statuses(descending, 1) of
         [Top1] -> Top1;
         [] -> undefined;
         Error -> Error
    end.

get_date_ordered_statuses(OrderDir, Number) ->
    {atomic, Rows} = mnesia:transaction(fun() ->
        TH = mnesia:table(hacksense_status),
        Q = qlc:q([X || X <- TH]),
        QS = qlc:keysort(#hacksense_status.timestamp, Q, [{order, OrderDir}]),
        QC = qlc:cursor(QS),
        qlc:next_answers(QC, Number)
    end),
    Rows.

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
