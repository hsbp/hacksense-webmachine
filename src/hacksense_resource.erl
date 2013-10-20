-module(hacksense_resource).
-export([init/1, generate_etag/2, content_types_provided/2, is_authorized/2]).
-export([to_html/2, to_csv/2, to_rss/2, to_xml/2, to_txt/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("hacksense_status.hrl").

-define(ISO_DATETIME_FMT, "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B").
-define(CT_CSV, {"text/csv", to_csv}).
-define(CT_RSS, {"application/rss+xml", to_rss}).
-define(CT_XML, {"text/xml", to_xml}).
-define(CT_TXT, {"text/plain", to_txt}).
-define(CT_JSON, {"application/json", to_json}).
-define(CT_HTML, {"text/html", to_html}).
-define(CSV_HEAD, "ID;Timestamp;Status\n").


%% Webmachine Resource functions

init([Model, Format]) -> {ok, {Model, Format, fetch_model_data(Model)}}.

is_authorized(ReqData, {submit, _, _} = State) ->
    {check_submit_hmac(wrq:path_info(data, ReqData)), ReqData, State};
is_authorized(ReqData, State) -> {true, ReqData, State}.

generate_etag(ReqData, {history, _, History} = State) ->
    Digest = base64:encode_to_string(erlsha2:sha256(term_to_binary(History))),
    {Digest, ReqData, State};
generate_etag(ReqData, {status, _, Status} = State) ->
    {Status#hacksense_status.id, ReqData, State};
generate_etag(ReqData, State) -> {undefined, ReqData, State}.

content_types_provided(ReqData, {_, csv, _} = State) -> {[?CT_CSV], ReqData, State};
content_types_provided(ReqData, {_, rss, _} = State) -> {[?CT_RSS], ReqData, State};
content_types_provided(ReqData, {_, xml, _} = State) -> {[?CT_XML], ReqData, State};
content_types_provided(ReqData, {_, txt, _} = State) -> {[?CT_TXT], ReqData, State};
content_types_provided(ReqData, {_, json, _} = State) -> {[?CT_JSON], ReqData, State};
content_types_provided(ReqData, State) ->
    {[?CT_HTML, ?CT_CSV, ?CT_RSS, ?CT_XML, ?CT_TXT, ?CT_JSON], ReqData, State}.


%% HTML

to_html(ReqData, {Model, _, Data} = State) ->
    Module = list_to_atom(atom_to_list(Model) ++ "_dtl"),
    {ok, Content} = Module:render(dtl_params(Model, Data)),
    {Content, ReqData, State}.

dtl_params(home, _) -> [];
dtl_params(history, History) ->
    [{events, [{S#hacksense_status.id, timestamp_to_isofmt(S),
               status_to_open_closed(S)} || S <- History]}];
dtl_params(status, Status) ->
    {OpenClosed, Since} = human_status(Status),
    [{open_closed, OpenClosed}, {since, Since}].


%% JSON

to_json(ReqData, {status, _, Status} = State) ->
    {mochijson2:encode(status_to_json(Status)), ReqData, State};
to_json(ReqData, {history, _, History} = State) ->
    {mochijson2:encode(lists:map(fun status_to_json/1, History)), ReqData, State}.

status_to_json(S) ->
    [{id, list_to_binary(S#hacksense_status.id)},
     {timestamp, iolist_to_binary(timestamp_to_isofmt(S))},
     {status, S#hacksense_status.status == 1}].


%% CSV

to_csv(ReqData, {status, _, Status} = State) ->
    {format_csv(Status), ReqData, State};
to_csv(ReqData, {history, _, History} = State) ->
    {[?CSV_HEAD | lists:map(fun format_csv/1, History)], ReqData, State}.

format_csv(Status) ->
    Since = timestamp_to_isofmt(Status),
    io_lib:format("~s;~s;~B\n",
        [Status#hacksense_status.id, Since, Status#hacksense_status.status]).


%% TXT

to_txt(ReqData, {status, _, Status} = State) ->
    {OpenClosed, Since} = human_status(Status),
    Content = ["H.A.C.K. is currently ", OpenClosed, " since ", Since, "\n"],
    {Content, ReqData, State};
to_txt(ReqData, {submit, _, _} = State) ->
    handle_submit(wrq:path_info(data, ReqData)),
    {"OK\n", ReqData, State}.


%% RSS

to_rss(ReqData, {status, _, Status} = State) ->
    ItemContents = [{title, ["H.A.C.K. has ", status_to_open_closed(Status, "opened", "closed")]},
                   {guid, ["http://vsza.hu/hacksense/state_changes/", Status#hacksense_status.id]},
                   {pubDate, [webmachine_util:rfc1123_date(Status#hacksense_status.timestamp)]}],
    Channel = {channel, [{title, ["State Changes/rss"]},
                         {link, ["http://vsza.hu/hacksense/"]},
                          description,
                         {item, ItemContents}]},
    RSS = xmerl:export_simple([{rss, [{version, "2.0"}], [Channel]}], xmerl_xml),
    {RSS, ReqData, State}.


%% XML

to_xml(ReqData, {status, _, Status} = State) ->
    {format_xml(Status), ReqData, State};
to_xml(ReqData, {history, _, History} = State) ->
    {xml_history(History), ReqData, State}.

xml_history(History) ->
    Children = lists:map(fun status_xml/1, History),
    xmerl:export_simple([{history, Children}], xmerl_xml).

format_xml(Status) ->
    xmerl:export_simple([{status, [status_xml(Status)]}], xmerl_xml).

status_xml(S) ->
    Since = lists:flatten(timestamp_to_isofmt(S)),
    {state_change, [{id, S#hacksense_status.id}, {'when', Since},
                    {what, S#hacksense_status.status}], []}.


%% Common conversion functions

human_status(Status) ->
    OpenClosed = status_to_open_closed(Status),
    Since = timestamp_to_isofmt(Status),
    {OpenClosed, Since}.

status_to_open_closed(Status) ->
    status_to_open_closed(Status, "open", "closed").
status_to_open_closed(Status, Open, Closed) ->
    case Status#hacksense_status.status of
        1 -> Open;
        0 -> Closed
    end.

timestamp_to_isofmt(#hacksense_status{timestamp={{Y, Mo, D}, {H, Mn, S}}}) ->
    io_lib:format(?ISO_DATETIME_FMT, [Y, Mo, D, H, Mn, S]).


%% Data access functions

fetch_model_data(status) ->
    case get_date_ordered_statuses(descending, 1) of
         [Top1] -> Top1;
         [] -> undefined;
         Error -> Error
    end;
fetch_model_data(history) ->
    get_date_ordered_statuses(ascending, all_remaining);
fetch_model_data(_) -> undefined.

get_date_ordered_statuses(OrderDir, Number) ->
    {atomic, Rows} = mnesia:transaction(fun() ->
        TH = mnesia:table(hacksense_status),
        Q = qlc:q([X || X <- TH]),
        QS1 = qlc:keysort(#hacksense_status.id, Q, [{order, ascending}]),
        QS = qlc:keysort(#hacksense_status.timestamp, QS1, [{order, OrderDir}]),
        QC = qlc:cursor(QS),
        qlc:next_answers(QC, Number)
    end),
    Rows.

check_submit_hmac(SubmitData) ->
    [Id, Status, MAC] = string:tokens(SubmitData, "!"),
    Subject = lists:append([Id, "!", Status]),
    case mochihex:to_hex(hmac:hmac256(get_key(), Subject)) of
        MAC -> true;
        _ -> "HackSense submission endpoint"
    end.

handle_submit(SubmitData) ->
    [Id, Status, _MAC] = string:tokens(SubmitData, "!"),
    Object = #hacksense_status{id=Id, timestamp=calendar:local_time(),
                               status=list_to_integer(Status)},
    {atomic, ok} = mnesia:transaction(fun() ->
         case mnesia:read(hacksense_status, Id) of
             [] -> mnesia:write(Object);
             _ -> ok
         end
    end).

get_key() ->
    FileName = filename:join(code:priv_dir(hacksense), "hacksense.key"),
    {ok, Key} = file:read_file(FileName),
    Key.
