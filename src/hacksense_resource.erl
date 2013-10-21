-module(hacksense_resource).
-export([init/1, generate_etag/2, content_types_provided/2, is_authorized/2]).
-export([to_html/2, to_csv/2, to_rss/2, to_xml/2, to_txt/2, to_json/2, to_eeml/2]).

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
-define(CT_EEML, {"text/xml", to_eeml}).
-define(CSV_HEAD, "ID;Timestamp;Status\n").


%% Webmachine Resource functions

init([Model, Format]) -> {ok, {Model, Format, fetch_model_data(Model)}}.

is_authorized(ReqData, {submit, _, _} = State) ->
	case check_submit_hmac(list_to_binary(wrq:path_info(data, ReqData))) of
		wrong_mac -> {"HackSense submission endpoint", ReqData, State};
		Submission -> {true, ReqData, setelement(3, State, Submission)}
	end;
is_authorized(ReqData, State) -> {true, ReqData, State}.

generate_etag(ReqData, {history, _, History} = State) ->
    Digest = base64:encode_to_string(erlsha2:sha256(term_to_binary(History))),
    {Digest, ReqData, State};
generate_etag(ReqData, {status, _, Status} = State) ->
    {binary_to_list(Status#status.id), ReqData, State};
generate_etag(ReqData, State) -> {undefined, ReqData, State}.

content_types_provided(ReqData, {_, csv, _} = State) -> {[?CT_CSV], ReqData, State};
content_types_provided(ReqData, {_, rss, _} = State) -> {[?CT_RSS], ReqData, State};
content_types_provided(ReqData, {_, xml, _} = State) -> {[?CT_XML], ReqData, State};
content_types_provided(ReqData, {_, txt, _} = State) -> {[?CT_TXT], ReqData, State};
content_types_provided(ReqData, {_, json, _} = State) -> {[?CT_JSON], ReqData, State};
content_types_provided(ReqData, {_, eeml, _} = State) -> {[?CT_EEML], ReqData, State};
content_types_provided(ReqData, State) ->
    {[?CT_HTML, ?CT_CSV, ?CT_RSS, ?CT_XML, ?CT_TXT, ?CT_JSON], ReqData, State}.


%% HTML

to_html(ReqData, {Model, _, Data} = State) ->
    Module = list_to_atom(atom_to_list(Model) ++ "_dtl"),
    {ok, Content} = Module:render(dtl_params(Model, Data)),
    {Content, ReqData, State}.

dtl_params(home, _) -> [];
dtl_params(history, History) ->
    [{events, [{S#status.id, S#status.timestamp,
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
    [{id, S#status.id},
     {timestamp, S#status.timestamp},
     {status, S#status.status == <<$1>>}].


%% CSV

to_csv(ReqData, {status, _, Status} = State) ->
    {format_csv(Status), ReqData, State};
to_csv(ReqData, {history, _, History} = State) ->
    {[?CSV_HEAD | lists:map(fun format_csv/1, History)], ReqData, State}.

format_csv(Status) ->
    [Status#status.id, $;, Status#status.timestamp, $;,
     Status#status.status, $\n].


%% TXT

to_txt(ReqData, {status, _, Status} = State) ->
    {OpenClosed, Since} = human_status(Status),
    Content = ["H.A.C.K. is currently ", OpenClosed, " since ", Since, "\n"],
    {Content, ReqData, State};
to_txt(ReqData, {submit, _, Submission} = State) ->
    handle_submit(Submission),
    {"OK\n", ReqData, State}.


%% RSS

to_rss(ReqData, {status, _, Status} = State) ->
    <<Y:4/binary, $-, Mo:2/binary, $-, D:2/binary, $\x20,
      H:2/binary, $:, Mi:2/binary, $:, S:2/binary>> = Status#status.timestamp,
    Date = {binary_to_integer(Y), binary_to_integer(Mo), binary_to_integer(D)},
    Time = {binary_to_integer(H), binary_to_integer(Mi), binary_to_integer(S)},
    ItemContents = [{title, ["H.A.C.K. has ", status_to_open_closed(Status, "opened", "closed")]},
                   {guid, ["http://vsza.hu/hacksense/state_changes/", [Status#status.id]]},
                   {pubDate, [webmachine_util:rfc1123_date({Date, Time})]}],
    Channel = {channel, [{title, ["State Changes/rss"]},
                         {link, ["http://vsza.hu/hacksense/"]},
                          description,
                         {item, ItemContents}]},
    RSS = xmerl:export_simple([{rss, [{version, "2.0"}], [Channel]}], xmerl_xml),
    {RSS, ReqData, State}.


%% EEML

to_eeml(ReqData, {status, _, S} = State) ->
    Location = {location, [{exposure, "indoor"}, {domain, "physical"}, {disposition, "fixed"}],
                [{name, ["Hackerspace BP"]}, {lat, ["47.489196"]}, {lon, ["19.059512"]}, {ele, ["117"]}]},
    Value = {value, [{minValue, "0.0"}, {maxValue, "1.0"}], [[S#status.status]]},
    Data = {data, [{id, 0}], [{tag, ["status code"]}, {tag, ["hackerspace opening"]}, Value]},
    Env = {environment, [{updated, binary:replace(S#status.timestamp, <<" ">>, <<$T>>)}],
           [{title, ["Hacksense Budapest"]}, {feed, ["http://vsza.hu/hacksense/eeml_status.xml"]},
            {status, ["live"]}, {description, ["Hacksense Hackerspace Budapest"]},
            {icon, ["http://www.hsbp.org/icon.png"]}, {website, ["http://www.hsbp.org/"]},
            Location, Data]},
    XML = xmerl:export_simple([{eeml, [
        {xmlns, "http://www.eeml.org/xsd/005"},
        {'xmlns:xsi', "http://www.w3.org/2001/XMLSchema-instance"},
        {'xsi:schemaLocation',
         "http://www.eeml.org/xsd/005 http://www.eeml.org/xsd/005/005.xsd"}
        ], [Env]}], xmerl_xml),
    {XML, ReqData, State}.


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
    {state_change, [{id, S#status.id}, {'when', S#status.timestamp},
                    {what, S#status.status}], []}.


%% Common conversion functions

human_status(Status) ->
    OpenClosed = status_to_open_closed(Status),
    Since = Status#status.timestamp,
    {OpenClosed, Since}.

status_to_open_closed(Status) ->
    status_to_open_closed(Status, "open", "closed").
status_to_open_closed(Status, Open, Closed) ->
    case Status#status.status of
        <<$1>> -> Open;
        <<$0>> -> Closed
    end.

timestamp_to_isofmt({{Y, Mo, D}, {H, Mn, S}}) ->
    io_lib:format(?ISO_DATETIME_FMT, [Y, Mo, D, H, Mn, S]).


%% Data access functions

fetch_model_data(status) ->
    {atomic, [Status]} =
        mnesia:transaction(fun() ->
            mnesia:read(hacksense_status, mnesia:last(hacksense_status))
        end),
    status_to_hacksense_status(Status);
fetch_model_data(history) ->
    {atomic, History} = mnesia:transaction(fun() ->
        mnesia:foldr(fun(R, A) -> [status_to_hacksense_status(R) | A] end, [], hacksense_status)
    end),
    History;
fetch_model_data(_) -> undefined.

status_to_hacksense_status(#hacksense_status{timestamp_id={TS, Id}, status=S}) ->
    #status{id=Id, timestamp=TS, status=S}.

check_submit_hmac(SubmitData) ->
    [Id, Status, MAC] = binary:split(SubmitData, <<$!>>, [global]),
    Subject = <<Id/binary, $!, Status/binary>>,
    case list_to_binary(mochihex:to_hex(hmac:hmac256(get_key(), Subject))) of
        MAC -> {Id, Status};
        _ -> wrong_mac
    end.

handle_submit({Id, Status}) ->
	IsoDateTime = list_to_binary(timestamp_to_isofmt(calendar:local_time())),
    Object = #hacksense_status{timestamp_id={IsoDateTime, Id}, status=Status},
    {atomic, ok} = mnesia:transaction(fun() ->
         case mnesia:match_object(#hacksense_status{timestamp_id={'_', Id}, status='_'}) of
             [] -> mnesia:write(Object);
             _ -> ok
         end
    end).

get_key() ->
    FileName = filename:join(code:priv_dir(hacksense), "hacksense.key"),
    {ok, Key} = file:read_file(FileName),
    Key.
