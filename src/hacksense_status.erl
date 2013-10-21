-module(hacksense_status).
-export([init/1, generate_etag/2, content_types_provided/2]).
-export([to_html/2, to_csv/2, to_rss/2, to_xml/2, to_txt/2, to_json/2, to_eeml/2]).
-export([status_to_json/1, format_csv/1, status_xml/1, status_to_hacksense_status/1, human_status/1, status_to_open_closed/1]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("hacksense_status.hrl").

-define(CT_CSV, {"text/csv", to_csv}).
-define(CT_RSS, {"application/rss+xml", to_rss}).
-define(CT_XML, {"text/xml", to_xml}).
-define(CT_TXT, {"text/plain", to_txt}).
-define(CT_JSON, {"application/json", to_json}).
-define(CT_HTML, {"text/html", to_html}).
-define(CT_EEML, {"text/xml", to_eeml}).


%% Webmachine Resource functions

init([Format]) -> {ok, {Format, fetch_model_data()}}.

generate_etag(ReqData, {_, Status} = State) ->
    {binary_to_list(Status#status.id), ReqData, State}.

content_types_provided(ReqData, {csv, _} = State) -> {[?CT_CSV], ReqData, State};
content_types_provided(ReqData, {rss, _} = State) -> {[?CT_RSS], ReqData, State};
content_types_provided(ReqData, {xml, _} = State) -> {[?CT_XML], ReqData, State};
content_types_provided(ReqData, {txt, _} = State) -> {[?CT_TXT], ReqData, State};
content_types_provided(ReqData, {json, _} = State) -> {[?CT_JSON], ReqData, State};
content_types_provided(ReqData, {eeml, _} = State) -> {[?CT_EEML], ReqData, State};
content_types_provided(ReqData, State) ->
    {[?CT_HTML, ?CT_CSV, ?CT_RSS, ?CT_XML, ?CT_TXT, ?CT_JSON], ReqData, State}.


%% HTML

to_html(ReqData, {_, Status} = State) ->
    {OpenClosed, Since} = human_status(Status),
    {ok, Content} = status_dtl:render([{open_closed, OpenClosed}, {since, Since}]),
    {Content, ReqData, State}.


%% JSON

to_json(ReqData, {status, Status} = State) ->
    {mochijson2:encode(status_to_json(Status)), ReqData, State}.

status_to_json(S) ->
    [{id, S#status.id},
     {timestamp, S#status.timestamp},
     {status, S#status.status == <<$1>>}].


%% CSV

to_csv(ReqData, {_, Status} = State) ->
    {format_csv(Status), ReqData, State}.

format_csv(Status) ->
    [Status#status.id, $;, Status#status.timestamp, $;,
     Status#status.status, $\n].


%% TXT

to_txt(ReqData, {_, Status} = State) ->
    {OpenClosed, Since} = human_status(Status),
    Content = ["H.A.C.K. is currently ", OpenClosed, " since ", Since, "\n"],
    {Content, ReqData, State}.


%% RSS

to_rss(ReqData, {_, Status} = State) ->
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

to_eeml(ReqData, {_, S} = State) ->
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

to_xml(ReqData, {_, Status} = State) ->
    XML = xmerl:export_simple([{status, [status_xml(Status)]}], xmerl_xml),
    {XML, ReqData, State}.

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


%% Data access functions

fetch_model_data() ->
    {atomic, [Status]} =
        mnesia:transaction(fun() ->
            mnesia:read(hacksense_status, mnesia:last(hacksense_status))
        end),
    status_to_hacksense_status(Status).

status_to_hacksense_status(#hacksense_status{timestamp_id={TS, Id}, status=S}) ->
    #status{id=Id, timestamp=TS, status=S}.