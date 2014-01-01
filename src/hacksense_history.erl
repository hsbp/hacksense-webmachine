-module(hacksense_history).
-export([init/1, generate_etag/2, content_types_provided/2]).
-export([to_html/2, to_csv/2, to_xml/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("hacksense_status.hrl").

-define(CT_CSV, {"text/csv", to_csv}).
-define(CT_XML, {"text/xml", to_xml}).
-define(CT_JSON, {"application/json", to_json}).
-define(CT_HTML, {"text/html", to_html}).
-define(CSV_HEAD, "ID;Timestamp;Status\n").


%% Webmachine Resource functions

init([Format]) ->
    {atomic, History} = mnesia:transaction(fun() ->
       qlc:e(qlc:q([hacksense_status:item_from_db(R) || R <- mnesia:table(hacksense_status)]))
    end),
    {ok, {Format, History}}.

generate_etag(ReqData, {_, History} = State) ->
    LastId = (lists:last(History))#status.id,
    Digest = io_lib:format("~B-~s", [erlang:phash2(History), LastId]),
    {Digest, ReqData, State}.

content_types_provided(ReqData, {csv, _} = State) -> {[?CT_CSV], ReqData, State};
content_types_provided(ReqData, {xml, _} = State) -> {[?CT_XML], ReqData, State};
content_types_provided(ReqData, {json, _} = State) -> {[?CT_JSON], ReqData, State};
content_types_provided(ReqData, State) ->
    {[?CT_HTML, ?CT_CSV, ?CT_XML, ?CT_JSON], ReqData, State}.


%% HTML

to_html(ReqData, {_, History} = State) ->
    {ok, Content} = history_dtl:render([{events, [
        {S#status.id, S#status.timestamp, hacksense_status:open_closed(S)} || S <- History]}]),
    {Content, ReqData, State}.


%% JSON

to_json(ReqData, {_, History} = State) ->
    {mochijson2:encode(lists:map(fun hacksense_status:item_to_json/1, History)), ReqData, State}.


%% CSV

to_csv(ReqData, {_, History} = State) ->
    {[?CSV_HEAD | lists:map(fun hacksense_status:item_to_csv/1, History)], ReqData, State}.


%% XML

to_xml(ReqData, {_, History} = State) ->
    Children = lists:map(fun hacksense_status:item_to_xml/1, History),
    XML = xmerl:export_simple([{history, Children}], xmerl_xml),
    {XML, ReqData, State}.
