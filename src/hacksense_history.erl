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

init([Format]) -> {ok, {Format, fetch_model_data()}}.

generate_etag(ReqData, {_, History} = State) ->
    Digest = base64:encode_to_string(erlsha2:sha256(term_to_binary(History))),
    {Digest, ReqData, State}.

content_types_provided(ReqData, {csv, _} = State) -> {[?CT_CSV], ReqData, State};
content_types_provided(ReqData, {xml, _} = State) -> {[?CT_XML], ReqData, State};
content_types_provided(ReqData, {json, _} = State) -> {[?CT_JSON], ReqData, State};
content_types_provided(ReqData, State) ->
    {[?CT_HTML, ?CT_CSV, ?CT_XML, ?CT_JSON], ReqData, State}.


%% HTML

to_html(ReqData, {_, History} = State) ->
    {ok, Content} = history_dtl:render([{events, [
        {S#status.id, S#status.timestamp, hacksense_status:status_to_open_closed(S)} || S <- History]}]),
    {Content, ReqData, State}.


%% JSON

to_json(ReqData, {_, History} = State) ->
    {mochijson2:encode(lists:map(fun hacksense_status:status_to_json/1, History)), ReqData, State}.


%% CSV

to_csv(ReqData, {_, History} = State) ->
    {[?CSV_HEAD | lists:map(fun hacksense_status:format_csv/1, History)], ReqData, State}.


%% XML

to_xml(ReqData, {_, History} = State) ->
    Children = lists:map(fun hacksense_status:status_xml/1, History),
    XML = xmerl:export_simple([{history, Children}], xmerl_xml),
    {XML, ReqData, State}.


%% Data access functions

fetch_model_data() ->
    {atomic, History} = mnesia:transaction(fun() ->
       qlc:e(qlc:q([hacksense_status:status_to_hacksense_status(R) || R <- mnesia:table(hacksense_status)]))
    end),
    History.
