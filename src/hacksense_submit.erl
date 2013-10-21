-module(hacksense_submit).
-export([init/1, content_types_provided/2, is_authorized/2]).
-export([to_txt/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("hacksense_status.hrl").

-define(ISO_DATETIME_FMT, "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B").
-define(CT_TXT, {"text/plain", to_txt}).

init([]) -> {ok, undefined}.

is_authorized(ReqData, State) ->
    case check_submit_hmac(list_to_binary(wrq:path_info(data, ReqData))) of
        wrong_mac -> {"HackSense submission endpoint", ReqData, State};
        Submission -> {true, ReqData, Submission}
    end.

content_types_provided(ReqData, State) -> {[?CT_TXT], ReqData, State}.

to_txt(ReqData, State) ->
    handle_submit(State),
    {"OK\n", ReqData, State}.

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

timestamp_to_isofmt({{Y, Mo, D}, {H, Mn, S}}) ->
    io_lib:format(?ISO_DATETIME_FMT, [Y, Mo, D, H, Mn, S]).

get_key() ->
    FileName = filename:join(code:priv_dir(hacksense), "hacksense.key"),
    {ok, Key} = file:read_file(FileName),
    Key.
