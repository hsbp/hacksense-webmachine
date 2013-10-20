%% @doc hacksense startup code

-module(hacksense).
-export([start/0, start_link/0, stop/0, import_remote_csv/1]).

-include_lib("hacksense_status.hrl").

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    mnesia:create_schema([node()]),
    ensure_started(mnesia),
    init_schema(),
    ensure_started(erlsha2),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    hacksense_sup:start_link().

%% @spec start() -> ok
%% @doc Start the hacksense server.
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    mnesia:create_schema([node()]),
    ensure_started(mnesia),
    init_schema(),
    ensure_started(erlsha2),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(hacksense).

init_schema() ->
    case mnesia:create_table(hacksense_status, [
        {attributes, record_info(fields, hacksense_status)}, {disc_copies, [node()]}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, hacksense_status}} -> ok
    end,
    ok = mnesia:wait_for_tables([hacksense_status], 5000).

import_remote_csv(URL) ->
    {ok, {_, _, CSV}} = httpc:request(get, {URL, []}, [], [{body_format, binary}]),
    [_Header | Lines] = binary:split(CSV, <<$\n>>, [global, trim]),
    {atomic, ok} = mnesia:transaction(fun () ->
        lists:foreach(fun import_csv_line/1, Lines) end).

import_csv_line(
    <<Id:36/binary, $;, Y:4/binary, $-, Mo:2/binary, $-, D:2/binary, $\x20,
      H:2/binary, $:, Mi:2/binary, $:, S:2/binary, $;, Status:1/binary>>) ->
    Date = {binary_to_integer(Y), binary_to_integer(Mo), binary_to_integer(D)},
    Time = {binary_to_integer(H), binary_to_integer(Mi), binary_to_integer(S)},
    Object = #hacksense_status{id=binary_to_list(Id), timestamp={Date, Time},
                               status=binary_to_integer(Status)},
    mnesia:write(Object).

%% @spec stop() -> ok
%% @doc Stop the hacksense server.
stop() ->
    Res = application:stop(hacksense),
    application:stop(webmachine),
    application:stop(mnesia),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.
