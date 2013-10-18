%% @doc Callbacks for the hacksense application.

-module(hacksense_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for hacksense.
start(_Type, _StartArgs) ->
    hacksense_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for hacksense.
stop(_State) ->
    ok.
