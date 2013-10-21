-module(hacksense_home).
-export([init/1]).
-export([to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    {ok, Content} = home_dtl:render([]),
    {Content, ReqData, State}.

