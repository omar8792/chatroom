%%%-------------------------------------------------------------------
%% @doc chatroom public API
%% @end
%%%-------------------------------------------------------------------

-module(chatroom_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    entrypoint:start().

stop(_State) ->
    ok.

%% internal functions
