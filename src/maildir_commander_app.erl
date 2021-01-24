%%%-------------------------------------------------------------------
%% @doc maildir commander public API
%% @end
%%%-------------------------------------------------------------------

-module(maildir_commander_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    maildir_commander_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
