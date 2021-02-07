%%%-------------------------------------------------------------------
%% @doc maildir_commander top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(maildir_commander_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [self_configer:child_spec([{name, mc_configer}]),
		  #{id => mc_server,
		    start => {mc_server, start_link, []},
		    type => worker},
		  #{id => mc_watcher,
		    start => {mc_watcher, start_link, []},
		    type => worker},
		  #{id => mc_local_server,
		    start => {mc_local_server, start_link, []},
		    type => worker}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
