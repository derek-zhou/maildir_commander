%% The module gate keep the pop activities for one User/Host combo
%%
-module(mc_pop_manager).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% apis
-export([start_link/0]).
-export([pop_all/3, list/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% from user
-spec pop_all(binary(), binary(), binary()) -> ok.
pop_all(User, Pass, Host) ->
    gen_server:cast(?MODULE, {pop_all, User, Pass, Host}).

-spec list() -> list().
list() ->
    gen_server:call(?MODULE, list).

%% server side api

init([]) ->
    process_flag(trap_exit, true),
    {ok, #{}}.

handle_call(list, _From, State) ->
    {reply, maps:keys(State), State}.

handle_cast({pop_all, User, Pass, Host}, State) ->
    case maps:get({User, Host}, State, undefined) of
	undefined ->
	    Pid = spawn_link(mc_popper, pop_all,
			     [ unicode:characters_to_list(User),
			       unicode:characters_to_list(Pass),
			       unicode:characters_to_list(Host) ]),
	    {noreply, State#{{User, Host} => Pid}};
	Pid ->
	    ?LOG_DEBUG("POP of ~ts@~ts is still going on: ~w", [User, Host, Pid]),
	    {noreply, State}
    end.

handle_info({'EXIT', Pid, normal}, State) ->
    Key = find_key(Pid, State),
    {noreply, maps:remove(Key, State)};
handle_info({'EXIT', Pid, Reason}, State) ->
    Key = {User, Host} = find_key(Pid, State),
    ?LOG_WARNING("POP of ~ts@~ts crashed: ~w", [User, Host, Reason]),
    {noreply, maps:remove(Key, State)}.

find_key(Value, Map) -> find_key(Value, maps:keys(Map), Map).

find_key(_Value, [], _Map) -> undefined;
find_key(Value, [Head | Tail], Map) ->
    case maps:get(Head, Map, undefined) of
	Value -> Head;
	_ ->  find_key(Value, Tail, Map)
    end.
