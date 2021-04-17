-module(mc_archiver).
%% this module handle automatic archiving of mails from Inbox
%% I have no state, just to make achiving serialized

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% apis
-export([start_link/0, trigger/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

trigger() -> gen_server:cast(?MODULE, run).

%% server side api

init([]) -> {ok, ok}.

handle_cast(run, State) -> {noreply, run_archiving(State), hibernate}.

handle_call(ping, _From, State) -> {reply, ok, State}.

run_archiving(_State) ->
    ?LOG_NOTICE("Starting archiving run"),
    case maildir_commander:find("maildir:/", true, ':date', false, false, false) of
	{error, Reason} ->
	    ?LOG_WARNING("query error: ~ts", [Reason]),
	    ok;
	{ok, Tree, Messages} ->
	    Horizon = erlang:system_time(seconds) -
		mc_configer:default(archive_days) * 86400,
	    Archive = mc_configer:default(archive_maildir),
	    My_addresses = sets:from_list(mc_configer:default(my_addresses)),
	    Is_recent =
		fun(Docid) ->
			is_recent(maps:get(Docid, Messages), Horizon)
		end,
	    Is_important =
		fun(Docid) ->
			is_important(maps:get(Docid, Messages), My_addresses)
		end,
	    {_Recent_list, Expired_list} =
		lists:partition(
		  fun (Each) -> mc_tree:any(Is_recent, Each, Tree) end,
		  mc_tree:root_list(Tree)),
	    {Archive_list, Junk_list} =
		lists:partition(
		  fun (Each) -> mc_tree:any(Is_important, Each, Tree) end,
		  Expired_list),
	    ?LOG_NOTICE("Finishing archiving run, ~B deleted, ~B archived",
		       [ delete_conversations(Junk_list, Tree, Messages),
			 archive_conversations(Archive_list, Archive,
					       Tree, Messages) ]),
	    ok
    end.

is_recent(#{date := Date}, Horizon) when Date > Horizon -> true;
is_recent(#{flags := Flags}, _Horizon) -> lists:member(unread, Flags).

is_important(#{from := [ _Name | Addr ]}, My_addresses) ->
    sets:is_element(binary_to_list(Addr), My_addresses).

delete_conversations(List, Tree, Messages) ->
    mc_tree:traverse(
      fun(Docid) ->
	      #{path := Path} = maps:get(Docid, Messages),
	      ?LOG_NOTICE("deleting mail (~B) ~ts", [Docid, Path]),
	      maildir_commander:delete(Docid)
      end, List, Tree).

archive_conversations(List, Archive, Tree, Messages) ->
    mc_tree:traverse(
      fun(Docid) ->
	      #{path := Path} = maps:get(Docid, Messages),
	      ?LOG_NOTICE("archiving mail (~B) ~ts", [Docid, Path]),
	      maildir_commander:scrub(Path, Archive)
      end, List, Tree).
