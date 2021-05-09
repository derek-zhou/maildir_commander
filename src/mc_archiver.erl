-module(mc_archiver).
%% this module handle automatic archiving of mails from Inbox
%% I have no state, just to make achiving serialized

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% apis
-export([start_link/0, trigger/0, run/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2]).

%% the state record to hold extended
-record(state, { client }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

trigger() -> gen_server:cast(?MODULE, run).

run() -> gen_server:call(?MODULE, run).

%% server side api

init([]) -> {ok, #state{}}.

handle_cast(run, State) ->
    {noreply, run_archiving(State), hibernate}.

handle_continue(run, State) ->
    {noreply, run_archiving(State), hibernate}.

handle_call(ping, _From, State) -> {reply, ok, State};

handle_call(run, {Client, _Tag}, State) ->
    {reply, ok, State#state{client = Client}, {continue, run}}.

run_archiving(State = #state{client = Client}) ->
    ?LOG_NOTICE("Starting archiving run"),
    case maildir_commander:find("maildir:/", true, ':date', false, false, false) of
	{error, Reason} ->
	    ?LOG_WARNING("query error: ~ts", [Reason]),
	    State#state{client = undefined};
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
	    notice(io_lib:format("~B conversations to be archived, ~B conversation to be deleted",
				 [length(Archive_list), length(Junk_list)]), Client),
	    Archived = archive_conversations(Archive_list, Archive, Tree, Messages, Client),
	    Deleted = delete_conversations(Junk_list, Tree, Messages, Client),
	    notice(io_lib:format("Done archiving, ~B mails archived, ~B mails deleted",
				 [Archived, Deleted]), Client),
	    State#state{client = undefined}
    end.

is_recent(#{date := Date}, Horizon) when Date > Horizon -> true;
is_recent(#{flags := Flags}, _Horizon) -> lists:member(unread, Flags).

is_important(#{from := [ _Name | Addr ]}, My_addresses) ->
    sets:is_element(binary_to_list(Addr), My_addresses).

delete_conversations(List, Tree, Messages, Client) ->
    mc_tree:traverse(
      fun(Docid) ->
	      #{path := Path} = maps:get(Docid, Messages),
	      notice(io_lib:format("deleting mail (~B) ~ts", [Docid, Path]), Client),
	      maildir_commander:delete(Docid)
      end, List, Tree).

archive_conversations(List, Archive, Tree, Messages, Client) ->
    mc_tree:traverse(
      fun(Docid) ->
	      #{path := Path} = maps:get(Docid, Messages),
	      notice(io_lib:format("archiving mail (~B) ~ts", [Docid, Path]), Client),
	      maildir_commander:scrub(Path, Archive)
      end, List, Tree).

notice(Msg, undefined) -> ?LOG_NOTICE(Msg);
notice(Msg, Client) ->
    ?LOG_NOTICE(Msg),
    Client ! {async, Msg}.
