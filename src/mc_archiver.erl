-module(mc_archiver).
%% this module handle automatic archiving of mails from Inbox

-include_lib("kernel/include/logger.hrl").

%% apis
-export([init/0]).

%% server side api

init() ->
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
	    ?LOG_NOTICE("~B conversations to be archived, ~B conversation to be deleted",
			[length(Archive_list), length(Junk_list)]),
	    Archived = archive_conversations(Archive_list, Archive, Tree, Messages),
	    Deleted = delete_conversations(Junk_list, Tree, Messages),
	    ?LOG_NOTICE("Done archiving, ~B mails archived, ~B mails deleted",
			[Archived, Deleted]),
	    ok
    end.

is_recent(#{date := Date}, Horizon) when Date > Horizon -> true;
is_recent(#{flags := Flags}, _Horizon) -> lists:member(unread, Flags).

is_important(#{from := [ _Name | Addr ]}, My_addresses) ->
    sets:is_element(Addr, My_addresses).

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
