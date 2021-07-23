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
	    {Mark_list, Unmark_list} =
		lists:partition(
		  fun (Each) -> mc_tree:any(Is_important, Each, Tree) end,
		  mc_tree:root_list(Tree)),
	    Marked = mark_conversations(Mark_list, Tree, Messages),
	    Unmarked = unmark_conversations(Unmark_list, Tree, Messages),
	    ?LOG_NOTICE("~B mails marked, ~B mails unmarked", [Marked, Unmarked]),
	    {_Recent_list, Archive_list} =
		lists:partition(
		  fun (Each) -> mc_tree:any(Is_recent, Each, Tree) end,
		  Mark_list),
	    {_Temp_list, Junk_list} =
		lists:partition(
		  fun (Each) -> mc_tree:any(Is_recent, Each, Tree) end,
		  Unmark_list),
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

% the flag replied is used to mark messages for archiving
mark_conversations(List, Tree, Messages) ->
    mc_tree:traverse(
      fun(Docid) ->
	      #{flags := Flags} = maps:get(Docid, Messages),
	      Marked = lists:member(replied, Flags),
	      if Marked -> [];
		 true ->
		      ?LOG_NOTICE("marking mail (~B)", [Docid]),
		      maildir_commander:flag(Docid, "+R")
	      end
      end, List, Tree).

unmark_conversations(List, Tree, Messages) ->
    mc_tree:traverse(
      fun(Docid) ->
	      #{flags := Flags} = maps:get(Docid, Messages),
	      Marked = lists:member(replied, Flags),
	      if Marked ->
		      ?LOG_NOTICE("unmarking mail (~B)", [Docid]),
		      maildir_commander:flag(Docid, "-R");
		 true -> []
	      end
      end, List, Tree).
