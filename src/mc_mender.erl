-module(mc_mender).

%% this module handle direct maildir accessing

-include_lib("kernel/include/logger.hrl").

-export([mend/2, deep_mend/2, scrub_part/1, set_parent/2, maildir_path/2]).

%% mend an email from Path by chenging its contents using the closure How,
-spec mend(function(), string()) -> ok | {error, term()}.
mend(How, Path) ->
    case mailfile:read_mail(Path) of
	{error, Reason} -> {error, Reason};
	{ok, [Head | Tail]} -> maildir_commit([How(Head) | Tail], Path)
    end.

%% mend an email by applying How to every node
-spec deep_mend(function(), string()) -> ok | {error, term()}.
deep_mend(How, Path) ->
    case mailfile:read_mail(Path) of
	{error, Reason} -> {error, Reason};
	{ok, Parts} -> maildir_commit(lists:map(How, Parts), Path)
    end.

scrub_part(Part = {Level, Headers, Parameters, _Body}) ->
    case keep_part(Parameters) of
	true -> Part;
	false -> {Level, Headers, Parameters#{body => <<>>}, <<>>}
    end.

keep_part(#{content_type := <<"text/", _Rest/binary>>}) -> true;
keep_part(#{content_type := <<"multipart/", _Rest/binary>>}) -> true;
keep_part(#{disposition := <<"inline">>}) -> false;
keep_part(#{disposition := <<"attachment">>}) -> false;
keep_part(_) -> true.

%% return the full path from mailpath
-spec maildir_path(string(), atom()) -> string().
maildir_path(Maildir, Type) ->
    maildir_path(Maildir, Type, "").

-spec maildir_path(string(), atom(), string()) -> string().
maildir_path(Maildir, Type, Basename)
  when Type == cur orelse Type == tmp orelse Type == new ->
    lists:flatten([mc_configer:default(index_path),
		   case Maildir of
		       "/" -> "";
		       String -> [$/ | String]
		   end,
		   $/,
		   atom_to_list(Type),
		   $/,
		   Basename]).

%% private functions
maildir_commit(Parts, Original) ->
    Basename = filename:basename(Original),
    Tmp = maildir_path("/", tmp, Basename),
    case mailfile:write_mail(Parts, Tmp) of
	{error, Reason} -> {error, Reason};
	ok -> file:rename(Tmp, Original)
    end.

set_parent({Level, Headers, Parameters, Body}, Pid) ->
    {Level, set_parent([], false, Headers, Pid), Parameters, Body}.

set_parent(List, true, [], _Pid) ->
    lists:reverse(List);
set_parent(List, false, [], undefined) ->
    lists:reverse(List);
set_parent(List, false, [], Pid) ->
    lists:reverse([{<<"In-Reply-To">>, Pid} | List]);
set_parent(List, Found, [{<<"In-Reply-To">>, _} | Tail], undefined) ->
    set_parent(List, Found, Tail, undefined);
set_parent(List, true, [{<<"In-Reply-To">>, _} | Tail], Pid) ->
    set_parent(List, true, Tail, Pid);
set_parent(List, false, [{<<"In-Reply-To">>, _} | Tail], Pid) ->
    set_parent([{<<"In-Reply-To">>, Pid} | List], true, Tail, Pid);
set_parent(List, Found, [{<<"References">>, _} | Tail], Pid) ->
    set_parent(List, Found, Tail, Pid);
set_parent(List, Found, [Head | Tail], Pid) ->
    set_parent([Head | List], Found, Tail, Pid).

