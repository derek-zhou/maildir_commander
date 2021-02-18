-module(mc_mender).

-include_lib("kernel/include/logger.hrl").

-export([mend/2, mend/3, leaf_mend/2, leaf_mend/3]).
-export([scrub_mime/1, set_mime_parent/2]).
-export([maildir_path/2, maildir_path/3, maildir_parse/1]).

%% mend an email from Path by chenging its contents using the closure How,
%% then put it into Maildir

-spec mend(function(), string()) -> {ok, integer()} | {error, binary()}.
mend(How, Path) ->
    case maildir_parse(Path) of
	{error, Reason} -> {error, Reason};
	{Maildir, cur, _Basename} -> mend(How, Path, Maildir);
	{_Maildir, _Type, _Basename} -> {error, "Not in cur maildir"}
    end.

-spec mend(function(), string(), string()) -> {ok, integer()} | {error, binary()}.
mend(How, Path, Maildir) ->
    case file:read_file(Path) of
	{error, Reason} -> {error, Reason};
	{ok, Binary} ->
	    Mime = mimemail:decode(Binary),
	    Mended_mime = How(Mime),
	    Mended = mimemail:encode(Mended_mime),
	    case maildir_commit(Mended, Maildir, filename:basename(Path), Path) of
		{error, Reason} -> {error, Reason};
		{ok, New_path} -> maildir_commander:add(New_path)
	    end
    end.

%% mend only the leaf mime parts

-spec leaf_mend(function(), string()) -> {ok, integer()} | {error, binary()}.
leaf_mend(How, Path) ->
    mend(fun(Mime) -> leaf_mime_mend(How, Mime) end, Path).

-spec leaf_mend(function(), string(), string()) -> {ok, integer()} | {error, binary()}.
leaf_mend(How, Path, Maildir) ->
    mend(fun(Mime) -> leaf_mime_mend(How, Mime) end, Path, Maildir).

%% all text shall be kept
scrub_mime({<<"text">>, SubType, Headers, Parameters, Body}) ->
    {<<"text">>, SubType, Headers, Parameters, Body};
%% all inline or attachments shall be removed
scrub_mime({Type, SubType, Headers, Parameters = #{disposition := <<"inline">>}, _Body}) ->
    {Type, SubType, Headers, Parameters, <<>>};
scrub_mime({Type, SubType, Headers, Parameters = #{disposition := <<"attachment">>}, _Body}) ->
    {Type, SubType, Headers, Parameters, <<>>};
%% everything fell through shall be kept
scrub_mime(Mime) -> Mime.

%% return the full path from mailpath
-spec maildir_path(string(), atom()) -> string().
maildir_path(Maildir, Type) ->
    maildir_path(Maildir, Type, "").

-spec maildir_path(string(), atom(), string()) -> string().
maildir_path(Maildir, Type, Basename)
  when Type == cur orelse Type == tmp orelse Type == new ->
    lists:flatten([default(index_path),
		   case Maildir of
		       "/" -> "";
		       String -> [$/ | String]
		   end,
		   $/,
		   atom_to_list(Type),
		   $/,
		   Basename]).

%% infer maildir, type and basename from full path
-spec maildir_parse(string()) -> {error, binary()} | {string(), atom(), string()}.
maildir_parse(Path) ->
    Dir = default(index_path),
    case string:prefix(Path, Dir) of
	nomatch -> {error, <<"Not under root Maildir">>};
	Remain ->
	    case re:split(Remain, "(/cur/|/new/|/tmp/)") of
		[Maildir, Type, Basename] ->
		    case filename:basename(Basename) of
			Basename ->
			    {
			     case Maildir of
				 <<>> -> "/";
				 _ -> unicode:characters_to_list(Maildir)
			     end,
			     maildir_type(Type),
			     unicode:characters_to_list(Basename)};
			_ -> {error, <<"Not valid Maildir path">>}
		    end;
		_ -> {error, <<"Not in proper Maildir branch">>}
	    end
    end.

maildir_type(<<"/cur/">>) -> cur;
maildir_type(<<"/tmp/">>) -> tmp;
maildir_type(<<"/new/">>) -> new.
 
%% private functions
maildir_commit(Binary, Maildir, Basename, Original) ->
    Tmp = maildir_path(Maildir, tmp, Basename),
    case file:write_file(Binary, Tmp) of
	{error, Reason} -> {error, Reason};
	ok ->
	    case maildir_path(Maildir, cur, Basename) of
		Original ->
		    %% overwrite, just rename it
		    ok = file:rename(Tmp, Original),
		    {ok, Original};
		New_path ->
		    %% different maildir
		    ok = file:delete(Original),
		    ok = file:rename(Tmp, New_path),
		    {ok, New_path}
	    end
    end.

leaf_mime_mend(How, {Type, SubType, Headers, Parameters, Body})
  when is_binary(Body) ->
    How({Type, SubType, Headers, Parameters, Body});
leaf_mime_mend(How, {Type, SubType, Headers, Parameters, Content})
  when is_tuple(Content) ->
    {Type, SubType, Headers, Parameters, leaf_mime_mend(How, Content)};
leaf_mime_mend(How, {Type, SubType, Headers, Parameters, Contents})
  when is_list(Contents) ->
    {Type, SubType, Headers, Parameters,
     lists:map(fun(Content) -> leaf_mime_mend(How, Content) end, Contents)}.

set_mime_parent({Type, SubType, Headers, Parameters, Content}, Pid) ->
    {Type, SubType, set_parent([], Headers, Pid), Parameters, Content}.

set_parent(List, [], _Pid) ->
    lists:reverse(List);
set_parent(List, [{<<"In-Reply-To">>, _} | Tail], undefined) ->
    set_parent(List, Tail, undefined);
set_parent(List, [{<<"In-Reply-To">>, _} | Tail], Pid) ->
    set_parent([{<<"In-Reply-To">>, Pid} | List], Tail, Pid);
set_parent(List, [{<<"References">>, _} | Tail], Pid) ->
    set_parent(List, Tail, Pid);
set_parent(List, [Head | Tail], Pid) ->
    set_parent([Head | List], Tail, Pid).

default(index_path) ->
    mc_configer:default_value(index_path, os:getenv("HOME") ++ "/Maildir").
