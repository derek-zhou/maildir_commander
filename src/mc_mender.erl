-module(mc_mender).

-include_lib("kernel/include/logger.hrl").

-export([mend/3, maildir_path/2, maildir_path/3, maildir_parse/1]).

%% mend an email from Path by chenging its contents using the closure How,
%% then put it into Maildir

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
-spec maildir_parse(string()) -> {error, binary()} | {atom(), string(), string()}.
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

default(index_path) ->
    mc_configer:default_value(index_path, os:getenv("HOME") ++ "/Maildir").
