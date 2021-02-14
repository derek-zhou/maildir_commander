-module(mc_mender).

-include_lib("kernel/include/logger.hrl").

-export([mend/3, leaf_mend/3, scrub_mime/2, maildir_path/2, maildir_path/3, maildir_parse/1]).

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

%% mend only the leaf mime parts

-spec leaf_mend(function(), string(), string()) -> {ok, integer()} | {error, binary()}.
leaf_mend(How, Path, Maildir) ->
    mend(fun (Mime) -> leaf_mend(How, Mime) end, Path, Maildir).

leaf_mend(How, Mime = {_Type, _SubType, _Headers, _Parameters, Body}) when is_binary(Body) ->
    How(Mime);
leaf_mend(How, {Type, SubType, Headers, Parameters, Content}) when is_tuple(Content) ->
    {Type, SubType, Headers, Parameters, leaf_mend(How, Content)};
leaf_mend(How, {Type, SubType, Headers, Parameters, Contents}) when is_list(Contents) ->
    {Type, SubType, Headers, Parameters,
     lists:map(fun(Content) -> leaf_mend(How, Content) end, Contents)}.

%% scrub all attachments

-spec scrub_mime(string(), string()) -> {ok, integer()} | {error, binary()}.
scrub_mime(Path, Maildir) ->
    leaf_mend(fun scrub_mime/1, Path, Maildir).

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
