-module(mc_mender).

%% this module handle direct maildir accessing

-include_lib("kernel/include/logger.hrl").

-export([mend/2, mend/3, leaf_mend/2, leaf_mend/3, scrub_mime/1,
	 is_attachment/1, attachment_info/1,
	 fetch_mime/1, all_leaf_mime/1, fetch_content/3, set_mime_parent/2]).
-export([maildir_path/2]).

-export([read_text_file/1, write_text_file/2]).

%% mend an email from Path by chenging its contents using the closure How,
%% then put it into Maildir

-spec mend(function(), string()) -> ok | {error, binary()}.
mend(How, Path) ->
    case maildir_parse(Path) of
	{error, Reason} -> {error, Reason};
	{Maildir, cur, _Basename} -> mend(How, Path, Maildir);
	{_Maildir, _Type, _Basename} -> {error, "Not in cur maildir"}
    end.

-spec mend(function(), string(), string()) -> ok | {error, binary()}.
mend(How, Path, Maildir) ->
    case read_text_file(Path) of
	{error, Reason} -> {error, Reason};
	{ok, Binary} ->
	    Mime = mimemail:decode(Binary),
	    Mended_mime = How(Mime),
	    Mended =
		try mimemail:encode(Mended_mime)
		catch error:_ ->
			?LOG_WARNING("Cannot encode mime of ~ts. Fallback to the original",
				     [Path]),
			Binary
		end,
	    maildir_commit(Mended, Maildir, Path, Path)
    end.

-spec fetch_mime(string()) -> tuple() | {error, binary()}.
fetch_mime(Path) ->
    case read_text_file(Path) of
	{error, Reason} -> {error, Reason};
	{ok, Binary} -> mimemail:decode(Binary)
    end.

-spec all_leaf_mime(tuple()) -> [tuple()].
all_leaf_mime(Mime = {_Type, _SubType, _Headers, _Params, Body})
 when is_binary(Body) ->
    [Mime];
all_leaf_mime({_Type, _SubType, _Headers, _Params, Body}) when is_tuple(Body) ->
    all_leaf_mime(Body);
all_leaf_mime({_Type, _SubType, _Headers, _Params, Body}) when is_list(Body) ->
    lists:flatmap(fun all_leaf_mime/1, Body).

-spec is_attachment(tuple()) -> boolean().
is_attachment({_Type, _SubType, _Headers, #{disposition := <<"attachment">>}, _Body}) ->
    true;
is_attachment({_Type, _SubType, _Headers, #{content_type_params := Params}, _Body}) ->
    case proplists:get_value(<<"name">>, Params) of
	undefined -> false;
	_Name -> true
    end;
is_attachment(_) -> false.

-spec attachment_info(tuple()) -> tuple().
attachment_info({Type, SubType, _Headers,
		 #{disposition := <<"attachment">>,
		   disposition_params := Params},
		 Body}) ->
    {proplists:get_value(<<"filename">>, Params),
     <<Type/binary, "/", SubType/binary>>,
     Body};
attachment_info({Type, SubType, _Headers,
		 #{content_type_params := Params},
		 Body}) ->
    {proplists:get_value(<<"name">>, Params),
     <<Type/binary, "/", SubType/binary>>,
     Body}.

-spec fetch_content(binary(), binary(), tuple()) -> binary() | undefined.
fetch_content(Type, SubType, {Type, SubType, _Headers, _Params, Body})
  when is_binary(Body) -> Body;
fetch_content(Type, SubType, {_Type, _SubType, _Headers, _params, List})
  when is_list(List) ->
    fetch_content_from_list(Type, SubType, List);
fetch_content(_, _, _) -> undefined.

%% mend only the leaf mime parts

-spec leaf_mend(function(), string()) -> ok | {error, binary()}.
leaf_mend(How, Path) ->
    mend(fun(Mime) -> leaf_mime_mend(How, Mime) end, Path).

-spec leaf_mend(function(), string(), string()) -> ok | {error, binary()}.
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
maildir_path(Maildir, Type, Filename)
  when Type == cur orelse Type == tmp orelse Type == new ->
    Basename = filename:basename(Filename),
    lists:flatten([mc_configer:default(index_path),
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
    Dir = mc_configer:default(index_path),
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
maildir_commit(Binary, Maildir, Filename, Original) ->
    Tmp = maildir_path(Maildir, tmp, Filename),
    case write_text_file(Binary, Tmp) of
	{error, Reason} -> {error, Reason};
	ok ->
	    case maildir_path(Maildir, cur, Filename) of
		Original ->
		    %% overwrite, just rename it
		    ok = file:rename(Tmp, Original);
		New_path ->
		    %% different maildir
		    ok = file:delete(Original),
		    ok = file:rename(Tmp, New_path),
		    ok = maildir_commander:add(New_path)
	    end
    end.

%% behave like file:read_file but convert LF to CRLF
read_text_file(Path) ->
    case file:open(Path, [read, raw, binary, read_ahead]) of
	{error, Reason} -> {error, Reason};
	{ok, Device} ->
	    case read_lines([], Device) of
		{error, Reason} ->
		    file:close(Device),
		    {error, Reason};
		{ok, Lines} ->
		    file:close(Device),
		    {ok, binstr:join(Lines, "\r\n")}
	    end
    end.

read_lines(Buffer, Device) ->
    case file:read_line(Device) of
	eof -> {ok, lists:reverse(Buffer)};
	{error, Reason} -> {error, Reason};
	{ok, Data} ->
	    read_lines([string:trim(Data, trailing, "\r\n") | Buffer], Device)
    end.

%% behave like file:write_file but convert CRLF to LF
write_text_file(Binary, Path) ->
    case file:open(Path, [write, raw, delayed_write]) of
	{error, Reason} -> {error, Reason};
	{ok, Device} ->
	    write_lines(Device, Binary),
	    file:close(Device)
    end.

write_lines(Device, Binary) ->
    case binstr:strpos(Binary, "\r\n") of
	0 -> file:write(Device, Binary);
	1 ->
	    file:write(Device, "\n"),
	    write_lines(Device, binstr:substr(Binary, 3));
	Index ->
	    file:write(Device, binstr:substr(Binary, 1, Index - 1)),
	    file:write(Device, "\n"),
	    write_lines(Device, binstr:substr(Binary, Index + 2))
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
    New_headers = set_parent([], false, Headers, Pid),
    {Type, SubType, New_headers, Parameters, Content}.

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

fetch_content_from_list(_, _, []) -> undefined;
fetch_content_from_list(Type, SubType, [Head | Tail]) ->
    case fetch_content(Type, SubType, Head) of
	undefined -> fetch_content_from_list(Type, SubType, Tail);
	Content -> Content
    end.

