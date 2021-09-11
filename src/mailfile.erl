%% this module parse and serialize a mailfile commonly found in a maildir mailbox
%% some code lifted from mimemail.erl from gen_smtp

-module(mailfile).

-export([read_mail/1, open_mail/1, close_mail/1, next_part/3, all_parts/1]).
-export([write_mail/2, new_mail/1, write_parts/2, write_parts/4]).

-type mime_part() :: {integer(), list(), map(), iolist()}.

-spec read_mail(file:filename()) -> [mime_part()].
read_mail(Path) ->
    {ok, Dev} = open_mail(Path),
    Parts = all_parts(Dev),
    ok = close_mail(Dev),
    Parts.

-spec write_mail([mime_part()], file:filename()) -> ok.
write_mail(Parts, Path) ->
    {ok, Dev} = new_mail(Path),
    ok = write_parts(Parts, Dev),
    ok = close_mail(Dev).

-spec open_mail(file:filename()) ->
	  {ok, file:io_device()} | {error, file:posix() | badarg | system_limit}.
open_mail(Path) ->
    file:open(Path, [read, read_ahead, binary]).

-spec new_mail(file:filename()) ->
	  {ok, file:io_device()} | {error, file:posix() | badarg | system_limit}.
new_mail(Path) ->
    file:open(Path, [write, delayed_write]).

-spec close_mail(file:io_device()) -> ok.
close_mail(Dev) ->
    ok = file:close(Dev).

-spec write_parts(list(), file:io_device()) -> ok | {error, term()}.
write_parts(Parts, Dev) -> write_parts(0, [], Parts, Dev).

-spec next_part(integer(), list(), file:io_device()) -> {integer(), list(), mime_part()}.
next_part(Level, Boundaries, Dev) ->
    Headers = read_headers(Dev, []),
    Parameters = parse_headers(Headers),
    {Level2, Boundaries2} =
	case get_boundary(Parameters) of
	    undefined -> {Level, Boundaries};
	    Boundary -> {Level + 1, [Boundary | Boundaries]}
	end,
    case read_until(Boundaries2, Dev) of
	{Body, true} ->
	    {Level2 - 1, tl(Boundaries2),
	     {Level, Headers, parse_body(Body, Parameters), Body}};
	{Body, false} ->
	    {Level2, Boundaries2,
		     {Level, Headers, parse_body(Body, Parameters), Body}}
    end.

-spec all_parts(file:io_device()) -> [mime_part()].
all_parts(Dev) -> all_parts(0, [], Dev).

all_parts(Level, Boundaries, Dev) ->
    case next_part(Level, Boundaries, Dev) of
	{0, [], Part = {0, _, _, _}} -> [Part];
	{Level2, Boundaries2, Part} -> [Part | all_parts(Level2, Boundaries2, Dev)]
    end.

-spec write_parts(integer(), list(), list(), file:io_device()) -> ok | {error, term()}.
write_parts(_Level, _Boundaries, [], _Dev) -> ok;
write_parts(Level, Boundaries, [{Level, Headers, Parameters, Body} | Tail], Dev) -> 
    lists:foreach(fun ({Key, Value}) ->
			 io:fwrite(Dev, "~s: ", [Key]),
			 file:write(Dev, Value)
		 end, Headers), 
    file:write(Dev, "\n"),
    file:write(Dev, Body),
    case get_boundary(Parameters) of
	undefined ->
	    case Tail of
		[] -> ok;
		[{Level, _, _, _} | _] ->
		    io:fwrite(Dev, "--~s\n", [hd(Boundaries)]),
		    write_parts(Level, Boundaries, Tail, Dev);		
		[{NextLevel, _, _, _} | _] when NextLevel =:= Level - 1->
		    io:fwrite(Dev, "--~s--\n", [hd(Boundaries)]),
		    write_parts(Level - 1, tl(Boundaries), Tail, Dev)
	    end;
	Boundary ->
	    io:fwrite(Dev, "--~s\n", [Boundary]),
	    write_parts(Level + 1, [Boundary | Boundaries], Tail, Dev)
    end.
    
-spec read_headers(file:io_device(), list()) -> list().
read_headers(Dev, Acc) ->
    case file:read_line(Dev) of
	eof -> lists:reverse(Acc);
	{error, Reason} -> error(Reason);
	{ok, <<"\n">>} -> lists:reverse(Acc);
	{ok, Line = <<"\s", _/binary>>} -> read_headers(Dev, add_to_headers(Line, Acc));
	{ok, Line = <<"\t", _/binary>>} -> read_headers(Dev, add_to_headers(Line, Acc));
	{ok, Line} -> read_headers(Dev, [read_header(Line) | Acc])
    end.

add_to_headers(Str, [{Key, Value} | Tail ]) when is_binary(Value) ->
    [{Key, [Value, Str]} | Tail];
add_to_headers(Str, [{Key, Value} | Tail ]) when is_list(Value) ->
    [{Key, Value ++ [Str]} | Tail].
    
read_header(Line) ->
    case string:split(Line, ":") of
	[Key, Value] -> {Key, string:trim(Value, leading, "\s\t")};
	_ -> error(["No : in header line: ", Line])
    end.

-spec read_until(list(), file:io_device()) -> {list(), boolean()}.
read_until([], Dev) ->
    case file:read_line(Dev) of
	eof -> {[], false};
	{error, Reason} -> error(Reason);
	{ok, Line} ->
	    {Lines, false} = read_until([], Dev),
	    {[Line | Lines], false}
    end;
read_until(Boundaries = [Boundary | _], Dev) ->
    Size = byte_size(Boundary),
    case file:read_line(Dev) of
	eof -> error("Unexpected EOF");
	{error, Reason} -> error(Reason);
	{ok, <<"--", Boundary:Size/binary, "--", _/binary>>} -> {[], true};
	{ok, <<"--", Boundary:Size/binary, _/binary>>} -> {[], false};
	{ok, Line} ->
	    {Lines, End} = read_until(Boundaries, Dev),
	    {[Line | Lines], End}
    end.

-spec parse_headers(list()) -> map().
parse_headers(Headers) -> lists:foldl(fun parse_header/2, #{}, Headers).

parse_header({<<"Content-Type">>, Value}, Map) ->
    {V, Params} = parse_header_value(Value),
    Map#{ content_type => V, content_type_params => Params};
parse_header({<<"Content-Disposition">>, Value}, Map) ->
    {V, Params} = parse_header_value(Value),
    Map#{ disposition => V, disposition_params => Params};
parse_header({<<"Content-Transfer-Encoding">>, Value}, Map) ->
    {V, _Params} = parse_header_value(Value),
    Map#{ encoding => V};
parse_header({_, _}, Map) -> Map.

% I am taking a shortcut here. For all the headers that I care about, There
% will be nothing more than ASCII string of:
% Value; Key1=Value1; Key2=Value2 ...
parse_header_value(Str) ->
    [Value | Additions] = tokenize_header_value(Str, []),
    Params = get_header_params(Additions, #{}),
    {string:lowercase(Value), Params}.

get_header_params([], Map) -> Map;
get_header_params([$;, Key, $=, Value | Tail], Map) ->
    get_header_params(Tail, Map#{string:lowercase(Key) => Value}).

tokenize_header_value(Str, Tokens) ->
    case string:next_codepoint(Str) of
	[] ->
	    lists:reverse(Tokens);
	[$\" | Tail] -> tokenize_header_value_in_quote(Tail, [], Tokens);
	[H | Tail] when H =:= $\s; H =:= $\t; H =:= $\r; H =:= $\n ->
            tokenize_header_value(Tail, Tokens);
	[H | Tail] when H =:= $\;; H =:= $\= ->
	    tokenize_header_value(Tail, [H | Tokens]);
        [H | Tail] ->
            tokenize_header_value_in_token(Tail, [H], Tokens)
    end.

tokenize_header_value_in_token(Str, Buffer, Tokens) ->
    case string:next_codepoint(Str) of
	[] ->
	    lists:reverse(Tokens);
	[$\" | Tail] ->
	    C = unicode:characters_to_binary(lists:reverse(Buffer)),
	    tokenize_header_value_in_quote(Tail, [], [C | Tokens]);
	[H | Tail] when H =:= $\s; H =:= $\t; H =:= $\r; H =:= $\n ->
	    C = unicode:characters_to_binary(lists:reverse(Buffer)),
	    tokenize_header_value(Tail, [C | Tokens]);
	[H | Tail] when H =:= $\;; H =:= $\= ->
	    C = unicode:characters_to_binary(lists:reverse(Buffer)),
	    tokenize_header_value(Tail, [H, C | Tokens]);
        [H | Tail] ->
            tokenize_header_value_in_token(Tail, [H | Buffer], Tokens)
    end.

tokenize_header_value_in_quote(Str, Buffer, Tokens) ->
    case string:next_codepoint(Str) of
	[] ->
	    error("unexpected end of string");
	[$\" | Tail] ->
	    C = unicode:characters_to_binary(lists:reverse(Buffer)),
	    tokenize_header_value(Tail, [C | Tokens]);
        [H | Tail] ->
            tokenize_header_value_in_quote(Tail, [H | Buffer], Tokens)
    end.

get_boundary(#{content_type_params := Params}) ->
    maps:get(<<"boundary">>, Params, undefined);
get_boundary(_) -> undefined.

-spec parse_body(list(), map()) -> map().
parse_body(Body, Map) ->
    case string:lowercase(maps:get(encoding, Map, <<"7bit">>)) of
	<<"quoted-printable">> ->
	    Map#{body => decode_quoted_printable(Body)};
	<<"base64">> ->
	    Map#{body => base64:decode(unicode:characters_to_binary(Body))};
	_ ->
	    Map#{body => unicode:characters_to_binary(Body)}
    end.

decode_quoted_printable(Lines) ->
    unicode:characters_to_binary(lists:map(fun decode_quoted_printable_line/1, Lines)).

decode_quoted_printable_line(Line) ->
    case string:split(Line, "=") of
	[Str] -> Str;
	[Head, Tail] -> [Head | decode_quoted_printable_after_equal(Tail)]
    end.

% soft new line
decode_quoted_printable_after_equal(<<"\n">>) -> <<>>;
decode_quoted_printable_after_equal(<<A:2/binary, Tail/binary>>) ->
    {ok, C, _} = io_lib:fread("~16u", binary_to_list(A)),
    [C | decode_quoted_printable_line(Tail)].