%% this module parse and serialize a mailfile commonly found in a maildir mailbox
%% some code lifted from mimemail.erl from gen_smtp

-module(mailfile).

-export([read_mail/1, open_mail/1, close_mail/1, next_part/3]).
-export([write_mail/2, new_mail/1, write_parts/2, write_parts/4]).

-type mime_part() :: {integer(), list(), map(), iolist()}.

-spec read_mail(file:filename()) -> {ok, [mime_part()]} | {error, term()}.
read_mail(Path) ->
    case open_mail(Path) of
	{error, Reason} -> {error, Reason};
	{ok, Dev} ->
	    Parts = all_parts(Dev),
	    ok = close_mail(Dev),
	    {ok, Parts}
    end.

-spec write_mail([mime_part()], file:filename()) -> ok | {error, term()}.
write_mail(Parts, Path) ->
    case new_mail(Path) of
	{error, Reason} -> {error, Reason};
        {ok, Dev} ->
	    ok = write_parts(Parts, Dev),
	    ok = close_mail(Dev)
    end.

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

-spec next_part(integer(), list(), file:io_device()) ->
	  {integer(), list(), mime_part()} | undefined.
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
	    Part = {Level, Headers, parse_body(Body, Parameters), Body},
	    case Level2 of
		0 -> {-1, [], Part};
		_ -> {Level2 - 1, tl(Boundaries2), Part}
	    end;
	{Body, false} ->
	    {Level2, Boundaries2,
	     {Level, Headers, parse_body(Body, Parameters), Body}}
    end.

-spec all_parts(file:io_device()) -> [mime_part()].
all_parts(Dev) -> all_parts(0, [], Dev).

all_parts(Level, Boundaries, Dev) ->
    case next_part(Level, Boundaries, Dev) of
	{-1, _, Part} -> [Part];
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
    
-spec read_headers(file:io_device(), list()) -> list() | undefined.
read_headers(Dev, Acc) ->
    case file:read_line(Dev) of
	eof -> lists:reverse(Acc);
	{error, Reason} -> error(Reason);
	{ok, <<"\n">>} -> lists:reverse(Acc);
	{ok, Line = <<"\s", _/binary>>} -> read_headers(Dev, add_to_headers(Line, Acc));
	{ok, Line = <<"\t", _/binary>>} -> read_headers(Dev, add_to_headers(Line, Acc));
	{ok, Line} ->
	    % ignore junk header lines
	    case read_header(Line) of
		undefined -> read_headers(Dev, Acc);
		Header -> read_headers(Dev, [Header | Acc])
	    end
    end.

add_to_headers(_, []) -> [];
add_to_headers(Str, [{Key, Value} | Tail ]) when is_binary(Value) ->
    [{Key, [Value, Str]} | Tail];
add_to_headers(Str, [{Key, Value} | Tail ]) when is_list(Value) ->
    [{Key, Value ++ [Str]} | Tail].
    
read_header(Line) ->
    case string:split(Line, ":") of
	[Key, Value] -> {Key, string:trim(Value, leading)};
	_ -> undefined
    end.

-spec read_until(list(), file:io_device()) -> {list(), boolean()}.
read_until([], Dev) -> read_until_end(Dev, []);
read_until([Boundary | _], Dev) -> read_until(Boundary, Dev, []).

read_until_end(Dev, Acc) ->
    case file:read_line(Dev) of
	eof -> {lists:reverse(Acc), true};
	{error, Reason} -> error(Reason);
	{ok, Line} -> read_until_end(Dev, [Line | Acc])
    end.

read_until(Boundary, Dev, Acc) ->
    Size = byte_size(Boundary),
    case file:read_line(Dev) of
	eof -> {lists:reverse(Acc), true};
	{error, Reason} -> error(Reason);
	{ok, <<"--", Boundary:Size/binary, "--", _/binary>>} -> {lists:reverse(Acc), true};
	{ok, <<"--", Boundary:Size/binary, _/binary>>} -> {lists:reverse(Acc), false};
	{ok, Line} -> read_until(Boundary, Dev, [Line | Acc])
    end.

-spec parse_headers(list()) -> map().
parse_headers(Headers) -> lists:foldl(fun parse_header/2, #{}, Headers).

parse_header({Key, Value}, Map) ->
    case string:lowercase(Key) of
	<<"content-type">> ->
	    {V, Params} = parse_header_value(Value),
	    Map#{ content_type => V, content_type_params => normalize_params(Params)};
	<<"content-disposition">> ->
	    {V, Params} = parse_header_value(Value),
	    Map#{ disposition => V, disposition_params => normalize_params(Params)};
	<<"content-transfer-encoding">> ->
	    {V, _Params} = parse_header_value(Value),
	    Map#{ encoding => V};
	_ -> Map
    end.

% I am taking a shortcut here. For all the headers that I care about, There
% will be nothing more than ASCII string of:
% Value; Key1=Value1; Key2=Value2 ...
parse_header_value(Str) ->
    [Value | Additions] = tokenize_header_value(Str, []),
    Params = get_header_params(Additions, #{}),
    Value2 = cast_utf8(decode_header_value(Value)),
    {string:lowercase(Value2), Params}.

get_header_params([], Map) -> Map;
get_header_params([$;], Map) -> Map;
get_header_params([$;, Key, $=, Value | Tail], Map) ->
    Value2 = cast_utf8(decode_header_value(Value)),
    Key2 = string:lowercase(parse_param_key(Key)),
    get_header_params(Tail, update_params(Map, Key2, Value2));
% there are broken emails out there
get_header_params(_, Map) -> Map.

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
	    C = string:trim(unicode:characters_to_binary(lists:reverse(Buffer)), trailing),
	    lists:reverse([C | Tokens]);
	[$\" | Tail] ->
	    C = unicode:characters_to_binary(lists:reverse(Buffer)),
	    tokenize_header_value_in_quote(Tail, [], [C | Tokens]);
	[H | Tail] when H =:= $\;; H =:= $\= ->
	    C = string:trim(unicode:characters_to_binary(lists:reverse(Buffer)), trailing),
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

get_boundary(#{content_type_params := #{<<"boundary">> := Boundary}}) ->
    Boundary;
get_boundary(_) -> undefined.

get_encoding(#{encoding := Encoding}) -> Encoding;
get_encoding(_) -> <<"7bit">>.

is_text_type(<<"text/", _Rest/binary>>) -> true;
is_text_type(_) -> false.

get_charset(#{content_type_params := #{<<"charset">> := Charset}}) ->
    string:lowercase(Charset);
get_charset(_) -> <<"utf-8">>.

-spec parse_body(list(), map()) -> map().
parse_body(Body, Map = #{content_type := Type}) ->
    Map#{ body =>
	      case {is_text_type(Type), get_encoding(Map)} of
		  {false, <<"base64">>} -> Body;
		  {false, Encoding} -> decode_body(Body, Encoding);
		  {true, Encoding} ->
		      cast_utf8(convert_utf8(decode_body(Body, Encoding), get_charset(Map)))
	      end };
% pre MIME mail or fillers between boundaries
parse_body(Body, Map) ->
    case string:trim(cast_utf8(iolist_to_binary(Body))) of
	<<>> -> Map;
	Bin -> Map#{ content_type => <<"text/plain">>, body => Bin }
    end.

decode_quoted_printable_body([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
decode_quoted_printable_body([Head | Tail], Acc) ->
    decode_quoted_printable_body(Tail, [decode_quoted_printable_line(Head) | Acc]).

decode_base64_body([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
decode_base64_body([Head | Tail], Acc) ->
    Str = try base64:decode(Head) of
	      Bin -> Bin
	  catch
	      _:_ -> Head
	  end,
    decode_base64_body(Tail, [Str | Acc]).

decode_body(Body, <<"quoted-printable">>) -> decode_quoted_printable_body(Body, []);
decode_body(Body, <<"base64">>) -> decode_base64_body(Body, []);
decode_body(Body, _) -> iolist_to_binary(Body).

convert_utf8(Body, <<"utf-8">>) -> Body;
convert_utf8(Body, Charset) -> iconv:convert(Charset, <<"utf-8">>, Body).

cast_utf8(Body) ->
    case unicode:characters_to_binary(Body) of
	{error, Bin, _Junk} -> Bin;
	{incomplete, Bin, _Junk} -> Bin;
	Bin -> Bin
    end.

decode_quoted_printable_line(Line) ->
    case string:split(Line, "=") of
	[Str] -> Str;
	[<<>>, Tail] -> decode_quoted_printable_after_equal(Tail);
	[Head, Tail] -> [Head | decode_quoted_printable_after_equal(Tail)]
    end.

% soft new line
decode_quoted_printable_after_equal(<<"\n">>) -> <<>>;
decode_quoted_printable_after_equal(Line = <<A:2/binary, Tail/binary>>) ->
    case io_lib:fread("~16u", binary_to_list(A)) of
	{ok, C, _} -> [C | decode_quoted_printable_line(Tail)];
	% there are email that give broken quote printable, this is the best I can do
	_ -> [$= | decode_quoted_printable_line(Line)]
    end.

decode_header_value(Str) ->
    case re:run(Str, "^\\=\\?([\\w-]+)\\?([QB])\\?(.*)\\?\\=",
		[{capture, all_but_first, binary}]) of
	{match, [Charset, <<"Q">>, Encoded]} ->
	    convert_utf8(iolist_to_binary(decode_quoted_printable_line(Encoded)), Charset);
	{match, [Charset, <<"B">>, Encoded]} ->
	    convert_utf8(base64:decode(Encoded), Charset);
	_  -> Str
    end.

parse_param_key(Key) ->
    case re:run(Key, "^([\\w-]+)\\*(\\d+)$", [{capture, all_but_first, binary}]) of
	{match, [K, _N]} -> K;
	_ -> Key
    end.

update_params(Map, Key, Value) ->
    case maps:get(Key, Map, undefined) of
	undefined -> Map#{Key => Value};
	V when is_binary(V) -> Map#{Key := [V, Value]};
	L when is_list(L)-> Map#{Key := L ++ [Value]}
    end.

normalize_params(Map) ->
    maps:map(fun (_K, V) when is_binary(V) -> V;
		 (_K, V) when is_list(V) -> unicode:characters_to_binary(V)
	     end, Map).
