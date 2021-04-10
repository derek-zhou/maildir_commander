-module(mc_sexp).

-include_lib("kernel/include/logger.hrl").

-export([parse/1, parse_incomplete/1, parse_term/1, to_string/1, cmd_string/1,
	 escape/1, quote_escape/1]).

%% public function.

%% escape an string. 
%% backslash and double qoute are escaped
%% wrapped in double qoute if the string contains whitespace
-spec escape(unicode:chardata()) -> unicode:chardata().
escape(Str) ->
    maybe_quote(escape_special(Str)).

quote_escape(Str) ->
    quote(escape_special(Str)).

escape_special(Str) ->
    string:replace(string:replace(Str, "\\", "\\\\", all), "\"", "\\\"", all).

quote(Str) -> [$", Str, $"].

maybe_quote(Str) ->
    {_, Remain} = string:take(Str, "\\ \r\t\n", true),
    case string:is_empty(Remain) of
	true -> Str;
	false -> quote(Str)
    end.

    
%% parse the sexp into erlang term
%% integer -> integer
%% unquoted string -> atom,
%% I am going to trust mu not to spit out too many different atoms
%% quoted string -> binary string
%% (value1 . value2) -> [value1 | value2]
%% value1 and value2 must be integer, atom or string
%% (:key1 value1 :key2 value2 ...) -> [{key1, value1}, {key2, value2} ...]
%% key has to be with in limited set as simple string, and will be converted to binaries
%% value can be anything as above
%% (value1 value2 ...) -> [value1, value2 ...]

-spec parse(unicode:chardata()) -> any().
parse(Str) ->
    {Term, Remain} = parse_term(Str),
    case string:is_empty(Remain) of
	true -> Term;
	false -> error("Garbage at the end", [Str])
    end.

-spec parse_incomplete(unicode:chardata()) -> incomplete | {any(), unicode:chardata()}.
parse_incomplete(Str) ->
    try parse_term(Str) of
	{Term, Remain} -> {Term, Remain}
    catch
	incomplete -> incomplete
    end.

-spec parse_term(unicode:chardata()) -> {any(), unicode:chardata()}.
parse_term(Str) ->
    Trimmed = trim(Str),
    case string:next_codepoint(Trimmed) of
	[$( | Tail] -> parse_list(trim(Tail));
	[$" | Tail] ->
	    {Term, Remain} = parse_quoted_string(Tail),
	    {unicode:characters_to_binary(Term), Remain};
	_ -> parse_single(Trimmed)
    end.

trim(Str) ->
    Trimmed = string:trim(Str, leading),
    case string:next_codepoint(Trimmed) of
	[] -> Trimmed;
	[$; | Tail] ->   %% elisp comment
	    {_Leading, Trailing} = string:take(Tail, "\n", true),
	    case string:is_empty(Trailing) of
		true -> throw(incomplete);
		false -> trim(Trailing)
	    end;
	_ -> Trimmed
    end.

parse_single(Str) ->
    case string:to_integer(Str) of
	{error, _Reason} -> parse_atom(Str);
	{Int, Rest} -> {Int, Rest}
    end.

parse_atom(Str) ->
    case string:next_codepoint(Str) of
	[] -> throw(incomplete);
	[$. | Rest] -> {dot, Rest};
	_ ->
	    {Leading, Trailing} = string:take(Str, "-" ++ lists:seq($a,$z)),
	    case unicode:characters_to_list(Leading) of
		"" -> throw(incomplete);
		Word -> {list_to_atom(Word), Trailing}
	    end
    end.

parse_key(Str) ->
    {Leading, Trailing} = string:take(Str, "\\\"\s\t\n()", true),
    case unicode:characters_to_binary(Leading) of
	<<>> -> throw(incomplete);
	Word -> {Word, Trailing}
    end.

parse_quoted_string(Str) ->
    case string:next_codepoint(Str) of
	[] -> throw(incomplete);
	[$" | Tail] -> {"", Tail};
	[$\\ | Tail] -> parse_quoted_string_after_escape(Tail);
	[Head | Tail] ->
	    {Token, Remain} = parse_quoted_string(Tail),
	    {[Head | Token], Remain}
    end.

parse_quoted_string_after_escape(Str) ->
    case string:next_codepoint(Str) of
	[] -> throw(incomplete);
	[Head | Tail] ->
	    {Token, Remain} = parse_quoted_string(Tail),
	    %% emacs spec only defined escape for return, another slash and double qoute
	    case Head of
		$\\ -> {[$\\ | Token], Remain};
		$" -> {[$" | Token], Remain};
		$\n -> {Token, Remain};
		_ -> {[$\\, Head | Token], Remain}
	    end
    end.

parse_list(Str) ->
    case string:next_codepoint(Str) of
	[] -> throw(incomplete);
	[$) | Tail] -> {[], Tail};
	[$: | Tail] ->
	    {Key, Remain} = parse_key(Tail),
	    {Value, Remain2} = parse_term(Remain),
	    {List, Remain3} = parse_list(trim(Remain2)),
	    {[{Key, Value} | List], Remain3};
	_ ->
	    {Term, Remain2} = parse_term(Str),
	    {List, Remain3} = parse_list(trim(Remain2)),
	    case List of
		[dot, Tail] when
		      (is_integer(Term) orelse is_atom(Term) orelse is_binary(Term)) andalso
		      (is_integer(Tail) orelse is_atom(Tail) orelse is_binary(Tail)) ->
		    {[Term | Tail], Remain3};
		[dot | _Tail ] -> error("illegal cons cell");
		_ -> {[Term | List], Remain3}
	    end
    end.

-spec to_string(any()) -> unicode:chardata(). 
to_string(I) when is_integer(I) -> integer_to_binary(I);
to_string(false) -> <<"nil">>;
to_string(true) -> <<"t">>;
to_string(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_string(B) when is_binary(B) -> quote_escape(B);
to_string({K, V}) when is_binary(K) -> [":", K, $\s, to_string(V)];
to_string([H | T]) when (is_integer(H) orelse is_atom(H) orelse is_binary(H))
			andalso (is_integer(T) orelse is_atom(T) orelse is_binary(T)) ->
    [$(, to_string(H), " . ", to_string(T), $)];
to_string(L) when is_list(L) ->
    ["(", lists:join(" ", lists:map(fun to_string/1, L)), ")"].

-spec cmd_string(any()) -> unicode:chardata().
cmd_string(L) -> [to_string(L), $\n].
