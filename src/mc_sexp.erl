-module(mc_sexp).

-export([parse/1, parse_term/1]).

%% public function.

%% parse the sexp into erlang term
%% t -> true
%% nil -> false
%% integer -> integer
%% unquoted string -> binary string
%% quoted string -> list string
%% (value1 value2 ...) -> [value1, value2 ...]
%% (:key1 value1 :key2 value2 ...) -> [{key1, value1}, {key2, value2} ...]
%% key has to be with in limited set as simple string, and will be converted to atoms
%% value can be anything as above

-spec parse(unicode:chardata()) -> any().
parse(Str) ->
    {Term, Remain} = parse_term(Str),
    case string:is_empty(Remain) of
	true -> Term;
	false -> error("Garbage at the end", [Str])
    end.

parse_term(Str) ->
    Trimmed = string:trim(Str, leading),
    case string:next_codepoint(Trimmed) of
	[$( | Tail] -> parse_list(string:trim(Tail, leading));
	[$" | Tail] -> parse_quoted_string(Tail);
	_ -> parse_single(Trimmed)
    end.

parse_single(Str) ->
    case string:to_integer(Str) of
	{error, _Reason} -> parse_word(Str);
	{Int, Rest} -> {Int, Rest}
    end.
    
parse_word(Str) ->
    {Leading, Trailing} = string:take(Str, "\n\t\r\s()", true),
    case unicode:characters_to_binary(Leading) of
	<<>> -> error("Unexpected end of string");
	<<"t">> -> {true, Trailing};
	<<"nil">> -> {false, Trailing};
	Word -> {Word, Trailing}
    end.

parse_key(Str) ->
    {Leading, Trailing} = string:take(Str, "\n\t\r\s()", true),
    case unicode:characters_to_list(Leading) of
	"" -> error("Unexpected end of string");
	Word -> {list_to_atom(Word), Trailing}
    end.

parse_quoted_string(Str) ->
    case string:next_codepoint(Str) of
	[] -> error("Unexpected end of string");
	[$" | Tail] -> {"", Tail};
	[$\\ | Tail] -> parse_quoted_string_after_escape(Tail);
	[Head | Tail] ->
	    {Token, Remain} = parse_quoted_string(Tail),
	    {[Head | Token], Remain}
    end.

parse_quoted_string_after_escape(Str) ->
    case string:next_codepoint(Str) of
	[] -> error("Unexpected end of string");
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
	[] -> error("Unexpected end of string");
	[$) | Tail] -> {[], Tail};
	[$: | Tail] ->
	    {Key, Remain} = parse_key(Tail),
	    {Value, Remain2} = parse_term(Remain),
	    {Plist, Remain3} = parse_plist(string:trim(Remain2, leading)),
	    {[{Key, Value} | Plist], Remain3};
	_ ->
	    {Term, Remain2} = parse_term(Str),
	    {List, Remain3} = parse_regular_list(string:trim(Remain2, leading)),
	    {[Term | List], Remain3}
    end.

parse_plist(Str) ->
    case string:next_codepoint(Str) of
	[] -> error("Unexpected end of string");
	[$) | Tail] -> {[], Tail};
	[$: | Tail] ->
	    {Key, Remain} = parse_key(Tail),
	    {Value, Remain2} = parse_term(Remain),
	    {Plist, Remain3} = parse_plist(string:trim(Remain2, leading)),
	    {[{Key, Value} | Plist], Remain3};
	_ -> error("Expect plist", [Str])
    end.

parse_regular_list(Str) ->
    case string:next_codepoint(Str) of
	[] -> error("Unexpected end of string");
	[$) | Tail] -> {[], Tail};
	[$: | _Tail] -> error("Expect regular list", [Str]);
	_ ->
	    {Term, Remain2} = parse_term(Str),
	    {List, Remain3} = parse_regular_list(string:trim(Remain2, leading)),
	    {[Term | List], Remain3}
    end.
