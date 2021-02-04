-module(mc_cmd).

-export([parse/1, to_string/1]).

%% mc_cmd are simple command interface used by mu server. The textual representation is:
%% Key:Value1 Key2:Value2 ...
%% Keys are predefined string. Values are strings, integers or true or false
%% internally the cmd is represented as a proplist

-spec parse(unicode:chardata()) -> incomplete | proplists:proplist().
parse(Str) ->
    Trimmed = string:trim(Str, leading, "\s\t"),
    case string:next_codepoint(Trimmed) of
	[] -> incomplete;
	[$\n | Remain] -> {[], Remain};
	_ ->
	    case parse_key(Trimmed) of
		incomplete -> incomplete;
		{Key, Remain} ->
 		    case parse_value(Remain) of
			incomplete -> incomplete;
			{Value, Remain2} ->
			    case parse(Remain2) of
				incomplete -> incomplete;
				{List, Remain3} -> {[{Key, Value} | List], Remain3}
			    end
		    end
	    end 
    end.

parse_key(Str) ->
    {Leading, Trailing} = string:take(Str, ":", true),
    case unicode:characters_to_binary(Leading) of
	<<>> -> incomplete;
	Word ->
	    [$: | Remain] = string:next_codepoint(Trailing),
	    {Word, Remain}
    end.

parse_value(Str) ->
    case string:next_codepoint(Str) of
	[] -> incomplete;
	[$\n | _Tail] -> {<<>>, Str};
	[$" | Tail] -> 
	    {Term, Remain} = parse_quoted_string(Tail),
	    {unicode:characters_to_binary(Term), Remain};
	_ ->
	    {Leading, Trailing} = string:take(Str, "\s\t\r\n", true),
	    case string:is_empty(Trailing) of
		true -> incomplete;
		false ->
		    case string:to_integer(Str) of
			{error, _Reason} -> {parse_word(Leading), Trailing};
			{Int, Rest} -> {Int, Rest}
		    end
	    end
    end.

parse_word(Str) ->
    case unicode:characters_to_binary(Str) of
	<<"true">> -> true;
	<<"false">> -> false;
	Word -> Word
    end.

parse_quoted_string(Str) ->
    case string:next_codepoint(Str) of
	[] -> incomplete;
	[$" | Tail] -> {"", Tail};
	[$\\ | Tail] -> parse_quoted_string_after_escape(Tail);
	[Head | Tail] ->
	    case parse_quoted_string(Tail) of
		incomplete -> incomplete;
		{Token, Remain} -> {[Head | Token], Remain}
	    end
    end.

parse_quoted_string_after_escape(Str) ->
    case string:next_codepoint(Str) of
	[] -> incomplete;
	[Head | Tail] ->
	    case parse_quoted_string(Tail) of
		incomplete -> incomplete;
		{Token, Remain} ->
	    %% emacs spec only defined escape for return, another slash and double qoute
		    case Head of
			$\\ -> {[$\\ | Token], Remain};
			$" -> {[$" | Token], Remain};
			$\n -> {Token, Remain};
			_ -> {[$\\, Head | Token], Remain}
		    end
	    end
    end.

value_string(true) -> <<"true">>;
value_string(false) -> <<"false">>;
value_string(I) when is_integer(I) -> integer_to_binary(I);
value_string(V) -> mc_sexp:escape(V).

-spec to_string(proplists:proplist()) -> unicode:chardata().
to_string([]) -> "\n";
to_string([{} | T]) -> to_string(T);
to_string([{Key, Value} | T]) -> [[Key, $:, value_string(Value), $ ] | to_string(T)].

