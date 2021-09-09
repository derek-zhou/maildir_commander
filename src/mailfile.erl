%% this module parse and serialize a mailfile commonly found in a maildir mailbox
%% much of the code lifted from mimemail.erl from gen_smtp
-module(mailfile).

-export([open_mail/1, close_mail/1, next_part/3, all_parts/1]).
-export([new_mail/1, write_mail/2, write_parts/4]).

-type mime_part() :: {integer(), list(), map(), iolist()}.

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

-spec write_mail(list(), file:io_device()) -> ok | {error, term()}.
write_mail(Parts, Dev) -> write_parts(0, [], Parts, Dev).

-spec next_part(integer(), list(), file:io_device()) -> {integer(), list(), mime_part()}.
next_part(Level, Boundaries, Dev) ->
    Headers = read_headers([], Dev),
    Parameters = parse_headers(Headers),
    {Level2, Boundaries2} =
	case maps:get(Parameters, boundary, undefined) of
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
	{0, [], Part} -> [Part];
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
    case maps:get(Parameters, boundary, undefined) of
	undefined ->
	    case Tail of
		[] -> ok;
		[{Level, _, _, _} | _] ->
		    io:fwrite(Dev, "--~s\n", [hd(Boundaries)]),
		    write_parts(Level, Boundaries, Tail, Dev);		
		[{NextLevel, _, _, _} | _] when NextLevel == Level - 1->
		    io:fwrite(Dev, "--~s--\n", [hd(Boundaries)]),
		    write_parts(Level - 1, tl(Boundaries), Tail, Dev)
	    end;
	Boundary ->
	    io:fwrite(Dev, "~s\n", [Boundary]),
	    write_parts(Level + 1, [Boundary, Boundaries], Tail, Dev)
    end.
    
-spec read_headers(list(), file:io_device()) -> list().
read_headers(Acc, Dev) ->
    case file:read_line(Dev) of
	eof -> error("Unexpected EOF");
	{error, Reason} -> error(Reason);
	{ok, <<"\n">>} -> lists:reverse(Acc);
	{ok, Line = <<"\s", _/binary>>} -> add_to_headers(Line, Acc);
	{ok, Line = <<"\t", _/binary>>} -> add_to_headers(Line, Acc);
	{ok, Line} -> [read_header(Line) | Acc]
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
	eof -> [];
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
	{ok, <<"--", Boundary:Size/binary-unit:8, "--", _/binary>>} -> {[], true};
	{ok, <<"--", Boundary:Size/binary-unit:8, _/binary>>} -> {[], false};
	{ok, Line} ->
	    {Lines, End} = read_until(Boundaries, Dev),
	    {[Line | Lines], End}
    end.

-spec parse_headers(list()) -> map().
parse_headers(_Headers) -> #{}.

-spec parse_body(list(), map()) -> map().
parse_body(_Body, Parameters) -> Parameters.
    
