-module(maildir_commander).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% apis
-export([start_link/0]).
-export([init/1, terminate/2, handle_call/3, handle_info/2, handle_cast/2]).

-export([kill/0,
	 find/1, find/2, find/3, find/4, find/5, find/6, find/7,
	 ping/0,
	 index/0, index/1, index/2, index/3, index/4,
	 contacts/0, contacts/1, contacts/2]).

%% the state record to hold extended
-record(mc_state, {port :: port()}).

%% apis
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

kill() -> gen_server:call(?MODULE, kill).

find(Query) ->
    find(Query, true, undefined, false, -1, false, false).

find(Query, Threads) ->
    find(Query, Threads, undefined, false, -1, false, false).

find(Query, Threads, Sortfield) ->
    find(Query, Threads, Sortfield, false, -1, false, false).

find(Query, Threads, Sortfield, Reverse) ->
    find(Query, Threads, Sortfield, Reverse, -1, false, false).

find(Query, Threads, Sortfield, Reverse, Maxnum) ->
    find(Query, Threads, Sortfield, Reverse, Maxnum, false, false).

find(Query, Threads, Sortfield, Reverse, Maxnum, Skip_dups) ->
    find(Query, Threads, Sortfield, Reverse, Maxnum, Skip_dups, false).

find(Query, Threads, Sortfield, Reverse, Maxnum, Skip_dups, Include_related) ->
    gen_server:call(?MODULE, {find, Query, Threads, Sortfield, Reverse,
			      Maxnum, Skip_dups, Include_related}).
 
index() ->
    index([os:getenv("HOME"), "/Maildir"], [], true, false).

index(Path) ->
    index(Path, [], true, false).

index(Path, My_addresses) ->
    index(Path, My_addresses, true, false).

index(Path, My_addresses, Cleanup) ->
    index(Path, My_addresses, Cleanup, false).

index(Path, My_addresses, Cleanup, Lazy_check) ->
    gen_server:call(?MODULE, {index, Path, My_addresses, Cleanup, Lazy_check}).

ping() ->  gen_server:call(?MODULE, ping).

contacts() ->
    contacts(false, 0).

contacts(Personal) ->
    contacts(Personal, 0).

contacts(Personal, After) ->
    gen_server:call(?MODULE, {contacts, Personal, After}).
    

%% internal functions
kill(#mc_state{port = undefined}) -> false;
kill(#mc_state{port = Port}) ->
    case erlang:port_info(Port) of
	undefined -> false;
	Plist -> 
	    Os_pid = proplists:get_value(os_pid, Plist),
	    os:cmd(io_lib:format("kill ~B", [Os_pid])),
	    ok
    end.

%% escape an string to be sent to mu. 
%% backslash and double qoute are escaped
%% wrapped in double qoute if the string contains whitespace
escape_for_mu(Str) ->
    Str1 = string:replace(Str, "\\", "\\\\"),
    Str2 = string:replace(Str1, "\"", "\\\""),
    {_, Remain} = string:take(Str2, " \r\t\n", true),
    case string:is_empty(Remain) of
	true -> Str2;
	false -> [$", Str2, $"]
    end.
 
read_multiple_info(Port) -> read_multiple_info(<<>>, [], Port).

read_multiple_info(Buffer, Sexps, Port) ->
    {Sexp, Remain} = read_port(Buffer, Port),
    New_Sexps = [Sexp | Sexps],
    case proplists:get_value(status, Sexp, undefined) of
	<<"complete">> -> {lists:reverse(New_Sexps), Remain};
	<<"running">> -> read_multiple_info(Remain, New_Sexps, Port);
 	_ ->
	    ?LOG_ERROR("Unrecognized sexp in read multiple info: ~p", [Sexp]),
	    error("Unrecognized sexp in read multiple info")
    end.
	
read_find_results(Port) -> read_find_results(<<>>, [], Port).

read_find_results(Buffer, Sexps, Port) ->
    {Sexp, Remain} = read_port(Buffer, Port),
    New_Sexps = [Sexp | Sexps],
    
    case proplists:is_defined(found, Sexp) orelse proplists:is_defined(error, Sexp) of
	true -> {lists:reverse(New_Sexps), Remain};
	false -> read_find_results(Remain, New_Sexps, Port)
    end.
	
read_port(Port) -> read_port(<<>>, Port).

read_port(Buffer, Port) ->
    case parse_header(Buffer) of
	incomplete ->
	    receive
		{Port, {data, Binary}} -> read_port(<<Buffer/binary,Binary/binary>>, Port)
	    end;
	{Len, Remain} ->
	    read_sexp([Remain], byte_size(Remain), Len, Port)
    end.

read_sexp(Buffer, Buffer_len, Len, _Port) when Buffer_len >= Len ->
    {Sexp, Remain} = mc_sexp:parse_term(Buffer),
    %% Making sure the remain is consolidated to one binary for easy handling
    {Sexp, iolist_to_binary(Remain)};
read_sexp(Buffer, Buffer_len, Len, Port) when Buffer_len < Len ->
    receive
	{Port, {data, Binary}} ->
	    read_sexp(Buffer ++ [Binary], Buffer_len + byte_size(Binary), Len, Port) 
    end.

%% FE, Length in HEX, FF, assume no more than 6 bytes. This is a sane upper limit
parse_header(<<>>) -> incomplete;
parse_header(<<16#FE, B1, 16#FF, Rest/binary>>) ->
    {list_to_integer([B1], 16), Rest};
parse_header(<<16#FE, B1, B2, 16#FF, Rest/binary>>) ->
    {list_to_integer([B1, B2], 16), Rest};
parse_header(<<16#FE, B1, B2, B3, 16#FF, Rest/binary>>) ->
    {list_to_integer([B1, B2, B3], 16), Rest};
parse_header(<<16#FE, B1, B2, B3, B4, 16#FF, Rest/binary>>) ->
    {list_to_integer([B1, B2, B3, B4], 16), Rest};
parse_header(<<16#FE, B1, B2, B3, B4, B5, 16#FF, Rest/binary>>) ->
    {list_to_integer([B1, B2, B3, B4, B5], 16), Rest};
parse_header(<<16#FE, B1, B2, B3, B4, B5, B6, 16#FF, Rest/binary>>) ->
    {list_to_integer([B1, B2, B3, B4, B5, B6], 16), Rest};
parse_header(<<16#FE, Rest/binary>>) when byte_size(Rest) < 7 -> incomplete;
parse_header(<<16#FE, Rest/binary>>) -> error("Header too long", [Rest]);
parse_header(<<_Head, Rest/binary>>) -> parse_header(Rest).
    
%% callbacks

init([]) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, "mu server"}, [binary]),
    {ok, #mc_state{port = Port}}.

terminate(_Reason, State) -> kill(State).

handle_cast(kill, State) -> {noreply, kill(State)}.

%% ignore everything mu give us outside calls
handle_info({_Port, {data, Binary}}, State) ->
    ?LOG_NOTICE("Got junk ~ts", [Binary]),
    {noreply, State};
handle_info({'EXIT', _Port, Reason}, _State) ->
    ?LOG_ERROR("Port unexpected closed: ~p", [Reason]),
    %% don't bother with relaunch, just crash and let the supervisor handling that
    error("Port unexpected closed").

handle_call(ping, _From, State = #mc_state{port = Port}) ->
    port_command(Port, "cmd:ping\n"),
    % we do not care about left over
    {Sexp, _Remain} = read_port(Port),
    {reply, Sexp, State};
handle_call({find, Query, Threads, Sortfield, Reverse, Maxnum, Skip_dups, Include_related},
	    _From, State = #mc_state{port = Port}) ->
    Command = 
	if Sortfield == undefined ->
		io_lib:format("cmd:find query:~s threads:~p reverse:~p maxnum:~B "
			      "skip-dups:~p include-related:~p\n",
			      [base64:encode(unicode:characters_to_binary(Query)),
			       Threads, Reverse, Maxnum, Skip_dups, Include_related]);
	   true ->
		io_lib:format("cmd:find query:~s threads:~p sortfield:~ts reverse:~p maxnum:~B "
			      "skip-dups:~p include-related:~p\n",
			      [base64:encode(unicode:characters_to_binary(Query)),
			       Threads, Sortfield, Reverse, Maxnum, Skip_dups, Include_related])
	end,
    port_command(Port, Command),
    % we do not care about left over
    {Sexp, _Remain} = read_find_results(Port),
    {reply, Sexp, State};
handle_call({index, Path, My_addresses, Cleanup, Lazy_check}, _From,
	    State = #mc_state{port = Port}) ->
    Path_escaped = escape_for_mu(Path),
    Command =
	if My_addresses == [] ->
		io_lib:format("cmd:index path:~ts cleanup:~p lazy-check:~p\n",
			    [Path_escaped, Cleanup, Lazy_check]);
	   true ->
		io_lib:format("cmd:index path:~ts my-addresses:~ts cleanup:~p lazy-check:~p\n",
			    [Path_escaped, lists:join($,, My_addresses), Cleanup, Lazy_check])
	end,
    port_command(Port, Command),
    % we do not care about left over
    {Sexp, _Remain} = read_multiple_info(Port),
    {reply, Sexp, State};
handle_call({contacts, Personal, After}, _From,
	    State = #mc_state{port = Port}) ->
    Command = io_lib:format("cmd:contacts personal:~p after:~B\n",
			    [Personal, After]),
    port_command(Port, Command),
    % we do not care about left over
    {Sexp, _Remain} = read_port(Port),
    {reply, Sexp, State}.
