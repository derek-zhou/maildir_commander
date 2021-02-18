-module(mc_local_server).

-include_lib("kernel/include/logger.hrl").

%% this is the unix domain socket server

-export([start_link/0]).
-export([init/0, service/1]).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    {ok, Pid}.

init() ->
    register(?MODULE, self()),
    File = default(socket_file),
    file:delete(File),
    {ok, LSock} = gen_tcp:listen(0, [binary, {packet, 0}, {active, false},
				     {ip, {local, File}}]),
    accept_loop(LSock),
    exit(shutdown).

accept_loop(LSock) ->
    case gen_tcp:accept(LSock) of
	{ok, Sock} ->
	    spawn(?MODULE, service, [Sock]),
	    accept_loop(LSock);
	{error, closed} -> ok;
	{error, Reason} -> error(Reason)
    end.

service(Sock) ->
    ?LOG_NOTICE("Client connected"),
    service_loop([], Sock).

service_loop(Buffer, Sock) ->
    case mc_cmd:parse(Buffer) of
	incomplete ->
	    case gen_tcp:recv(Sock, 0) of
		{ok, Packet} -> service_loop(Buffer ++ [Packet], Sock);
		{error, _Reason} ->
		    ?LOG_NOTICE("Client disconnected"),
		    gen_tcp:shutdown(Sock, write)
	    end;
	{[], Remain} -> service_loop([Remain], Sock);
	{[{<<"cmd">>, <<"quit">>} | _], _Remain} ->
	    gen_tcp:shutdown(Sock, write);
	{Command = [{<<"cmd">>, _} | _], Remain} ->
	    ok = mc_server:command(Command),
	    send_sexp_loop(mc_mu_api:fun_ending(Command), Sock),
	    service_loop([Remain], Sock);
	%% index is special because we need to print progress
	{[{<<"cmdx">>, <<"index">>}], _Remain} ->
	    ok = mc_server:command(mc_mu_api:index()),
	    wait_index_loop(Sock),
	    gen_tcp:shutdown(Sock, write);
	{[{<<"cmdx">>, Command} | Args], _Remain} ->
	    gen_tcp:send(Sock, issue_command(Command, Args)),
	    gen_tcp:shutdown(Sock, write)
    end.

issue_command(<<"contacts">>, _Args) ->
    case maildir_commander:contacts() of
	{error, Message} ->
	    io_lib:format("error: ~ts~n", [Message]);
	{ok, Contacts} ->
	    lists:map(
	      fun({Name, Mail}) ->
		      unicode:characters_to_binary(io_lib:format("~ts <~ts>~n", [Name, Mail]))
	      end, Contacts)
    end;
issue_command(<<"index">>, _Args) ->
    case maildir_commander:index() of
	{error, Message} ->
	    io_lib:format("error: ~ts~n", [Message]);
	{ok, Num} ->
	    io_lib:format("~B messages indexed~n", [Num])
    end;
issue_command(<<"find">>, [Query]) ->
    print_tree(maildir_commander:find(Query));
issue_command(<<"findx">>, [Query]) ->
    print_tree(maildir_commander:find(Query, true));
issue_command(<<"scrub">>, [Path]) ->
    print_status(maildir_commander:scrub(Path));
issue_command(<<"graft">>, [Path, Parent]) ->
    print_status(maildir_commander:graft(Path, Parent));
issue_command(<<"graft">>, [Path, Parent, Maildir]) ->
    print_status(maildir_commander:graft(Path, Parent, Maildir));
issue_command(Command, Args) ->
    io_lib:format("Syntax error in command: ~ts args: ~p~n", [Command, Args]).

print_status({error, Message}) ->
    io_lib:format("error: ~ts~n", [Message]);
print_status({ok, _Docid}) -> "ok\n".

print_tree({error, Message}) ->
    io_lib:format("error: ~ts~n", [Message]);
print_tree({ok, List, Mails}) ->
    mc_tree:flatmap(fun(Docid, Level) ->
			    format_mail_header(maps:get(Docid, Mails), Level)
		    end, List).

format_mail_header(#{date := Date, from := From, subject := Subject}, Level) ->
    unicode:characters_to_binary(
      io_lib:format("~s~ts ~ts <~ts> ~ts~n",
		    [ lists:duplicate(Level*2, $\s),
		      calendar:system_time_to_rfc3339(Date),
		      hd(From), tl(From), Subject ])).

wait_index_loop(Sock) ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} ->
	    gen_tcp:send(Sock, io_lib:format("error: ~ts~n", [Msg]));
	{async, [{<<"info">>, index}, {<<"status">>, complete} | Rest ]} ->
	    gen_tcp:send(Sock, io_lib:format("~B messages indexed~n",
					     [proplists:get_value(<<"processed">>, Rest)]));
	{async, [{<<"info">>, index} | Rest ]} ->
	    gen_tcp:send(Sock, io_lib:format("~B messages indexed~n",
					     [proplists:get_value(<<"processed">>, Rest)])),
	    wait_index_loop(Sock)
    end.

send_sexp_loop(Test_done, Sock) ->
    receive
	{async, Sexp} ->
	    send_sexp(Sexp, Sock),
	    case Test_done(Sexp) of
		true -> ok;
		false  -> send_sexp_loop(Test_done, Sock)
	    end
    end.

send_sexp(Sexp, Sock) ->
    Serialized = mc_sexp:to_string(Sexp),
    Length = iolist_size(Serialized),
    Length_bin = integer_to_binary(Length, 16),
    gen_tcp:send(Sock, [<<16#FE, Length_bin/binary, 16#FF>>, Serialized]).

%% internal functions

default(socket_file) ->
    mc_configer:default_value(socket_file, os:getenv("HOME") ++ "/.mc_server_sock").
