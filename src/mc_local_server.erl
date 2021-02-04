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
    accept_loop(LSock).

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
    print_prompt(Sock),
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
	{[{<<"cmd">>, <<"quit">>} | _], _Remain} ->
	    ?LOG_NOTICE("Client sent quit"),
	    gen_tcp:shutdown(Sock, write);
	{[], Remain} ->
	    print_prompt(Sock),
            service_loop([Remain], Sock);
	{Command, Remain} ->
	    ok = mc_server:async_command(Command),
	    send_sexp_loop(mc_mu_api:fun_ending(Command), Sock),
	    print_prompt(Sock),
	    service_loop([Remain], Sock)
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

print_prompt(Sock) ->
    gen_tcp:send(Sock, ";; welcome to mc(mu 1.2.0)\n;; mu> ").

%% internal functions

default_value(Key, Default) ->
    case application:get_env(?MODULE) of
	undefined -> Default;
	{ok, Map} -> maps:get(Key, Map, Default)
    end.

default(socket_file) ->
    default_value(socket_file, [os:getenv("HOME"), "/.mc_server_sock"]).
