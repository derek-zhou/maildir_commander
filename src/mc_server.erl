-module(mc_server).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% apis
-export([start_link/0]).
-export([init/1, terminate/2, handle_call/3, handle_info/2, handle_cast/2, handle_continue/2]).

%% the state record to hold extended
-record(mc_state, { port, client, end_test, buffer = <<>>}).

%% apis
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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

read_port_until(Test_done, Port) -> read_port_until(<<>>, [], Test_done, Port).

read_port_until(Buffer, Sexps, Test_done, Port) ->
    {Sexp, Remain} = read_port(Buffer, Port),
    New_Sexps = [Sexp | Sexps],

    case Test_done(Sexp) of
	true -> {lists:reverse(New_Sexps), Remain};
	false -> read_port_until(Remain, New_Sexps, Test_done, Port)
    end.
	
read_port(Buffer, Port) ->
    case parse_header(Buffer) of
	{incomplete, Remain} ->
	    receive
		{Port, {data, Binary}} -> read_port(<<Remain/binary,Binary/binary>>, Port)
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
parse_header(<<>>) -> {incomplete, <<>>};
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
parse_header(Buffer = <<16#FE, Rest/binary>>) when byte_size(Rest) < 7 ->
    {incomplete, Buffer};
parse_header(<<16#FE, Rest/binary>>) -> error("Header too long", [Rest]);
parse_header(<<_Head, Rest/binary>>) -> parse_header(Rest).
    
%% callbacks

init([]) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, "mu server"}, [binary]),
    Command = mc_mu_api:init_command(),
    port_command(Port, mc_cmd:to_string(Command)),
    Fun = mc_mu_api:fun_ending(Command),
    {ok, #mc_state{port = Port, end_test = Fun}, {continue, async}}.

terminate(_Reason, State) -> kill(State).

%% ignore everything mu give us outside calls
handle_info({_Port, {data, Binary}}, State) ->
    ?LOG_NOTICE("Got junk ~ts", [Binary]),
    {noreply, State};
handle_info({'EXIT', _Port, Reason}, _State) ->
    ?LOG_ERROR("Port unexpected closed: ~p", [Reason]),
    %% don't bother with relaunch, just crash and let the supervisor handling that
    error("Port unexpected closed").

handle_call({mu_server, Command}, _From, State = #mc_state{port = Port}) ->
    port_command(Port, mc_cmd:to_string(Command)),
    % we do not care about left over
    {Sexps, _Remain} = read_port_until(mc_mu_api:fun_ending(Command), Port),
    {reply, Sexps, State};
handle_call({mu_server_async, Command}, {Client, _Tag}, State = #mc_state{port = Port}) ->
    port_command(Port, mc_cmd:to_string(Command)),
    Fun = mc_mu_api:fun_ending(Command),
    {reply, ok, State#mc_state{client = Client, end_test = Fun, buffer = <<>>},
     {continue, async}}.

handle_cast({mu_server, Command}, State = #mc_state{port = Port}) ->
    port_command(Port, mc_cmd:to_string(Command)),
    % we do not care about return values
    {_Sexps, _Remain} = read_port_until(mc_mu_api:fun_ending(Command), Port),
    {noreply, State}.

handle_continue(async, State = #mc_state{port = Port,
					 client = Client,
					 end_test = Test_done,
					 buffer = Buffer}) ->
    {Sexp, Remain} = read_port(Buffer, Port),
    if Client /= undefined -> Client ! {async, Sexp};
       Client == undefined -> ok
    end,
    case Test_done(Sexp) of
	true ->
	    {noreply, State#mc_state{client = undefined, end_test = undefined, buffer = <<>>}};
	false ->
	    {noreply, State#mc_state{buffer = Remain}, {continue, async}}
    end.
