-module(mc_server).
-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").

%% apis
-export([start_link/0, async_command/1, command/1]).

%% callbacks
-export([terminate/3, code_change/4, init/1, callback_mode/0]).

%% the states
-export([running/3, cooldown/3, hibernate/3]).

%% the state record to hold extended
-record(mc_state, { port, client, end_test, count = 0, buffer = <<>>}).

%% apis
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

command(Command) ->
    gen_statem:call(?MODULE, {command, Command}).

async_command(Command) ->
    gen_statem:call(?MODULE, {async_command, Command}).

%% Mandatory callback functions
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.

callback_mode() -> state_functions.

terminate(_Reason, running, Data) -> kill(Data);
terminate(_Reason, _, _Data) -> ok. 

init([]) ->
    process_flag(trap_exit, true),
    {ok, running, cold_boot(), [{state_timeout, 60000, timeout}, {next_event, internal, async}]}.

%% internal functions
kill(#mc_state{port = Port}) ->
    Command = mc_mu_api:quit_command(),
    port_command(Port, mc_cmd:to_string(Command)),
    %% we do not care what the server has to say in quit; and it is not reliable anyway.
    %% however we want to wait until the server close the port, don't sigpipe the server
    ok = flush_port(Port),
    ?LOG_NOTICE("mu server quited"),
    ok.

cold_boot() ->
    Data = #mc_state{port = Port} = boot(),
    Command = mc_mu_api:init_command(),
    port_command(Port, mc_cmd:to_string(Command)),
    Fun = mc_mu_api:fun_ending(Command),
    Data#mc_state{end_test = Fun}.

boot() ->
    ?LOG_NOTICE("mu server booting"),
    Port = open_port({spawn, "mu server"}, [binary]),
    #mc_state{port = Port}.
    
read_port_until(Test_done, Port) -> read_port_until(<<>>, [], Test_done, Port).

read_port_until(Buffer, Sexps, Test_done, Port) ->
    {Sexp, Remain} = read_port(Buffer, Port),
    New_Sexps = [Sexp | Sexps],

    case Test_done(Sexp) of
	true -> {lists:reverse(New_Sexps), Remain};
	false -> read_port_until(Remain, New_Sexps, Test_done, Port)
    end.

flush_port(Port) ->
    receive
	{'EXIT', Port, _Reason} -> ok;
	{Port, {data, _Binary}} -> flush_port(Port)
    end.

read_port(Buffer, Port) ->
    case parse_header(Buffer) of
	{incomplete, Remain} ->
	    receive
		{'EXIT', Port, Reason} ->
		    ?LOG_ERROR("mu server crashed unexpectedly: ~p. Buffer is ~p, Remain is ~p",
		 	       [Reason, Buffer, Remain]),
		    error(Reason);
		{Port, {data, Binary}} ->
		    read_port(<<Remain/binary,Binary/binary>>, Port)
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
	{'EXIT', Port, Reason} ->
	    ?LOG_ERROR("mu server crashed unexpectedly: ~p", [Reason]),
	    error(Reason);
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

%% in running state we have the mu server running

%% ignore everything mu give us outside calls
running(info, {Port, {data, Binary}}, Data = #mc_state{port = Port}) ->
    ?LOG_NOTICE("Got junk ~ts", [Binary]),
    {keep_state, Data};
running(info, {'EXIT', Port, Reason}, #mc_state{port = Port}) ->
    ?LOG_WARNING("mu server crashed: ~p", [Reason]),
    {next_state, cooldown, #mc_state{}, [{state_timeut, 1000, timeout}]};
running(state_timeout, timeout, Data = #mc_state{count = 0}) ->
    kill(Data),
    {next_state, cooldown, #mc_state{}, [{state_timeout, 1000, timeout}]};
running(state_timeout, timeout, Data) ->
    {keep_state, Data#mc_state{count = 0}, [{state_timeout, 60000, timeout}]};
running(internal, async, Data = #mc_state{port = Port,
					  client = Client,
					  end_test = Test_done,
					  buffer = Buffer}) ->
    {Sexp, Remain} = read_port(Buffer, Port),
    if Client /= undefined -> Client ! {async, Sexp};
       Client == undefined -> ok
    end,
    case Test_done(Sexp) of
	true ->
	    {keep_state,
	     Data#mc_state{client = undefined, end_test = undefined, buffer = <<>>}};
	false ->
	    {keep_state,
	     Data#mc_state{buffer = Remain}, [{next_event, internal, async}]}
    end;
running({call, From}, {command, Command},
	Data = #mc_state{port = Port, count = Count}) ->
    port_command(Port, mc_cmd:to_string(Command)),
    % we do not care about left over
    {Sexps, _Remain} = read_port_until(mc_mu_api:fun_ending(Command), Port),
    {keep_state,
     Data#mc_state{count = Count + 1},
     [{reply, From, Sexps}]};
running({call, From = {Client, _Tag}}, {async_command, Command},
	Data = #mc_state{port = Port, count = Count}) ->
    port_command(Port, mc_cmd:to_string(Command)),
    Fun = mc_mu_api:fun_ending(Command),
    {keep_state,
     Data#mc_state{client = Client, end_test = Fun, buffer = <<>>, count = Count + 1},
     [{reply, From, ok}, {next_event, internal, async}]}.

%% the cooldown state block 1 second so we do not relaunch the server too often
cooldown(state_timeout, timeout, #mc_state{count = 1}) ->
    {next_state, running, boot(), [{state_timeout, 60000, timeout}]};
cooldown(state_timeout, timeout, Data) ->
    {next_state, hibernate, Data, [hibernate]};
cooldown({call, _Client}, _Content, _Data) ->
    {keep_state, #mc_state{count = 1}, [postpone]}.

%% the hibernate state can be waken up by any api call
hibernate({call, _client}, _Content, _Data) ->
    {next_state, running, boot(), [postpone, {state_timeout, 60000, timeout}]}.
