-module(mc_server).
-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").

%% apis
-export([start_link/0, command/1, post/1, snooze/0]).

%% callbacks
-export([terminate/3, code_change/4, init/1, callback_mode/0]).

%% the states
-export([running/3, cooldown/3, housekeeping/3, hibernate/3]).

%% the state record to hold extended
-record(mc_state, { port, client, end_test, housekeeper, count = 0, buffer = <<>> }).

%% apis
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

command(Command) ->
    gen_statem:call(?MODULE, {command, Command}).

post(Command) ->
    gen_statem:cast(?MODULE, {command, Command}).

snooze() ->
    gen_statem:cast(?MODULE, snooze).

%% Mandatory callback functions
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.

callback_mode() -> state_functions.

terminate(_Reason, running, #mc_state{port = Port}) -> kill(Port);
terminate(_Reason, _, _Data) -> ok. 

init([]) ->
    process_flag(trap_exit, true),
    {ok, running, cold_boot(), [{state_timeout, 60000, timeout}, {next_event, internal, async}]}.

%% internal functions
kill(Port) ->
    Command = mc_mu_api:quit(),
    port_command(Port, mc_sexp:cmd_string(Command)),
    %% we do not care what the server has to say in quit; and it is not reliable anyway.
    %% however we want to wait until the server close the port, don't sigpipe the server
    flush_port(Port).

cold_boot() ->
    Port = open_port({spawn, "mu server"}, [binary]),
    Command = mc_mu_api:index(),
    port_command(Port, mc_sexp:cmd_string(Command)),
    Fun = mc_mu_api:fun_ending(Command),
    #mc_state{port = Port, end_test = Fun}.

boot(Backlog) ->
    ?LOG_NOTICE("mu server booting"),
    Port = open_port({spawn, "mu server"}, [binary]),
    lists:foreach(
      fun (Command) ->
	      run_command(Command, Port)
      end, lists:reverse(Backlog)),
    #mc_state{port = Port}.

run_command(Command, Port) ->
    port_command(Port, mc_sexp:cmd_string(Command)),
    run_loop(mc_mu_api:fun_ending(Command), <<>>, Port).

run_loop(Test_done, Buffer, Port) ->
    {Sexp, Remain} = read_port(Buffer, Port),
    case Test_done(Sexp) of
	true -> ok;
	false -> run_loop(Test_done, Remain, Port)
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
	    read_sexp(Remain, Len, Port)
    end.

read_sexp(Buffer, Len, _Port) when byte_size(Buffer) >= Len ->
    This = binary_part(Buffer, 0, Len),
    Remain = binary_part(Buffer, Len, byte_size(Buffer) - Len),
    {Sexp, _Junk} = mc_sexp:parse_term(This),
    {Sexp, Remain};
read_sexp(Buffer, Len, Port) ->
    receive
	{'EXIT', Port, Reason} ->
	    ?LOG_ERROR("mu server crashed unexpectedly: ~p", [Reason]),
	    error(Reason);
	{Port, {data, Binary}} ->
	    read_sexp(<<Buffer/binary,Binary/binary>>, Len, Port)
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
    {next_state, cooldown, [], [{state_timeut, 1000, timeout}]};
running(state_timeout, timeout, Data = #mc_state{port = Port, count = 0}) ->
    case mc_configer:default(housekeeper) of
	undefined ->
	    kill(Port),
	    {next_state, hibernate, [], [hibernate]};
	{M, F, A} ->
	    ?LOG_NOTICE("Start housekeeping..."),
	    {next_state, housekeeping, Data#mc_state{housekeeper = spawn_link(M, F, A)}}
    end;
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
running(cast, snooze, Data = #mc_state{count = 0}) -> {keep_state, Data#mc_state{count = 1}};
running(cast, snooze, Data) -> {keep_state, Data};
running(cast, {command, Command},
	Data = #mc_state{port = Port, count = Count}) ->
    port_command(Port, mc_sexp:cmd_string(Command)),
    Fun = mc_mu_api:fun_ending(Command),
    {keep_state,
     Data#mc_state{client = undefined, end_test = Fun, buffer = <<>>, count = Count + 1},
     [{next_event, internal, async}]};
running({call, From = {Client, _Tag}}, {command, Command},
	Data = #mc_state{port = Port, count = Count}) ->
    port_command(Port, mc_sexp:cmd_string(Command)),
    Fun = mc_mu_api:fun_ending(Command),
    {keep_state,
     Data#mc_state{client = Client, end_test = Fun, buffer = <<>>, count = Count + 1},
     [{reply, From, ok}, {next_event, internal, async}]}.

%% in housekeeping mode, we wait until the housekeeper have quit
housekeeping(info, {Port, {data, Binary}}, Data = #mc_state{port = Port}) ->
    ?LOG_NOTICE("Got junk ~ts", [Binary]),
    {keep_state, Data};
housekeeping(info, {'EXIT', Pid, _Reason}, #mc_state{port = Port, housekeeper = Pid}) ->
    ?LOG_NOTICE("Housekeeping done"),
    kill(Port),
    {next_state, hibernate, [], [hibernate]};
housekeeping(internal, async, Data = #mc_state{port = Port,
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
housekeeping(cast, snooze, Data) -> {keep_state, Data};
housekeeping(cast, {command, Command},
	     Data = #mc_state{port = Port}) ->
    port_command(Port, mc_sexp:cmd_string(Command)),
    Fun = mc_mu_api:fun_ending(Command),
    {keep_state,
     Data#mc_state{client = undefined, end_test = Fun, buffer = <<>>},
     [{next_event, internal, async}]};
housekeeping({call, From = {Client, _Tag}}, {command, Command},
	     Data = #mc_state{port = Port}) ->
    port_command(Port, mc_sexp:cmd_string(Command)),
    Fun = mc_mu_api:fun_ending(Command),
    {keep_state,
     Data#mc_state{client = Client, end_test = Fun, buffer = <<>>},
     [{reply, From, ok}, {next_event, internal, async}]}.

%% the cooldown state block 1 second so we do not relaunch the server too often
cooldown(state_timeout, timeout, Backlog) ->
    {next_state, hibernate, Backlog, [hibernate]};
cooldown(cast, snooze, Backlog) ->
    {next_state, running, boot(Backlog), [{state_timeout, 60000, timeout}]};
cooldown(cast, {command, Command}, Backlog) -> {keep_state, [Command | Backlog]};
cooldown({call, _Client}, _Content, Backlog) ->
    {next_state, running, boot(Backlog), [{state_timeout, 60000, timeout}]}.

%% the hibernate state can be waken up by any api call
hibernate(cast, snooze, Backlog) ->
    {next_state, running, boot(Backlog), [{state_timeout, 60000, timeout}]};
hibernate(cast, {command, Command}, Backlog) -> {keep_state, [Command | Backlog]};
hibernate({call, _client}, _Content, Backlog) ->
    {next_state, running, boot(Backlog), [postpone, {state_timeout, 60000, timeout}]}.
