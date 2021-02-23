-module(mc_watcher).

-include_lib("kernel/include/logger.hrl").

%% this is the maildir watcher

-export([start_link/0]).
-export([init/0]).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    {ok, Pid}.

init() ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Newdir = mc_mender:maildir_path("/", new),
    Curdir = mc_mender:maildir_path("/", cur),
    %% I have to watch for both move_to and create. Some program use one or the other
    Command = "inotifywait -q -m -e moved_to -e create " ++ Newdir,
    Port = open_port({spawn, Command}, [binary]),
    move_existing_new_mail(Newdir, Curdir),
    watch_loop(<<>>, Newdir, Curdir, Port),
    exit(shutdown).

watch_loop(Buffer, From, To, Port) ->
    case handle_message(Buffer, From, To) of
	{ok, Remain} -> watch_loop(Remain, From, To, Port);
	{incomplete, Remain} ->
	    receive
		{Port, {data, Binary}} ->
		    watch_loop(<<Remain/binary, Binary/binary>>, From, To, Port);
		{'EXIT', Port, _Reason} -> ok;
		{'EXIT', _From, _Reason} ->
		    kill(Port),
		    watch_loop(Remain, From, To, Port);
		Msg ->
		    ?LOG_ERROR("Received something unexpected ~p", [Msg]),
		    error("Unrecognized message received")
	    end
    end.

kill(Port) ->
    case erlang:port_info(Port) of
	undefined -> ok;
	Plist ->
	    case proplists:get_value(os_pid, Plist) of
		undefined -> ok;
		Pid when is_integer(Pid) ->
		    os:cmd(io_lib:format("kill ~B", [Pid])),
		    ok
	    end
    end.

handle_message(Buffer, From, To) ->
    case parse_message(Buffer) of
	{incomplete, Remain} -> {incomplete, Remain};
	{File, Remain} ->
	    move_mail(File, From, To),
	    {ok, Remain}
    end.

parse_message(Buffer) ->
    case string:take(Buffer, "\n", true) of
	{Leading, <<>>} -> {incomplete, Leading};
	{Leading, Trailing} ->
	    Remain = string:trim(Trailing, leading, "\n"),
	    case string:split(Leading, " ", all) of
		[_Dir, _Event, File] -> {unicode:characters_to_list(File), Remain};
		_  ->
		    ?LOG_WARNING("Unrecognized text: ~ts", [Leading]),
		    {incomplete, Remain}
	    end
    end.

move_existing_new_mail(From, To) ->
    {ok, Filenames} = file:list_dir(From),
    lists:foreach(fun(Each) -> move_mail(Each, From, To) end, Filenames).

move_mail(File, From, To) ->
    Path = From ++ File,
    case filelib:is_file(Path) of
	false ->
	    %% there could be a race condition
	    ?LOG_WARNING("Mail ~ts disappeared, moved already?", [File]);
	true ->
	    %% per maildir standard: https://cr.yp.to/proto/maildir.html
	    Dest = To ++ File ++ ":2,",
	    ok = file:rename(Path, Dest),
	    case maildir_commander:add(Dest) of
		{error, Msg} ->
		    ?LOG_WARNING("error in adding ~ts. Error: ~ts", [File, Msg]);
		ok ->
		    ?LOG_NOTICE("added ~ts to the database", [File])
	    end
    end.
