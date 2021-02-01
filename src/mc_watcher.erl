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
    Dir = mc_mu_api:default(index_path),
    %% I have to watch for both move_to and create. Some program use one or the other
    Command = ["inotifywait -m -e moved_to -e create ", Dir, "/new/"],
    Port = open_port({spawn, unicode:characters_to_list(Command)}, [binary]),
    move_existing_new_mail(Dir),
    watch_loop(<<>>, Dir, Port).

watch_loop(Buffer, Dir, Port) ->
    case handle_message(Buffer, Dir) of
	{ok, Remain} -> watch_loop(Remain, Dir, Port);
	{incomplete, Remain} ->
	    receive
		{Port, {data, Binary}} ->
		    watch_loop(<<Remain/binary, Binary/binary>>, Dir, Port)
	    end
    end.

handle_message(Buffer, Dir) ->
    case parse_message(Buffer) of
	{incomplete, Remain} -> {incomplete, Remain};
	{File, Remain} ->
	    move_mail_to_cur(File, Dir),
	    {ok, Remain}
    end.

parse_message(Buffer) ->
    case string:take(Buffer, "\n", true) of
	{Leading, <<>>} -> {incomplete, Leading};
	{Leading, Trailing} ->
	    Remain = string:trim(Trailing, leading, "\n"),
	    case string:split(Leading, " ", all) of
		[_Dir, _Event, File] -> {File, Remain};
		_  ->
		    ?LOG_WARNING("Unrecognized text: ~ts", [Leading]),
		    {incomplete, Remain}
	    end
    end.

move_existing_new_mail(Dir) ->
    {ok, Filenames} = file:list_dir(unicode:characters_to_list([Dir, "/new/"])),
    lists:foreach(fun(Each) -> move_mail_to_cur(Each, Dir) end, Filenames).

move_mail_to_cur(File, Dir) ->
    Path = unicode:characters_to_list([Dir, "/new/", File]),
    case filelib:is_file(Path) of
	false ->
	    %% there could be a race condition
	    ?LOG_WARNING("Mail ~ts disappeared, moved already?", [File]);
	true ->
	    %% per maildir standard: https://cr.yp.to/proto/maildir.html
	    Dest = unicode:characters_to_list([Dir, "/cur/", File, ":2,"]),
	    ok = file:rename(Path, Dest),
	    %% the sent function name is from mu. add return too much crap
	    Sexp = mc_mu_api:sent(Dest, "/"),
	    case proplists:is_defined(error, Sexp) of
		false ->
		    ?LOG_NOTICE("added ~ts to the database, docid: ~B",
				[File, proplists:get_value(docid, Sexp)]);
		true ->
		    ?LOG_WARNING("error in adding ~ts. Error code ~B: ~ts",
				 [File, proplists:get_value(error, Sexp),
				  proplists:get_value(message, Sexp)])
	    end
    end.
