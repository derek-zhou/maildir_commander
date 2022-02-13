%% pop emails from pop3 servers

-module(mc_popper).

-include_lib("kernel/include/logger.hrl").

%% apis
-export([pop_all/3]).

%% pop a POP3 account. emails are delivered into maildir.
%% throw error if anything wrong happen
-spec pop_all(list(), list(), list()) -> ok.
pop_all(User, Pass, Host) ->
    {ok, Connection} = epop_client:connect(User, Pass,
					   [{addr, Host}, {port, 995}, {user, User}, ssl ]),
    {ok, {TotalCount, TotalSize}} = epop_client:stat(Connection),
    lists:foreach(fun (Seq) -> pop_one(Seq, Connection) end, lists:seq(1, TotalCount)),
    epop_client:quit(Connection),
    ?LOG_NOTICE("~B mails, ~B bytes downloaded by POP3", [TotalCount, TotalSize]),
    ok.

pop_one(Seq, Connection) ->
    Filename = make_temp_mail(),
    Newdir = mc_mender:maildir_path("/", new),
    Tmpdir = mc_mender:maildir_path("/", tmp),
    TmpFile = Tmpdir ++ Filename,
    NewFile = Newdir ++ Filename,
    {ok, File} = file:open(TmpFile, [write, raw, delayed_write]),
    {ok, Acc} = epop_client:retrieve_start(Connection, Seq),
    NewAcc = write_mail(Acc, File),
    file:close(File),
    ok = epop_client:retrieve_after(NewAcc),
    ok = epop_client:delete(Connection, Seq),
    ok = file:rename(TmpFile, NewFile).

make_temp_mail() ->
    Time = os:system_time(microsecond),
    {ok, Host} = net:gethostname(),
    Pid = string:trim(pid_to_list(self()), both, "<>"),
    io_lib:format("~B.M~BP~s.~s", [Time div 1_000_000, Time rem 1_000_000, Pid, Host]).

write_mail(Acc, File) ->
    case epop_client:retrieve_next(Acc) of
	{halt, NewAcc} -> NewAcc;
	{Line, NewAcc} ->
	    Trimmed = string:trim(Line, trailing, ["\r\n"]),
	    ok = file:write(File, [Trimmed, "\n"]),
	    write_mail(NewAcc, File)
    end.
