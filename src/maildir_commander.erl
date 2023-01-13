-module(maildir_commander).

-include_lib("kernel/include/logger.hrl").

%% public interface of maildir_commander

-export([index/0, add/1, delete/1, contacts/0, view/1, stream_mail/1, stream_parts/5,
	 find/1, find/2, find/3, find/4, find/5, find/6, find_all/6, flag/2, move/2,
	 scrub/1, graft/2, orphan/1, snooze/0, pop_all/3]).

%% rerun indexing. return {ok, Num} where Num is the number of messages indexed
%% or {error, Msg} where Msg is a binary string for the error
-spec index() -> {ok, integer()} | {error, binary()}.
index() ->
    Command = mc_mu_api:index(),
    ok = mc_server:command(Command),
    index_loop().

index_loop() ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"info">>, index}, {<<"status">>, complete} | Rest ]} ->
	    {ok, proplists:get_value(<<"checked">>, Rest)};
	%% last 2 are chatty messages that we don't care
	{async, [{<<"update">>, _} | _ ]} -> ?FUNCTION_NAME();
	{async, [{<<"info">>, _} | _ ]} -> ?FUNCTION_NAME()
    end.

%% just a head up to bring the server from hibernating
snooze() -> mc_server:snooze().

%% add a message to the database. it must has the full path and is already be in the Maildir.
%% return ok if successful or {error, Msg} where Msg is a binary string for the error
-spec add(string()) -> ok.
add(Path) ->
    case mc_configer:default(notify_new_mail) of
	undefined -> ok;
	{M,F,A} -> apply(M, F, A)
    end,
    %% the sent function name is from mu. add return too much crap
    Command = mc_mu_api:sent(unicode:characters_to_binary(Path)),
    mc_server:post(Command).

%% delete a message from both the filesystem and the database
-spec delete(integer()) -> ok.
delete(Docid) ->
    Command = mc_mu_api:remove(Docid),
    mc_server:post(Command).

%% return all contacts in the database
-spec contacts() -> {ok, [{term(), binary()}]} | {error, binary()}.
contacts() ->
    Command = mc_mu_api:contacts(),
    ok = mc_server:command(Command),
    contacts_loop().

contacts_loop() ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"contacts">>, Contacts} | _ ]} ->
	    {ok, lists:map(fun parse_contact/1, Contacts)};
	%% last 2 are chatty messages that we don't care
	{async, [{<<"update">>, _} | _ ]} -> ?FUNCTION_NAME();
	{async, [{<<"info">>, _} | _ ]} -> ?FUNCTION_NAME()
    end.

%% return a mail with Headers
-spec view(integer()) -> {ok, map()} | {error, binary()}.
view(Docid) ->
    Command = mc_mu_api:view(Docid),
    ok = mc_server:command(Command),
    case view_loop() of
	{error, Msg} -> {error, Msg};
	{ok, Mail} -> {ok, parse_mail_headers(Mail)}
    end.

%% stream full content of a mail with {mail_part, Ref, Map}
-spec stream_mail(string()) -> {ok, reference()} | {error, binary()}.
stream_mail(Path) ->
    case mailfile:open_mail(Path) of
	{error, Reason} -> {error, Reason};
	{ok, Dev} ->
	    Ref = make_ref(),
	    spawn_link(?MODULE, stream_parts, [0, [], self(), Ref, Dev]),
	    {ok, Ref}
    end.

%% pop all mails from a remote pop3 mailbox
-spec pop_all(binary(), binary(), binary()) -> ok.
pop_all(User, Pass, Host) ->
    mc_pop_manager:pop_all(unicode:characters_to_list(User),
			   unicode:characters_to_list(Pass),
			   unicode:characters_to_list(Host)).

stream_parts(Level, Boundaries, Pid, Ref, Dev) ->
    case mailfile:next_part(Level, Boundaries, Dev) of
	{-1, [], {_Level, _Headers, Parameters, _Body}} ->
	    case should_send(Parameters) of
		true -> Pid ! {mail_part, Ref, Parameters};
		false -> ok
	    end,
	    Pid ! {mail_part, Ref, eof},
	    ok = mailfile:close_mail(Dev);
	{Level2, Boundaries2, {_Level, _Headers, Parameters, _Body}} ->
	    case should_send(Parameters) of
		true -> Pid ! {mail_part, Ref, Parameters};
		false -> ok
	    end,
	    stream_parts(Level2, Boundaries2, Pid, Ref, Dev)
    end.

should_send(#{content_type := <<"text/plain">>}) -> true;
should_send(#{content_type := <<"text/html">>}) -> true;
should_send(#{content_type := <<"multipart/", _Rest/binary>>}) -> false;
should_send(#{disposition_params := #{<<"filename">> := _}}) -> true;
should_send(#{content_type_params := #{<<"name">> := _}, disposition := <<"attachment">>}) -> true;
should_send(_) -> false.

%% return all mail matching the query. return a tree of docids, and a map from docid to mail
-spec find(string()) -> {ok, mc_tree:t(), map()} | {error, binary()}.
find(Query) -> find(Query, false).

-spec find(string(), boolean()) -> {ok, mc_tree:t(), map()} | {error, binary()}.
find(Query, Threads) -> find(Query, Threads, mc_configer:default(sort_field)).

-spec find(string(), boolean(), string()) -> {ok, mc_tree:t(), map()} | {error, binary()}.
find(Query, Threads, Sort_field) -> find(Query, Threads, Sort_field, false).

-spec find(string(), boolean(), string(), boolean()) ->
	  {ok, mc_tree:t(), map()} | {error, binary()}.
find(Query, Threads, Sort_field, Descending) ->
    find(Query, Threads, Sort_field, Descending, mc_configer:default(skip_dups)).

-spec find(string(), boolean(), string(), boolean(), boolean()) ->
	  {ok, mc_tree:t(), map()} | {error, binary()}.
find(Query, Threads, Sort_field, Descending, Skip_dups) ->
    find(Query, Threads, Sort_field, Descending, Skip_dups,
	 mc_configer:default(include_related)).

-spec find(string(), boolean(), string(), boolean(), boolean(), boolean()) ->
	  {ok, mc_tree:t(), map()} | {error, binary()}.
find(Query, Threads, Sort_field, Descending, Skip_dups, Include_related) ->
    Command = mc_mu_api:find(unicode:characters_to_binary(Query),
			     Threads, Sort_field, Descending,
			     mc_configer:default(max_num),
			     Skip_dups, Include_related),
    ok = mc_server:command(Command),
    find_loop([], #{}).

-spec find_all(string(), boolean(), string(), boolean(), boolean(), boolean()) ->
	  {ok, mc_tree:t(), map()} | {error, binary()}.
find_all(Query, Threads, Sort_field, Descending, Skip_dups, Include_related) ->
    Command = mc_mu_api:find(unicode:characters_to_binary(Query),
			     Threads, Sort_field, Descending, 65535,
			     Skip_dups, Include_related),
    ok = mc_server:command(Command),
    find_loop([], #{}).

%% change the flags of a mail
-spec flag(integer(), string()) -> {ok, map()} | {error, binary()}.
flag(Docid, Flags) ->
    Command = mc_mu_api:move(Docid, undefined,
			     unicode:characters_to_binary(Flags)),
    ok = mc_server:command(Command),
    case move_loop() of
	{error, Msg} -> {error, Msg};
	{ok, Mail} -> {ok, parse_mail_headers(Mail)}
    end.

%% move a mail to another location
-spec move(integer(), string()) -> ok | {error, binary()}.
move(Docid, Maildir) ->
    Command = mc_mu_api:move(Docid, unicode:characters_to_binary(Maildir)),
    ok = mc_server:command(Command),
    case move_loop() of
	{error, Msg} -> {error, Msg};
	{ok, Mail} -> {ok, parse_mail_headers(Mail)}
    end.

move_loop() ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"update">>, Mail} | _ ]} -> {ok, Mail}
    end.

view_loop() ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"view">>, Mail } | _ ]} -> {ok, Mail}
    end.

find_loop(Tree, Mails) ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"erase">>, t} | _ ]} -> find_loop([], #{});
	{async, [{<<"found">>, _Total} | _ ]} -> {ok, mc_tree:finalize(Tree), Mails};
	{async, [{<<"headers">>, [Headers]} | _ ]} ->
	    Docid = proplists:get_value(<<"docid">>, Headers, <<>>),
	    find_loop(mc_tree:append(Docid, parse_thread_level(Headers), Tree),
		      maps:put(Docid, parse_mail_headers(Headers), Mails));
	%% last 2 are chatty messages that we don't care
	{async, [{<<"update">>, _} | _ ]} -> find_loop(Tree, Mails);
	{async, [{<<"info">>, _} | _ ]} -> find_loop(Tree, Mails)
    end.

parse_thread_level(Headers) ->
    case proplists:get_value(<<"meta">>, Headers) of
	undefined -> 0;
	Thread ->
	    case proplists:get_value(<<"level">>, Thread, 0) of
		0 -> 0;
		Level ->
		    case proplists:get_value(<<"orphan">>, Thread, nil) of
			t -> 0;
			_ -> Level
		    end
	    end
    end.

parse_mail_headers(Headers) ->
    #{ subject => proplists:get_value(<<"subject">>, Headers, <<>>),
       %% from is a list of email addresses. Pick the first one
       from => case proplists:get_value(<<"from">>, Headers) of
		   undefined -> [<<>> | <<>>];
		   [] -> [<<>> | <<>>];
		   [[{<<"name">>, Name}, {<<"email">>, Email}] | _] -> [Name | Email];
		   [[{<<"email">>, Email}, {<<"name">>, Name}] | _] -> [Name | Email];
		   [[{<<"email">>, Email}] | _] -> [nil | Email]
	       end,
       to => lists:map(fun parse_recipient/1, proplists:get_value(<<"to">>, Headers, [])),
       cc => lists:map(fun parse_recipient/1, proplists:get_value(<<"cc">>, Headers, [])),
       bcc => lists:map(fun parse_recipient/1, proplists:get_value(<<"bcc">>, Headers, [])),
       references => proplists:get_value(<<"references">>, Headers, []),
       date => case proplists:get_value(<<"date">>, Headers) of
		   undefined -> 0;
		   [H, L | _ ] -> H*65536+L
	       end,
       size => proplists:get_value(<<"size">>, Headers, 0),
       msgid => proplists:get_value(<<"message-id">>, Headers),
       path => unicode:characters_to_list(proplists:get_value(<<"path">>, Headers)),
       flags => proplists:get_value(<<"flags">>, Headers, []) }.

parse_recipient([{<<"name">>, Name}, {<<"email">>, Email}]) -> [Name | Email];
parse_recipient([{<<"email">>, Email}, {<<"name">>, Name}]) -> [Name | Email];
parse_recipient([{<<"email">>, Email}]) -> [nil | Email];
parse_recipient(_) -> [<<>> | <<>>].

%% scrub all attachments
-spec scrub(string()) -> ok | {error, binary()}.
scrub(Path) ->
    mc_mender:deep_mend(fun mc_mender:scrub_part/1, Path).

%% graft a message to a new message-id as direct parent. That can be undefined, then
%% it will have no parent. Since we have the definite parent at hand, we will just clear
%% references header and use the passwd in message-id for in-reply-to. 
%% please not we will not touch children of it; if they have good in-reply-to then everything
%% shall come out ok, which should be the majority cases. If they have something bogus then they
%% could be orphaned. It is better to limit mending to the minimum.

-spec graft(string(), undefined | string()) -> ok | {error, binary()}.
graft(Path, Parent_id) ->
    Pid = msgid_str(Parent_id),
    mc_mender:mend(fun(Mail) -> mc_mender:set_parent(Mail, Pid) end, Path).

%% orphan a message so it has no parent

-spec orphan(string()) -> ok | {error, binary()}.
orphan(Path) ->
    mc_mender:mend(fun(Mail) -> mc_mender:set_parent(Mail, undefined) end, Path).

%% private functions
msgid_str(undefined) -> undefined;
msgid_str(Parent_id) -> unicode:characters_to_binary([$<, Parent_id, $>]).

%% parsr a contact into [Name | Mail]
parse_contact(Contact) ->
    case re:run(Contact, "(.*)<(.*)>", [{capture, all_but_first, binary}]) of
	nomatch -> [nil | Contact];
	{match, [N, Mail]} -> [string:trim(N, both, "\s\r\n\t\"") | Mail]
    end.
