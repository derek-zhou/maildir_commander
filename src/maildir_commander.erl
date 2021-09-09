-module(maildir_commander).

-include_lib("kernel/include/logger.hrl").

%% public interface of maildir_commander

-export([index/0, add/1, delete/1, contacts/0, full_mail/1, extract/3,
	 find/1, find/2, find/3, find/4, find/5, find/6, flag/2, move/2,
	 scrub/1, scrub/2, graft/2, graft/3, orphan/1, orphan/2]).

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
	    {ok, proplists:get_value(<<"processed">>, Rest)};
	%% last 2 are chatty messages that we don't care
	{async, [{<<"update">>, _} | _ ]} -> ?FUNCTION_NAME();
	{async, [{<<"info">>, _} | _ ]} -> ?FUNCTION_NAME()
    end.

%% add a message to the database. it must has the full path and is already be in the Maildir.
%% return ok if successful or {error, Msg} where Msg is a binary string for the error
-spec add(string()) -> ok | {error, binary()}.
add(Path) ->
    %% the sent function name is from mu. add return too much crap
    Command = mc_mu_api:sent(unicode:characters_to_binary(Path)),
    mc_server:command(Command),
    add_loop().

add_loop() ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"sent">>, t} | _Rest ]} -> ok;
	%% last 2 are chatty messages that we don't care
	{async, [{<<"update">>, _} | _ ]} -> ?FUNCTION_NAME();
	{async, [{<<"info">>, _} | _ ]} -> ?FUNCTION_NAME()
    end.

%% delete a message from both the filesystem and the database
-spec delete(integer()) -> ok | {error, binary()}.
delete(Docid) ->
    Command = mc_mu_api:remove(Docid),
    ok = mc_server:command(Command),
    remove_loop().

remove_loop() ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"remove">>, _} | _Rest ]} -> ok;
	%% last 2 are chatty messages that we don't care
	{async, [{<<"update">>, _} | _ ]} -> ?FUNCTION_NAME();
	{async, [{<<"info">>, _} | _ ]} -> ?FUNCTION_NAME()
    end.

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
	    {ok, lists:map(
		   fun ([Contact | _]) ->
			   parse_contact(Contact)
		   end, Contacts)};
	%% last 2 are chatty messages that we don't care
	{async, [{<<"update">>, _} | _ ]} -> ?FUNCTION_NAME();
	{async, [{<<"info">>, _} | _ ]} -> ?FUNCTION_NAME()
    end.

%% return all attachments of a mail as a list of {Name, Mime_type, Binary}
-spec extract(integer(), list(), list()) -> list().
extract(Docid, Parts, Temp) ->
    lists:map(
      fun({Index, Name, Type}) ->
	      case extract_one_part(Docid, Index, Temp) of
		  {error, Msg} -> {Name, error, Msg};
		  {ok, Binary} -> {Name, Type, Binary}
	      end
      end, Parts).

extract_one_part(Docid, Index, Path) ->
    Command = mc_mu_api:extract(save, Docid, Index, Path),
    ok = mc_server:command(Command),
    case extract_loop() of
	{error, Msg} -> {error, Msg};
	ok ->
	    {ok, Binary} = file:read_file(Path),
	    ok = file:delete(Path),
	    {ok, Binary}
    end.

extract_loop() ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"info">>, save} | _ ]} -> ok;
	%% last 2 are chatty messages that we don't care
	{async, [{<<"update">>, _} | _ ]} -> ?FUNCTION_NAME();
	{async, [{<<"info">>, _} | _ ]} -> ?FUNCTION_NAME()
    end.

%% return full body of a mail in a tuple {Headers, Text, Html}
-spec full_mail(integer()) -> {map(), binary(), binary()} | {error, binary()}.
full_mail(Docid) ->
    Command = mc_mu_api:view(Docid),
    ok = mc_server:command(Command),
    case view_loop() of
	{error, Msg} -> {error, Msg};
	Mail ->
	    { parse_mail_headers(Mail),
	      proplists:get_value(<<"body-txt">>, Mail, <<"">>),
	      proplists:get_value(<<"body-html">>, Mail, <<"">>),
	      parse_parts(proplists:get_value(<<"parts">>, Mail, [])) }
    end.

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

%% change the flags of a mail
-spec flag(integer(), string()) -> ok | {error, binary()}.
flag(Docid, Flags) ->
    Command = mc_mu_api:move(Docid, undefined,
			     unicode:characters_to_binary(Flags)),
    ok = mc_server:command(Command),
    move_loop().

%% move a mail to another location
-spec move(integer(), string()) -> ok | {error, binary()}.
move(Docid, Maildir) ->
    Command = mc_mu_api:move(Docid, Maildir),
    ok = mc_server:command(Command),
    move_loop().

move_loop() ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"update">>, _} | _ ]} -> ok
    end.

view_loop() ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"view">>, [{<<"docid">>, _Docid} | Mail ] } | _ ]} -> Mail
    end.

find_loop(Tree, Mails) ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"erase">>, t} | _ ]} -> find_loop([], #{});
	{async, [{<<"found">>, _Total} | _ ]} -> {ok, mc_tree:finalize(Tree), Mails};
	{async, [{<<"docid">>, Docid} | Headers ]} when is_integer(Docid) ->
	    find_loop(mc_tree:append(Docid, parse_thread_level(Headers), Tree),
		      maps:put(Docid, parse_mail_headers(Headers), Mails));
	%% last 2 are chatty messages that we don't care
	{async, [{<<"update">>, _} | _ ]} -> find_loop(Tree, Mails);
	{async, [{<<"info">>, _} | _ ]} -> find_loop(Tree, Mails)
    end.

parse_thread_level(Headers) ->
    case proplists:get_value(<<"thread">>, Headers) of
	undefined -> 0;
	Thread ->
	    case proplists:get_value(<<"level">>, Thread, 0) of
		0 -> 0;
		Level ->
		    case proplists:get_value(<<"empty-parent">>, Thread, nil) of
			t -> 0;
			_ -> Level
		    end
	    end
    end.

parse_mail_headers(Headers) ->
    #{ subject => proplists:get_value(<<"subject">>, Headers, <<>>),
       %% from is a list of email addresses. Pick the first one
       from => case proplists:get_value(<<"from">>, Headers) of
		   undefined -> undefined;
		   [] -> [<<>> | <<>>];
		   [Add | _] -> Add
	       end,
       to => proplists:get_value(<<"to">>, Headers, []),
       cc => proplists:get_value(<<"cc">>, Headers, []),
       bcc => proplists:get_value(<<"bcc">>, Headers, []),
       references => proplists:get_value(<<"references">>, Headers, []),
       date => case proplists:get_value(<<"date">>, Headers) of
		   undefined -> 0;
		   [H, L | _ ] -> H*65536+L
	       end,
       size => proplists:get_value(<<"size">>, Headers, 0),
       msgid => proplists:get_value(<<"message-id">>, Headers),
       path => unicode:characters_to_list(proplists:get_value(<<"path">>, Headers)),
       flags => proplists:get_value(<<"flags">>, Headers, []) }.

parse_parts(Parts) ->
    lists:filtermap(
      fun(Part) ->
	      case proplists:get_value(<<"attachment">>, Part) of
		  t ->
		      {true, {proplists:get_value(<<"index">>, Part),
			      proplists:get_value(<<"name">>, Part),
			      proplists:get_value(<<"mime-type">>, Part)}};
		  _ -> false
	      end
      end, Parts).

%% scrub all attachments
-spec scrub(string()) -> ok | {error, binary()}.
scrub(Path) ->
    mc_mender:leaf_mend(fun mc_mender:scrub_mime/1, Path).

-spec scrub(string(), string()) -> ok | {error, binary()}.
scrub(Path, Maildir) ->
    mc_mender:leaf_mend(fun mc_mender:scrub_mime/1, Path, Maildir).

%% graft a message to a new message-id as direct parent. That can be undefined, then
%% it will have no parent. Since we have the definite parent at hand, we will just clear
%% references header and use the passwd in message-id for in-reply-to. 
%% please not we will not touch children of it; if they have good in-reply-to then everything
%% shall come out ok, which should be the majority cases. If they have something bogus then they
%% could be orphaned. It is better to limit mending to the minimum.

-spec graft(string(), undefined | string()) -> ok | {error, binary()}.
graft(Path, Parent_id) ->
    Pid = msgid_str(Parent_id),
    mc_mender:mend(fun(Mime) -> mc_mender:set_mime_parent(Mime, Pid) end, Path).

-spec graft(string(), undefined | string(), string()) -> ok | {error, binary()}.
graft(Path, Parent_id, Maildir) ->
    Pid = msgid_str(Parent_id),
    mc_mender:mend(fun(Mime) -> mc_mender:set_mime_parent(Mime, Pid) end, Path, Maildir).

%% orphan a message so it has no parent

-spec orphan(string()) -> ok | {error, binary()}.
orphan(Path) ->
    mc_mender:mend(fun(Mime) -> mc_mender:set_mime_parent(Mime, undefined) end, Path).

-spec orphan(string(), string()) -> ok | {error, binary()}.
orphan(Path, Maildir) ->
    mc_mender:mend(fun(Mime) -> mc_mender:set_mime_parent(Mime, undefined) end, Path, Maildir).

%% private functions
msgid_str(undefined) -> undefined;
msgid_str(Parent_id) -> unicode:characters_to_binary([$<, Parent_id, $>]).

%% parsr a contact into [Name | Mail]
parse_contact(Contact) ->
    case re:run(Contact, "(.*)<(.*)>", [{capture, all_but_first, binary}]) of
	nomatch -> [<<"">> | Contact];
	{match, [N, Mail]} -> [string:trim(N, both, "\s\r\n\t\"") | Mail]
    end.
