-module(maildir_commander).

-include_lib("kernel/include/logger.hrl").

%% public interface of maildir_commander

-export([index/0, add/1, contacts/0,
	 find/1, find/2, find/3, find/4]).

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
	{async, [{<<"info">>, index} | _ ]} -> index_loop()
    end.

%% add a message to the database. it must has the full path and is already be in the Maildir.
%% return {ok, Docid} where Docid is the docid of the message in the database,
%% or {error, Msg} where Msg is a binary string for the error
-spec add(string()) -> {ok, integer()} | {error, binary()}.
add(Path) ->
    case mc_mender:maildir_parse(Path) of
	{error, Reason} -> {error, Reason};
	{cur, Maildir, _Basename} ->
	    %% the sent function name is from mu. add return too much crap
	    Command = mc_mu_api:sent(Path, Maildir),
	    mc_server:command(Command),
	    add_loop();
	{_Type, _Maildir, _Basename} -> {error, <<"Not in cur sub dir">>}
    end.

add_loop() ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"sent">>, t} | Rest ]} ->
	    {ok, proplists:get_value(<<"docid">>, Rest)}
    end.

%% return all contacts in the database
-spec contacts() -> {ok, [{term(), binary()}]} | {error, binary()}.
contacts() ->
    Command = mc_mu_api:contacts(),
    ok = mc_server:command(Command),
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"contacts">>, Contacts}]} ->
	    {ok, lists:map(
		   fun (Each) ->
			   Mail = proplists:get_value(<<"mail">>, Each),
			   case proplists:get_value(<<"name">>, Each) of
			       nil -> {<<>>, Mail};
			       Name -> {Name, Mail}
			   end
		   end, Contacts)};
	{async, [{<<"info">>, index} | _ ]} -> index_loop()
    end.

%% return all mail matching the query. return a tree of docids, a map from docid to messages,
%% and a map from docid to parent docid
-spec find(string()) ->
	  {ok, list(), map(), map()} | {error, binary()}.
find(Query) -> find(Query, false).

-spec find(string(), boolean()) ->
	  {ok, [proplist:proplist()]} | {error, binary()}.
find(Query, Threads) -> find(Query, Threads, default(sort_field)).

-spec find(string(), boolean(), string()) ->
	  {ok, [proplist:proplist()]} | {error, binary()}.
find(Query, Threads, Sort_field) -> find(Query, Threads, Sort_field, false).

-spec find(string(), boolean(), string(), boolean()) ->
	  {ok, [proplist:proplist()]} | {error, binary()}.
find(Query, true, Sort_field, Reverse_sort) ->
    Command = mc_mu_api:find(Query, true, Sort_field, Reverse_sort,
			     default(max_num), true, true),
    ok = mc_server:command(Command),
    find_loop([], #{}, #{});
find(Query, false, Sort_field, Reverse_sort) ->
    Command = mc_mu_api:find(Query, false, Sort_field, Reverse_sort),
    ok = mc_server:command(Command),
    case find_loop([], #{}, #{}) of
	{error, Msg} -> {error, Msg};
	{ok, List, Mails, #{}} -> {ok, List, Mails}
    end.

find_loop(Tree, Mails, Parents) ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} ->
	    {error, Msg};
	{async, [{<<"erase">>, t} | _ ]} ->
	    find_loop([], #{}, #{});
	{async, [{<<"found">>, _Total} | _ ]} ->
	    {ok, reverse_tree(Tree), Mails, Parents};
	{async, [{<<"docid">>, Docid} | Headers ]} ->
	    Mail = parse_mail_headers(Headers),
	    {Level, Should_patch} = parse_thread_level(Headers),
	    %% for first orphan we have to attach a dummy parent
	    Patched = if Should_patch -> merge_into_tree(undefined, Level - 1, Tree);
			 true -> Tree
		      end,
	    find_loop(merge_into_tree(Docid, Level, Patched),
		      maps:put(Docid, Mail, Mails),
		      case parent_in_tree(Level, undefined, Patched) of
			  undefined -> Parents;
			  Parent -> maps:put(Docid, Parent, Parents)
		      end)
    end.

parse_thread_level(Headers) ->
    case proplists:get_value(<<"thread">>, Headers) of
	undefined -> {0, false};
	Thread ->
	    Level = proplists:get_value(<<"level">>, Thread, 0),
	    First_child = proplists:get_value(<<"first-child">>, Thread),
	    Empty_parent = proplists:get_value(<<"empty-parent">>, Thread),
	    %% only make dummy parent when I am the first of orphan siblings
	    {Level, (First_child == t) and (Empty_parent == t)}
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
       date => case proplists:get_value(<<"date">>, Headers) of
		   undefined -> 0;
		   [H, L | _ ] -> H*65536+L
	       end,
       size => proplists:get_value(<<"size">>, Headers, 0),
       msgid => proplists:get_value(<<"message-id">>, Headers),
       path => proplists:get_value(<<"path">>, Headers),
       flags => proplists:get_value(<<"flags">>, Headers, []) }.

merge_into_tree(Docid, 0, Tree) -> [Docid | Tree];
merge_into_tree(Docid, _N, []) -> [Docid];
merge_into_tree(Docid, N, [{Pid, Children} | Tail]) ->
    [{Pid, merge_into_tree(Docid, N-1, Children)} | Tail];
merge_into_tree(Docid, _N, [Pid | Tail]) -> [{Pid, [Docid]} | Tail].

parent_in_tree(0, Parent, _Tree) -> Parent;
parent_in_tree(_N, Parent, []) -> Parent;
parent_in_tree(N, _Parent, [{Pid, Children} | _Tail]) ->
    parent_in_tree(N-1, Pid, Children);
parent_in_tree(_N, _Parent, [Pid | _Tail]) -> Pid.

%% rebverse the order of a Tree
reverse_tree(Tree) -> lists:map(fun reverse_node/1, lists:reverse(Tree)).

reverse_node({Docid, Children}) -> {Docid, reverse_tree(Children)};
reverse_node(Docid) -> Docid.

default(sort_field) -> mc_configer:default_value(sort_field, "subject");
default(max_num) -> mc_configer:default_value(max_num, 1024).
