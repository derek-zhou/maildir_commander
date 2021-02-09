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
    Dir = mc_mu_api:default(index_path),
    case get_maildir(Path, Dir) of
	undefined -> {error, <<"Not in maildir">>};
	Maildir ->
	    %% the sent function name is from mu. add return too much crap
	    Command = mc_mu_api:sent(Path, Maildir),
	    mc_server:command(Command),
	    add_loop()
    end.

get_maildir(Path, Dir) ->
    case string:prefix(Path, Dir) of
	nomatch -> undefined;
	Remain ->
	    case string:split(Remain, "/cur/") of
		[_File] -> undefined;
		[[], _File] -> "/";
		[Maildir, _File] -> Maildir
	    end
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

%% return all mail matching the query. no threading. return a list of proplist for each mail
-spec find(string()) ->
	  {ok, [proplists:proplist()]} | {error, binary()}.
find(Query) -> find(Query, false).

-spec find(string(), boolean()) ->
	  {ok, [proplist:proplist()]} | {error, binary()}.
find(Query, Threads) -> find(Query, Threads, mc_mu_api:default(sort_field)).

-spec find(string(), boolean(), string()) ->
	  {ok, [proplist:proplist()]} | {error, binary()}.
find(Query, Threads, Sort_field) -> find(Query, Threads, Sort_field, false).

-spec find(string(), boolean(), string(), boolean()) ->
	  {ok, [proplist:proplist()]} | {error, binary()}.
find(Query, Threads, Sort_field, Reverse_sort) ->
    Command = mc_mu_api:find(Query, Threads, Sort_field, Reverse_sort,
			     mc_mu_api:default(max_num), Threads, Threads),
    ok = mc_server:command(Command),
    find_loop([]).

find_loop(List) ->
    receive
	{async, [{<<"error">>, _Code}, {<<"message">>, Msg} | _ ]} -> {error, Msg};
	{async, [{<<"erase">>, t} | _ ]} -> find_loop([]);
	{async, [{<<"found">>, _Total} | _ ]} -> {ok, reverse_mails(List)};
	{async, [{<<"docid">>, _Docid} | Headers ]} ->
	    Mail = parse_mail_headers(Headers),
	    {Level, Duplicate, First_child, Empty_parent} = parse_thread_level(Headers),
	    %% for first orphan we have to attach a dummy parent
	    Patched = if First_child and Empty_parent ->
			      merge_mail_into_tree(dummy_parent(), Level - 1, List);
			 true -> List
		      end,
	    %% remove dups. There could be dups even if we specified skip-dups
	    Inserted = if Duplicate -> Patched;
			  true -> merge_mail_into_tree(Mail, Level, Patched)
		       end,
	    find_loop(Inserted)
    end.

parse_thread_level(Headers) ->
    case proplists:get_value(<<"thread">>, Headers) of
	undefined -> {0, false, false, false};
	Thread ->
	    Level = proplists:get_value(<<"level">>, Thread, 0),
	    Duplicate = proplists:get_value(<<"duplicate">>, Thread, false),
	    First_child = proplists:get_value(<<"first-child">>, Thread, false),
	    Empty_parent = proplists:get_value(<<"empty-parent">>, Thread, false),
	    %% only make dummy parent when I am the first of orphan siblings
	    {Level, Duplicate == t, First_child == t, Empty_parent == t}
    end.

parse_mail_headers(Headers) ->
    Subject = proplists:get_value(<<"subject">>, Headers, <<>>),
    %% from is a list of email addresses. Pick the first one
    From = case proplists:get_value(<<"from">>, Headers) of
	       undefined -> undefined;
	       [] -> [<<>> | <<>>];
	       [Add | _] -> Add
	   end,
    To = proplists:get_value(<<"to">>, Headers, []),
    Cc = proplists:get_value(<<"cc">>, Headers, []),
    Bcc = proplists:get_value(<<"bcc">>, Headers, []),
    Date =
	case proplists:get_value(<<"date">>, Headers) of
	    undefined -> 0;
	    [H, L | _ ] -> H*65536+L
	end,
    Size = proplists:get_value(<<"size">>, Headers, 0),
    Path = proplists:get_value(<<"path">>, Headers),
    Flags = proplists:get_value(<<"flags">>, Headers, []),
    [{subject, Subject},
     {from, From},
     {to, To},
     {cc, Cc},
     {bcc, Bcc},
     {date, Date},
     {size, Size},
     {path, Path},
     {flags, Flags} ].

dummy_parent() ->
    [{subject, <<>>},
     {from, [<<>> | <<>>]},
     {to, []},
     {cc, []},
     {bcc, []},
     {date, 0},
     {size, 0},
     {path, undefined},
     {flags, []} ].

merge_mail_into_tree(Mail, 0, List) -> [Mail | List];
merge_mail_into_tree(Mail, N, [])
  when is_integer(N) andalso N > 0 ->
    ?LOG_WARNING("Cannot merge message ~ts into the tree",
		 [proplists:get_value(path, Mail, <<>>)]),
    [Mail];
merge_mail_into_tree(Mail, N, [[{children, Children} | Head] | Tail])
  when is_integer(N) andalso N > 0 ->
    [[{children, merge_mail_into_tree(Mail, N-1, Children)} | Head] | Tail];
merge_mail_into_tree(Mail, N, [Head | Tail])
  when is_integer(N) andalso N > 0 ->
    merge_mail_into_tree(Mail, N, [[{children, []} | Head] | Tail]).

%% rebverse the order of a list of mails, aldo fixup each mail
reverse_mails(List) ->
    lists:map(fun reverse_children/1, lists:reverse(List)).

%% reverse children of a mail to the original order
reverse_children([{children, Children} | Rest]) ->
    [{children, reverse_mails(Children)} | Rest];
reverse_children(List) when is_list(List) -> List.
