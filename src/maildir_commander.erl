-module(maildir_commander).

%% public interface of maildir_commander

-export([index/0, add/1, contacts/0]).

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

