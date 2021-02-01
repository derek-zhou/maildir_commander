-module(mc_mu_api).
-define(SERVER, mc_server).

% this moddule wrap around mu server with erlang api

-export([find/1, find/2, find/3, find/4, find/5, find/6, find/7,
	 move/2, move/3, move/4,
	 add/2,
	 sent/2,
	 view/1, view/2, view/3, view/4, view/5,
	 compose/0, compose/2, compose/3, compose/4,
	 extract/3, extract/4, extract/5, extract/6, extract/7,
	 remove/1,
	 mkdir/1,
	 ping/0,
	 contacts/0, contacts/1, contacts/2,
	 index/0, index/1, index/2, index/3, index/4 ]).
-export([init_command/0, fun_ending/1, cmd_of/1, default/1 ]).

find(Query) ->
    find(Query, default(threading)).

find(Query, Threads) ->
    find(Query, Threads, default(sort_field)).

find(Query, Threads, Sortfield) ->
    find(Query, Threads, Sortfield, default(reverse_sort)).

find(Query, Threads, Sortfield, Reverse) ->
    find(Query, Threads, Sortfield, Reverse, default(max_num)).

find(Query, Threads, Sortfield, Reverse, Maxnum) ->
    find(Query, Threads, Sortfield, Reverse, Maxnum, default(skip_dups)).

find(Query, Threads, Sortfield, Reverse, Maxnum, Skip_dups) ->
    find(Query, Threads, Sortfield, Reverse, Maxnum, Skip_dups, default(include_related)).

find(Query, Threads, Sortfield, Reverse, Maxnum, Skip_dups, Include_related) ->
    Command = [{cmd, "find"}, 
	       {query, base64:encode(unicode:characters_to_binary(Query))},
	       {threads, Threads},
	       {sortfield, Sortfield},
	       {reverse, Reverse},
	       {maxnum, Maxnum},
	       {'skip-dups', Skip_dups},
	       {'include-related', Include_related}],
    Sexps = gen_server:call(?SERVER, {mu_server, Command}),
    case proplists:is_defined(error, hd(Sexps)) of
	true -> hd(Sexps);  %% error
	%% date is the signature field of a valid found results
	false -> lists:filter(fun(Each) -> proplists:is_defined(date, Each) end, Sexps)
    end.    

move(Id, Maildir) ->
    move(Id, Maildir, undefined).

move(Id, Maildir, Flags) ->
    move(Id, Maildir, Flags, default(move_new_name)).

move(Id, Maildir, Flags, Newname) -> 
    Command = if is_integer(Id) -> [{cmd, "move"}, {docid, Id}];
		 true -> [{cmd, "move"}, {msgid, Id}]
	      end,
    Command2 = if Maildir == undefined -> Command;
		  true -> [Command ++ [{maildir, Maildir}]]
	       end,
    Command3 = if Flags == undefined -> Command2;
		  true -> [Command2 ++ [{flags, Flags}]]
	       end,
    Command4 = [Command3 ++ [{newname, Maildir /= undefined and Newname}]],
    [Sexp] = gen_server:call(?SERVER, {mu_server, Command4}),
    Sexp.

add(Path, Maildir) ->
    Command = [{cmd, "add"}, {path, Path}, {maildir, Maildir}],
    Sexps = gen_server:call(?SERVER, {mu_server, Command}),
    lists:last(Sexps).
    
sent(Path, Maildir) ->
    Command = [{cmd, "sent"}, {path, Path}, {maildir, Maildir}],
    [Sexp] = gen_server:call(?SERVER, {mu_server, Command}),
    Sexp.
    
view(Id) ->
    view(Id, undefined).
    
view(Id, Path) ->
    view(Id, Path, default(extract_images)).
    
view(Id, Path, Extract_images) ->
    view(Id, Path, Extract_images, default(extract_encrypted)).
    
view(Id, Path, Extract_images, Extract_encrypted) ->
    view(Id, Path, Extract_images, Extract_encrypted, default(use_agent)).
    
view(Id, Path, Extract_images, Extract_encrypted, Use_agent) ->
    Id_part =
	if Id == undefined -> {path, Path};
	   is_integer(Id) -> {docid, Id};
	   true -> {msgid, Id}
	end,
    Command = [{cmd, "view"}, Id_part] ++
	[{'extract-images', Extract_images},
	 {'extract-encrypted', Extract_encrypted},
	 {'use-agent', Use_agent}],
    [Sexp] = gen_server:call(?SERVER, {mu_server, Command}),
    Sexp.
    
compose() ->
    [Sexp] = gen_server:call(?SERVER, {mu_server, [{cmd, "compose"}, {type, "new"}]}),
    Sexp.
compose(Type, Docid) ->
    compose(Type, Docid, default(extract_encrypted)).
compose(Type, Docid, Extract_encrypted) ->
    compose(Type, Docid, Extract_encrypted, default(use_agent)).
compose(Type, Docid, Extract_encrypted, Use_agent)
  when Type == forward; Type == reply; Type == edit; Type == resend ->
    Command = [{cmd, "compose"},
	       {type, Type},
	       {docid, Docid},
	       {'extract-encrypted', Extract_encrypted},
	       {'use-agent', Use_agent}],
    [Sexp] = gen_server:call(?SERVER, {mu_server, Command}),
    Sexp.

extract(open, Docid, Index) ->
    extract(open, Docid, Index, default(extract_encrypted)).

extract(temp, Docid, Index, What) ->
    extract(temp, Docid, Index, What, undefined);
extract(open, Docid, Index, Extract_encrypted) ->
    extract(open, Docid, Index, Extract_encrypted, default(use_agent));
extract(save, Docid, Index, Path) ->
    extract(save, Docid, Index, Path, default(extract_encrypted)).

extract(temp, Docid, Index, What, Param) ->
    extract(temp, Docid, Index, What, Param, default(extract_encrypted));
extract(save, Docid, Index, Path, Extract_encrypted) ->
    extract(save, Docid, Index, Path, Extract_encrypted, default(use_agent));
extract(open, Docid, Index, Extract_encrypted, Use_agent) ->
    Command = [{cmd, "extract"},
	       {action, "open"},
	       {docid, Docid},
	       {index, Index},
	       {'extract-encrypted', Extract_encrypted},
	       {'use-agent', Use_agent}],
    [Sexp] = gen_server:call(?SERVER, {mu_server, Command}),
    Sexp.

extract(temp, Docid, Index, What, Param, Extract_encrypted) ->
    extract(temp, Docid, Index, What, Param, Extract_encrypted, default(use_agent));
extract(save, Docid, Index, Path, Extract_encrypted, Use_agent) ->
    Command = [{cmd, "extract"},
	       {action, "save"},
	       {docid, Docid},
	       {index, Index},
	       {path, Path},
	       {'extract-encrypted', Extract_encrypted},
	       {'use-agent', Use_agent}],
    [Sexp] = gen_server:call(?SERVER, {mu_server, Command}),
    Sexp.

extract(temp, Docid, Index, What, Param, Extract_encrypted, Use_agent) ->
    Command = [{cmd, "extract"},
	       {action, "temp"},
	       {docid, Docid},
	       {index, Index},
	       {what, What}],
    Command2 = if Param == undefined -> Command;
		  true -> Command ++ [{param, Param}]
	       end,
    Command3 = Command2 ++ [{'extract-encrypted', Extract_encrypted},
			   {'use-agent', Use_agent}],
    [Sexp] = gen_server:call(?SERVER, {mu_server, Command3}),
    Sexp.
    
remove(Docid) ->
    Command = [{cmd, "remove"}, {docid, Docid}],
    [Sexp] = gen_server:call(?SERVER, {mu_server, Command}),
    Sexp.

mkdir(Path) ->
    Command = [{cmd, "mkdir"}, {path, Path}],
    [Sexp] = gen_server:call(?SERVER, {mu_server, Command}),
    Sexp.

ping() ->
    [Sexp] = gen_server:call(?SERVER, {mu_server, [{cmd, "ping"}]}),
    Sexp.

contacts() ->
    contacts(default(contacts_personal)).

contacts(Personal) ->
    contacts(Personal, default(contacts_after)).

contacts(Personal, After) ->
    Command = [{cmd, "contacts"}, {personal, Personal}, {'after', After}],
    [{contacts, List}] = gen_server:call(?SERVER, {mu_server, Command}),
    List.
    
index() ->
    index(default(index_path)).

index(Path) ->
    index(Path, default(my_addresses)).

index(Path, My_addresses) ->
    index(Path, My_addresses, default(index_cleanup)).

index(Path, My_addresses, Cleanup) ->
    index(Path, My_addresses, Cleanup, default(index_lazy_check)).

index(Path, My_addresses, Cleanup, Lazy_check) ->
    Command = [{cmd, "index"}, {path, Path}],
    Command2 = if My_addresses == [] -> Command;
		  true -> Command ++ [{'my-address', lists:join($,, My_addresses)}]
	       end,
    Command3 = Command2 ++ [{cleanup, Cleanup}, {'lazy-check', Lazy_check}],
    Sexps = gen_server:call(?SERVER, {mu_server, Command3}),
    lists:last(Sexps).

init_command() ->
    Command = [{cmd, "index"}, {path, default(index_path)}],
    case default(my_addresses) of
	[] ->Command;
	My_addresses -> Command ++ [{'my-address', lists:join($,, My_addresses)}]
    end.

cmd_of(Command) ->
    unicode:characters_to_list(proplists:get_value(cmd, Command)).

fun_ending(Command) ->
    case cmd_of(Command) of
	"find" ->
	    fun(Sexp) ->
		    proplists:is_defined(error, Sexp) orelse
			proplists:is_defined(found, Sexp)
	    end;
	"index" ->
	    fun(Sexp) ->
		    proplists:is_defined(error, Sexp) orelse
			proplists:get_value(status, Sexp, undefined) == complete
	    end;
	"add" ->
	    fun(Sexp) ->
		    proplists:is_defined(error, Sexp) orelse
			proplists:get_value(update, Sexp)
	    end;
	_  ->
	    fun(_Sexp) -> true end
    end.    

%% internal functions

default_value(Key, Default) when is_function(Default, 0) ->
    case application:get_env(?MODULE) of
	undefined -> Default();
	{ok, Map} ->
	    case maps:get(Key, Map, undefined) of
		undefined -> Default();
		Value -> Value
	    end
    end;
default_value(Key, Default) ->
    case application:get_env(?MODULE) of
	undefined -> Default;
	{ok, Map} -> maps:get(Key, Map, Default)
    end.
    
default(index_path) ->
    default_value(index_path, [os:getenv("HOME"), "/Maildir"]);
default(my_addresses) ->
    default_value(my_addresses,
		 fun() ->
			 case file:read_file("/etc/mailname") of
			     {error, _Reason} -> [];
			     {ok, Binary} ->
				 Trimmed = string:trim(Binary, trailing),
				 User = os:getenv("USER"),
				 [[User, $@, Trimmed]]
			 end
		 end);
default(threading) -> default_value(threading, false);
default(sort_field) -> default_value(sort_field, "subject");
default(max_num) -> default_value(max_num, 1024);
default(reverse_sort) -> default_value(reverse_sort, false); 
default(skip_dups) -> default_value(skip_dups, false); 
default(include_related) -> default_value(include_related, false); 
default(move_new_name) -> default_value(move_new_name, false); 
default(extract_images) -> default_value(extract_images, false); 
default(extract_encrypted) -> default_value(extract_encrypted, false); 
default(use_agent) -> default_value(use_agent, true); 
default(contacts_personal) -> default_value(contacts_personal, true); 
default(contacts_after) -> default_value(contacts_after, 0);
default(index_cleanup) -> default_value(index_cleanup, true);
default(index_lazy_check) -> default_value(index_lazy_check, false). 
