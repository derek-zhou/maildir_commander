-module(mc_mu_api).

% this moddule wrap around mu server with erlang api

-export([find/1, find/2, find/3, find/4, find/5, find/6, find/7,
	 move/2, move/3, move/4, move/5,
	 add/1,
	 sent/1,
	 view/1, view/2,
	 compose/0, compose/2, compose/3,
	 remove/1,
	 mkdir/1,
	 ping/0,
	 quit/0,
	 contacts/0, contacts/1, contacts/2,
	 index/0, index/1, index/2 ]).
-export([fun_ending/1 ]).

find(Query) ->
    find(Query, mc_configer:default(threading)).

find(Query, Threads) ->
    find(Query, Threads, mc_configer:default(sort_field)).

find(Query, Threads, Sortfield) ->
    find(Query, Threads, Sortfield, mc_configer:default(descending_sort)).

find(Query, Threads, Sortfield, Descending) ->
    find(Query, Threads, Sortfield, Descending, mc_configer:default(max_num)).

find(Query, Threads, Sortfield, Descending, Maxnum) ->
    find(Query, Threads, Sortfield, Descending, Maxnum, mc_configer:default(skip_dups)).

find(Query, Threads, Sortfield, Descending, Maxnum, Skip_dups) ->
    find(Query, Threads, Sortfield, Descending, Maxnum, Skip_dups,
	 mc_configer:default(include_related)).

%% return a list of messages without the message-body
find(Query, Threads, Sortfield, Descending, Maxnum, Skip_dups, Include_related) ->
    [find,
     {<<"query">>, Query},
     {<<"threads">>, Threads},
     {<<"sortfield">>, Sortfield},
     {<<"descending">>, Descending},
     {<<"maxnum">>, Maxnum},
     {<<"skip-dups">>, Skip_dups},
     {<<"include-related">>, Include_related}].

move(Id, Maildir) ->
    move(Id, Maildir, undefined).

move(Id, Maildir, Flags) ->
    move(Id, Maildir, Flags, mc_configer:default(move_rename)).

move(Id, Maildir, Flags, Rename) ->
    move(Id, Maildir, Flags, Rename, mc_configer:default(move_no_view)).

move(Id, Maildir, Flags, Rename, No_view) ->
    [move,
     if is_integer(Id) -> {<<"docid">>, Id};
	true -> {<<"msgid">>, Id}
     end,
     if Flags == undefined -> {};
	true -> {<<"flags">>, Flags}
     end,
     if Maildir == undefined -> {};
	true -> {<<"maildir">>, Maildir}
     end,
     {<<"rename">>, Rename},
     {<<"no-view">>, No_view}
    ].

add(Path) ->
    [add, {<<"path">>, Path}].
    
sent(Path) ->
    [sent, {<<"path">>, Path}].
    
view(Id) ->
    view(Id, undefined).
    
view(Id, Path) ->
    [view,
     if Id == undefined -> {<<"path">>, Path};
	is_integer(Id) -> {<<"docid">>, Id};
	true -> {<<"msgid">>, Id}
     end].
    
compose() ->
    [compose, {<<"type">>, new}].

compose(Type, Docid) ->
    compose(Type, Docid, mc_configer:default(decrypt)).
compose(Type, Docid, Decrypt)
  when Type == forward; Type == reply; Type == edit; Type == resend ->
    [compose,
     {<<"type">>, Type},
     {<<"docid">>, Docid},
     {<<"decrypt">>, Decrypt}].

remove(Docid) ->
    [remove, {<<"docid">>, Docid}].

mkdir(Path) ->
    [mkdir, {<<"path">>, Path}].

ping() ->
    [ping].

contacts() ->
    contacts(mc_configer:default(contacts_personal)).

contacts(Personal) ->
    contacts(Personal, mc_configer:default(contacts_after)).

contacts(Personal, After) ->
    [contacts,
     {<<"personal">>, Personal},
     {<<"after">>, integer_to_binary(After)}].

index() ->
    index(mc_configer:default(index_cleanup)).

index(Cleanup) ->
    index(Cleanup, mc_configer:default(index_lazy_check)).

index(Cleanup, Lazy_check) ->
    [index,
     {<<"cleanup">>, Cleanup},
     {<<"lazy-check">>, Lazy_check}].

quit() ->
    [quit].

fun_ending([find | _]) ->
    fun([{<<"error">>, _} | _Tail]) -> true;
       ([{<<"found">>, _} | _Tail]) -> true;
       (_) -> false
    end;
fun_ending([index | _]) ->
    fun([{<<"error">>, _} | _Tail]) -> true;
       ([{<<"info">>, index}, {<<"status">>, complete} | _Tail]) -> true;
       (_) -> false
    end;
fun_ending([add | _]) ->
    fun([{<<"error">>, _} | _Tail]) -> true;
       ([{<<"update">>, _} | _Tail]) -> true;
       (_) -> false
    end;
fun_ending([view | _]) ->
    fun([{<<"error">>, _} | _Tail]) -> true;
       ([{<<"view">>, _} | _Tail]) -> true;
       (_) -> false
    end;
fun_ending(_) -> fun(_) -> true end.
