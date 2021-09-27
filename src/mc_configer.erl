-module(mc_configer).
-define(APP, maildir_commander).

-export([default/1]).

default(index_path) ->
    default_value(index_path, os:getenv("HOME") ++ "/Maildir");
default(my_addresses) ->
    default_value(my_addresses,
		  fun() ->
			  case file:read_file("/etc/mailname") of
			      {error, _Reason} -> [];
			      {ok, Binary} ->
				  Trimmed = string:trim(Binary, trailing),
				  User = os:getenv("USER"),
				  [unicode:characters_to_binary([User, $@, Trimmed])]
			  end
		  end);
default(threading) -> default_value(threading, false);
default(sort_field) -> default_value(sort_field, ':date');
default(max_num) -> default_value(max_num, 1024);
default(descending_sort) -> default_value(descending_sort, true);
default(skip_dups) -> default_value(skip_dups, false);
default(include_related) -> default_value(include_related, false);
default(move_rename) -> default_value(move_rename, false);
default(move_no_view) -> default_value(move_no_view, false);
default(contacts_personal) -> default_value(contacts_personal, false);
default(contacts_after) -> default_value(contacts_after, 0);
default(index_cleanup) -> default_value(index_cleanup, true);
default(index_lazy_check) -> default_value(index_lazy_check, false);
default(socket_file) ->
    default_value(socket_file, os:getenv("HOME") ++ "/.mc_server_sock");
default(Key) -> default_value(Key, undefined).

%% return the config value with default. Default can be a value or a zero arity function
%% to compute the default value
default_value(Key, Default) when is_function(Default, 0) ->
    case application:get_env(?APP, Key) of
	undefined -> Default();
	{ok, Val} -> Val
    end;
default_value(Key, Default) ->
    application:get_env(?APP, Key, Default).
