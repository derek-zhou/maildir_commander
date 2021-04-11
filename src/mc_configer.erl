-module(mc_configer).

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
				  [unicode:characters_to_list([User, $@, Trimmed])]
			  end
		  end);
default(threading) -> default_value(threading, false);
default(sort_field) -> default_value(sort_field, ':date');
default(max_num) -> default_value(max_num, 1024);
default(descending_sort) -> default_value(descending_sort, true);
default(skip_dups) -> default_value(skip_dups, false);
default(include_related) -> default_value(include_related, false);
default(move_new_name) -> default_value(move_new_name, false);
default(move_no_view) -> default_value(move_no_view, false);
default(extract_images) -> default_value(extract_images, false);
default(decrypt) -> default_value(decrypt, false);
default(verify) -> default_value(verify, true);
default(contacts_personal) -> default_value(contacts_personal, false);
default(contacts_after) -> default_value(contacts_after, 0);
default(index_cleanup) -> default_value(index_cleanup, true);
default(index_lazy_check) -> default_value(index_lazy_check, false);
default(archive_days) -> default_value(archive_days, 30);
default(archive_maildir) -> default_value(archive_maildir, "/.Archive");
default(socket_file) ->
    default_value(socket_file, os:getenv("HOME") ++ "/.mc_server_sock").

%% return the config value with default. Default can be a value or a zero arity function
%% to compute the default value
default_value(Key, Default) when is_function(Default, 0) ->
    {ok, App} = application:get_application(?MODULE),
    case application:get_env(App, Key) of
	undefined -> Default();
	{ok, Val} -> Val
    end;
default_value(Key, Default) ->
    {ok, App} = application:get_application(?MODULE),
    case application:get_env(App, Key) of
	undefined -> Default;
	{ok, Val} -> Val
    end.

