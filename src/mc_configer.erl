-module(mc_configer).

-export([default_value/2]).

%% return the config value with default. Default can be a value or a zero arity function
%% to compute the default value
-spec default_value(atom(), term() | function()) -> term().
default_value(Key, Default) when is_function(Default, 0) ->
    case application:get_env(Key) of
	undefined ->
	    Def = Default(),
	    self_configer:set_env(?MODULE, Key, Def),
	    Def;
	{ok, Val} -> Val
    end;
default_value(Key, Default) ->
    case application:get_env(Key) of
	undefined ->
	    self_configer:set_env(?MODULE, Key, Default),
	    Default;
	{ok, Val} -> Val
    end.

