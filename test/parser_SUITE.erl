-module(parser_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test_basic/1, test_print/1, test_special/1]).

all() -> [test_basic, test_print, test_special].

test_basic(_Config) ->
    27 = mc_sexp:parse("27"),
    [hi, 'there-sth'] = mc_sexp:parse("(hi there-sth)"),
    [{<<"a">>, 1}, {<<"b">>, 2}] = mc_sexp:parse("(:a 1 :b 2)"),
    [cmd, {<<"b">>, 2}] = mc_sexp:parse("(cmd :b 2)"),
    [<<"cmd">> | 2] = mc_sexp:parse("(\"cmd\" . 2)").

test_print(_Config) ->
    parse_print_verify("(cmd)"),
    parse_print_verify("(cmd :a 1 :b 2)"),
    parse_print_verify("(cmd :a \"haha\" :b (3 4))").

test_special(_Config) ->
    Sexp = [{<<"a">>, true}, {<<"b">>, false}] = mc_sexp:parse("(:a true :b false)"),
    Str = mc_sexp:to_string(Sexp),
    true = string:equal(Str, "(:a t :b nil)").

parse_print_verify(Str) ->
    Sexp = mc_sexp:parse(Str),
    Str2 = mc_sexp:to_string(Sexp),
    true = string:equal(Str, Str2).


    

