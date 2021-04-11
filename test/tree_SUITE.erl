-module(tree_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test_basic/1, test_graft/1]).

all() -> [test_basic, test_graft].

test_basic(_Config) ->
    Data = [{1, 0}, {2, 1}, {3, 1}, {4, 0}, {5, 1}, {6, 2}],
    Tin = lists:foldl(
	    fun ({N, L}, T) ->
		    mc_tree:append(N, L, T)
	    end, [], Data),
    Tree = {Root_list, Children_map, Parent_map} = mc_tree:finalize(Tin),
    [1, 4] = Root_list,
    #{1 := [2, 3], 4 := [5], 5 := [6]} = Children_map,
    #{2 := 1, 3 := 1, 5 := 4, 6 := 5} = Parent_map,
    [1, 2, 3, 4, 5, 6] = trace_tree(fun mc_tree:next/2, 1, Tree),
    [6, 5, 4, 3, 2, 1] = trace_tree(fun mc_tree:prev/2, 6, Tree),
    Data = mc_tree:flatmap(fun (N, L) -> {N, L} end, Tree).

test_graft(_Config) ->
    Data = [{1, 0}, {2, 1}, {3, 1}, {4, 0}, {5, 1}, {6, 2}],
    Tin = lists:foldl(
	    fun ({N, L}, T) ->
		    mc_tree:append(N, L, T)
	    end, [], Data),
    Tree = mc_tree:finalize(Tin),
    {R1, C1, P1} = mc_tree:graft(6, undefined, Tree),
    [1, 4, 6] = R1,
    #{1 := [2, 3], 4 := [5]} = C1,
    #{2 := 1, 3 := 1, 5 := 4} = P1,
    {R2, C2, P2} = mc_tree:graft(6, 1, Tree),
    [1, 4] = R2,
    #{1 := [2, 3, 6], 4 := [5]} = C2,
    #{2 := 1, 3 := 1, 6 := 1, 5 := 4} = P2.
    
trace_tree(Fun_next, Head, Tree) -> 
    case Fun_next(Head, Tree) of
	undefined -> [Head];
	Next -> [Head | trace_tree(Fun_next, Next, Tree)]
    end.
