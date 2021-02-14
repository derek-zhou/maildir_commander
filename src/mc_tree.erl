-module(mc_tree).

%% this module deal with simple tree, each node is a either an Item (leave) or
%% {Item, [Children]} non-leave

-export([reverse/1, append/3, flatmap/2, children/2, parent/2]).

%% append an Item to the tree at level N
-spec append(term(), integer(), list()) -> list().
append(Item, 0, Tree) -> [Item | Tree];
append(Item, _N, []) -> [Item];
append(Item, N, [{Pid, Children} | Tail]) ->
    [{Pid, append(Item, N-1, Children)} | Tail];
append(Item, _N, [Pid | Tail]) -> [{Pid, [Item]} | Tail].

%% rebverse the order of a Tree
-spec reverse(list()) -> list().
reverse(Tree) -> lists:map(fun reverse_node/1, lists:reverse(Tree)).

%% return a list that apply Fun(Each, Level) to each item
-spec flatmap(function(), list()) -> list().
flatmap(Fun, Tree) -> flatmap(Fun, 0, Tree).

%% get the children of an Item. This is O(n)
-spec children(term(), list()) -> list().
children(_Item, []) -> [];
children(Item, [{Item, Children} | _ ]) -> Children; 
children(Item, [Item | _]) -> []; 
children(Item, [{_Head, H_children} | Tail]) ->
    case children(Item, H_children) of
	[] -> children(Item, Tail);
	List -> List
    end;
children(Item, [_Head | Tail]) -> children(Item, Tail).
    
%% get the parent of an Item. This is O(n). default is undefined
-spec parent(term(), list()) -> undefined | term().
parent(Item, Tree) -> parent(Item, undefined, Tree).

%% private functions
reverse_node({Item, Children}) -> {Item, reverse(Children)};
reverse_node(Item) -> Item.

flatmap(Fun, Level, Tree) ->
    lists:map(fun(Each) -> flatmap_node(Fun, Level, Each) end, Tree).

flatmap_node(Fun, Level, {Item, Children}) ->
    [Fun(Item, Level) | flatmap(Fun, Level + 1, Children)];
flatmap_node(Fun, Level, Item) -> Fun(Item, Level).

parent(_Item, _Default, []) -> undefined;
parent(Item, Default, [{Item, _Children} | _Tail]) -> Default; 
parent(Item, Default, [Item | _Tail]) -> Default; 
parent(Item, Default, [{Head, H_children} | Tail]) ->
    case parent(Item, Head, H_children) of
	undefined -> parent(Item, Default, Tail);
	Parent -> Parent
    end;
parent(Item, Default, [_Head | Tail]) -> parent(Item, Default, Tail).

	    
    
