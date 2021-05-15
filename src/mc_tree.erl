-module(mc_tree).

%% this module deal with simple tree. At building phase it is a list of root nodes
%% and each node is a either an Item (leave) or {Item, [Children]} non-leave
%% after finalization, it is a tuple of {Root_list, Children_map, Parent_map}
%% where Root_list is a list of root Items, Children_map is a map of Item to 
%% a list of its Children Items, and Parent_map is a map of Item to its Parent item.

-include_lib("kernel/include/logger.hrl").

-export([append/3, finalize/1, flatmap/2, traverse/2, traverse/3, single/1,
	 root_list/1, any/3, children/2, parent/2, collapse/1, graft/3,
	 first/1, last/1, next/2, prev/2]).

-type t() :: {list(), map(), map()}.

%% append an Item to the tree at level N
-spec append(term(), integer(), list()) -> list().
append(Item, 0, Tree) -> [Item | Tree];
append(Item, _N, []) -> [Item];
append(Item, N, [{Pid, Children} | Tail]) ->
    [{Pid, append(Item, N-1, Children)} | Tail];
append(Item, _N, [Pid | Tail]) -> [{Pid, [Item]} | Tail].

%% finalize a Tree
-spec finalize(list()) -> t().
finalize(Tree) ->
    { nodes_to_items(Tree),
      nodes_to_children_map(Tree),
      nodes_to_parent_map(undefined, Tree) }.

%% return a degenerated tree with one node
-spec single(term()) -> t().
single(Node) -> { [Node], #{}, #{} }.

%% return a list that apply Fun(Each, Level) to each item
-spec flatmap(function(), t()) -> list().
flatmap(Fun, {Root_list, Children_map, _Parent_map}) ->
    flatmap(Fun, 0, Root_list, Children_map).

%% do action on every node in the tree. The action should return ok
%% when successful. return count of successful actions
-spec traverse(function(), t()) -> integer().
traverse(Action, {Root_list, Children_map, _Parent_map}) ->
    traverse(Action, 0, Root_list, Children_map).

%% do action on every node that are decendent of list. The action should return ok
%% when successful. return count of successful actions
-spec traverse(function(), list(), t()) -> integer().
traverse(Action, List, {_Root_list, Children_map, _Parent_map}) ->
    traverse(Action, 0, List, Children_map).

%% return the root list
-spec root_list(t()) -> list().
root_list({Root_list, _Children_map, _Parent_map}) -> Root_list.

%% get the children of an Item
-spec children(undefined | term(), t()) -> list().
children(undefined, {Root_list, _Children_map, _Parent_map}) -> Root_list;
children(Item, {_Root_list, Children_map, _Parent_map}) ->
    maps:get(Item, Children_map, []).
    
%% get the parent of an Item
-spec parent(term(), t()) -> undefined | term().
parent(Item, {_Root_list, _Children_map, Parent_map}) ->
    maps:get(Item, Parent_map, undefined).

%% to cut a Item from the tree and connect to another Item as a Child.
%% First, The Item and its sub tree will be disconnected.
%% The second Item can be undefined, and the Item will be connected to root
-spec graft(term(), undefined | term(), t()) -> list().
graft(Item, undefined, {Root_list, Children_map, Parent_map}) ->
    case maps:get(Item, Parent_map, undefined) of
	undefined -> {Root_list, Children_map, Parent_map};
	Old_parent ->
	    { Root_list ++ [Item],
	      maps:put( Old_parent,
		        lists:delete(Item, maps:get(Old_parent, Children_map)),
		        Children_map ),
	      maps:remove(Item, Parent_map) }
    end;
graft(Item, Parent, {Root_list, Children_map, Parent_map}) ->
    case maps:get(Item, Parent_map, undefined) of
	Parent -> {Root_list, Children_map, Parent_map};
	undefined ->
	    { lists:delete(Item, Root_list),
	      maps:put(Parent, maps:get(Parent, Children_map, []) ++ [Item],
		       Children_map),
	      maps:put(Item, Parent, Parent_map) };
	Old_parent ->
	    { Root_list,
	      maps:merge(Children_map,
			 #{ Old_parent =>
				lists:delete(Item, maps:get(Old_parent, Children_map)),
			    Parent =>
				maps:get(Parent, Children_map, []) ++ [Item] }),
	      maps:put(Item, Parent, Parent_map) }
    end.

%% given an Item, return if any of itself and it's decendants satisfy a prdicate
%% if Item is undefined, run it on all Root_list
-spec any(fun(), undefined | term(), t()) -> boolean().
any(Pred, undefined, {Root_list, Children_map, _Parent_map}) ->
    any_in_list(Pred, Root_list, Children_map);
any(Pred, Item, {_Root_list, Children_map, _Parent_map}) ->
    case Pred(Item) of
	true -> true;
	false ->
	    any_in_list(Pred, maps:get(Item, Children_map, []), Children_map)
    end.

%% given a Tree, return the first of the root list
-spec first(t()) -> undefined | term().
first({[], _, _}) -> undefined;
first({[Head | _Tail], _, _}) -> Head.

%% given a Tree, return the last of the tree in tree order
-spec last(t()) -> undefined | term ().
last({[], _, _}) -> undefined;
last({Root_list, Children_map, _}) ->
    tail_of(lists:last(Root_list), Children_map).

%% given an Item and a Tree, return the next Item in tree order
%% the tree order is defined as:
%% if the Item has children, it is the first of its children
%% if the Item has sibling, it is the next of its sibling
%% if there is no sibling, go up until there is
-spec next(term(), t()) -> undefined | term(). 
next(Item, {Root_list, Children_map, Parent_map}) ->
    case maps:get(Item, Children_map, []) of
	[Head | _] -> Head;
	_ -> next_sibling(Item, Root_list, Children_map, Parent_map)
    end.

%% given an Item and a Tree, return the previous Item in tree order
%% the tree order is defined as:
%% if the Item has sibling, it is the tail of previous sibling
%% if there is no sibling, return the parent
-spec prev(term(), t()) -> undefined | term().
prev(Item, {Root_list, Children_map, Parent_map}) ->
    case maps:get(Item, Parent_map, undefined) of
	undefined ->
	    prev_sibling(Item, undefined, Root_list, Children_map);
	Parent ->
	    prev_sibling(Item, Parent, maps:get(Parent, Children_map, []),
			 Children_map)
    end.

%% collapse a tree so it is shallower and fatter. For example:
%%  A -> B -> C -> D 
%%  becomes 
%%  A -> B
%%    |> C
%%    \> D
%%  Assume the parent of A has > 1 fanouts
-spec collapse(t()) -> t().
collapse({[Head], Children_map, _Parent_map}) ->
    {New_rl, New_cm} = collapse_node_from(Head, [], Children_map),
    {New_rl, New_cm, parent_map(New_rl, New_cm)};
collapse({Root_list, Children_map, _Parent_map}) ->
    New_cm = lists:foldl(fun collapse_node/2, Children_map, Root_list),
    {Root_list, New_cm, parent_map(Root_list, New_cm)}.

%% private functions
flatmap(_Fun, _Level, [], _Children_map) -> [];
flatmap(Fun, Level, [Item | Tail], Children_map) ->
    case maps:get(Item, Children_map, []) of
	[] -> [Fun(Item, Level) | flatmap(Fun, Level, Tail, Children_map)];
	Children ->
	    [Fun(Item, Level) | flatmap(Fun, Level + 1, Children, Children_map)] ++
	     flatmap(Fun, Level, Tail, Children_map)
    end.

traverse(_Action, Count, [], _Children_map) -> Count;
traverse(Action, Count, [Item | Tail], Children_map) ->
    Count1 = traverse(Action, Count,
		      maps:get(Item, Children_map, []),
		      Children_map),
    Count2 = traverse(Action, Count1, Tail, Children_map),
    case Action(Item) of
	ok -> Count2 + 1;
	_ -> Count2
    end.

nodes_to_items(List) -> nodes_to_items([], List).

nodes_to_items(List, []) -> List;
nodes_to_items(List, [{Item, _Children} | Tail]) -> nodes_to_items([Item | List], Tail); 
nodes_to_items(List, [Item | Tail]) -> nodes_to_items([Item | List], Tail). 

nodes_to_children_map([]) -> #{};
nodes_to_children_map([{Item, Children} | Tail]) ->
    maps:merge( maps:put( Item, nodes_to_items(Children),
			  nodes_to_children_map(Children) ),
		nodes_to_children_map(Tail) );
nodes_to_children_map([_Item | Tail]) ->
    nodes_to_children_map(Tail).

nodes_to_parent_map(_Parent, []) -> #{};
nodes_to_parent_map(undefined, [{Item, Children} | Tail]) ->
    maps:merge( nodes_to_parent_map(Item, Children),
		nodes_to_parent_map(undefined, Tail) );
nodes_to_parent_map(undefined, [_Item | Tail]) ->
    nodes_to_parent_map(undefined, Tail);
nodes_to_parent_map(Parent, [{Item, Children} | Tail]) ->
    maps:merge( maps:put( Item, Parent,
			  nodes_to_parent_map(Item, Children) ),
		nodes_to_parent_map(Parent, Tail) );
nodes_to_parent_map(Parent, [Item | Tail]) ->
    maps:put(Item, Parent, nodes_to_parent_map(Parent, Tail)).

any_in_list(_Pred, [], _Children_map) -> false;
any_in_list(Pred, [Head | Tail], Children_map) ->
    case Pred(Head) of
	true -> true;
	false ->
	    case any_in_list(Pred, maps:get(Head, Children_map, []), Children_map) of
		true -> true;
		false -> any_in_list(Pred, Tail, Children_map)
	    end
    end.

next_sibling(Item, Root_List, Children_map, Parent_map) ->
    case maps:get(Item, Parent_map, undefined) of
	undefined -> next_in_list(Item, Root_List);
	Parent ->
	    case next_in_list(Item, maps:get(Parent, Children_map, [])) of
		undefined -> next_sibling(Parent, Root_List, Children_map, Parent_map);
		Sibling -> Sibling
	    end
    end.

next_in_list(_Item, []) -> undefined;
next_in_list(Item, [Item]) -> undefined; 
next_in_list(Item, [Item, Next | _]) -> Next; 
next_in_list(Item, [_Head | Tail]) -> next_in_list(Item, Tail).

prev_sibling(Item, Parent, List, Children_map) ->
    case prev_in_list(Item, List) of
	undefined -> Parent;
	Sibling -> tail_of(Sibling, Children_map)
    end.
    
prev_in_list(_Item, []) -> undefined;
prev_in_list(Item, [Item | _]) -> undefined; 
prev_in_list(Item, [Head, Item | _]) -> Head; 
prev_in_list(Item, [_Head | Tail]) -> prev_in_list(Item, Tail). 

tail_of(Item, Children_map) ->
    case maps:get(Item, Children_map, []) of
	[] -> Item;
	List -> tail_of(lists:last(List), Children_map)
    end.

parent_map([], _Children_map) -> #{};
parent_map([Head | Tail], Children_map) ->
    Children = maps:get(Head, Children_map, []),
    Map1 = maps:from_list(lists:map(
			    fun (Each) -> {Each, Head} end,
			    Children)),
    Map2 = parent_map(Children, Children_map),
    Map3 = parent_map(Tail, Children_map),
    maps:merge(Map1, maps:merge(Map2, Map3)).

collapse_node_from(Node, List, Children_map) ->
    case maps:get(Node, Children_map, []) of
	[Head] ->
	    collapse_node_from(Head, [Node | List], maps:remove(Node, Children_map));
	Clist ->
	    { lists:reverse([Node | List]),
	      lists:foldl(fun collapse_node/2, Children_map, Clist) }
    end.

collapse_node(Node, Children_map) ->
    case maps:get(Node, Children_map, []) of
	[Head] ->
	    {List, New_map} = collapse_node_from(Head, [], Children_map),
	    maps:put(Node, List, New_map);
	List ->
	    lists:foldl(fun collapse_node/2, Children_map, List)
    end.
