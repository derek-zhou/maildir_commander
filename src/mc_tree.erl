-module(mc_tree).

%% this module deal with simple tree. At building phase it is a list of root nodes
%% and each node is a either an Item (leave) or {Item, [Children]} non-leave
%% after finalization, it is a tuple of {Root_list, Children_map, Parent_map}
%% where Root_list is a list of root Items, Children_map is a map of Item to 
%% a list of its Children Items, and Parent_map is a map of Item to its Parent item.

-export([append/3, finalize/1, flatmap/2, children/2, parent/2, graft/3]).

-type mc_tree() :: {list(), map(), map()}.

%% append an Item to the tree at level N
-spec append(term(), integer(), list()) -> list().
append(Item, 0, Tree) -> [Item | Tree];
append(Item, _N, []) -> [Item];
append(Item, N, [{Pid, Children} | Tail]) ->
    [{Pid, append(Item, N-1, Children)} | Tail];
append(Item, _N, [Pid | Tail]) -> [{Pid, [Item]} | Tail].

%% finalize a Tree
-spec finalize(list()) -> mc_tree().
finalize(Tree) ->
    { nodes_to_items(Tree),
      nodes_to_children_map(Tree),
      nodes_to_parent_map(undefined, Tree) }.

%% return a list that apply Fun(Each, Level) to each item
-spec flatmap(function(), mc_tree()) -> list().
flatmap(Fun, {Root_list, Children_map, _Parent_map}) ->
    flatmap(Fun, 0, Root_list, Children_map).

%% get the children of an Item
-spec children(term(), mc_tree()) -> list().
children(Item, {_Root_list, Children_map, _Parent_map}) ->
    maps:get(Item, Children_map, []).
    
%% get the parent of an Item
-spec parent(term(), mc_tree()) -> undefined | term().
parent(Item, {_Root_list, _Children_map, Parent_map}) ->
    maps:get(Item, Parent_map, undefined).

%% to cut a Item from the tree and connect to another Item as a Child.
%% First, The Item and its sub tree will be disconnected.
%% The second Item can be undefined, and the Item will be connected to root
-spec graft(term(), undefined | term(), mc_tree()) -> list().
graft(Item, undefined, {Root_list, Children_map, Parent_map}) ->
    case maps:get(Item, Parent_map, undefined) of
	undefined -> {Root_list, Children_map, Parent_map};
	Old_parent ->
	    { [Item | Root_list],
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
	      maps:put(Parent, [Item | maps:get(Parent, Children_map, [])],
		       Children_map),
	      maps:put(Item, Parent, Parent_map) };
	Old_parent ->
	    { Root_list,
	      maps:merge(Children_map,
			 #{ Old_parent =>
				lists:delete(Item, maps:get(Old_parent, Children_map)),
			    Parent =>
				[Item | maps:get(Parent, Children_map, [])] }),
	      maps:put(Item, Parent, Parent_map) }
    end.

%% private functions
flatmap(_Fun, _Level, [], _Children_map) -> [];
flatmap(Fun, Level, [Item | Tail], Children_map) ->
    case maps:get(Item, Children_map, []) of
	[] -> [Fun(Item, Level) | flatmap(Fun, Level, Tail, Children_map)];
	Children ->
	    [[Fun(Item, Level) | flatmap(Fun, Level + 1, Children, Children_map)] |
	     flatmap(Fun, Level, Tail, Children_map)]
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
