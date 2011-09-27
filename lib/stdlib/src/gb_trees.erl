%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%% =====================================================================
%% @doc General Balanced Trees - highly efficient functional dictionaries.
%%
%% NOTE: The {@link dict} module now supports using General Balanced Trees
%% as the underlying representation, and a tree can be created by calling
%% {@link dict:new/1. dict:new([ordered])}. <b>It is no longer recommended
%% to use the `gb_trees' module directly unless you need to use some of its
%% more unusual features.</b>
%%
%% This module is an efficient purely functional implementation of Prof.
%% Arne Andersson's General Balanced Trees. These have no storage overhead
%% compared to plain unbalanced binary trees, and their performance is in
%% general better than AVL trees.
%%
%% This module considers two keys as different if and only if they do not
%% compare arithmetically equal (`=='). I.e., `1' and `1.0' are considered
%% to be the same, and so are `{1.0, 2}' and `{1, 2.0}'.
%%
%% @copyright 1999-2001 Sven-Olof Nyström, Richard Carlsson

-module(gb_trees).

-export([empty/0, is_empty/1, size/1, lookup/2, get/2, get/3, insert/3,
	 update/3, enter/3, delete/2, delete_any/2, balance/1, values/2,
	 is_defined/2, keys/1, values/1, to_list/1, from_orddict/1,
	 smallest/1, largest/1, take_smallest/1, take_largest/1,
         first_key/1, last_key/1, take_first/1, take_last/1,
	 iterator/1, next/1, map/2, filter/2, foldl/3, foldr/3,
         next_key/2, prev_key/2, increment/3, find/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data structure:
%% - {Size, Tree}, where `Tree' is composed of nodes of the form:
%%   - {Key, Value, Smaller, Bigger}, and the "empty tree" node:
%%   - nil.
%%
%% I make no attempt to balance trees after deletions. Since deletions
%% don't increase the height of a tree, I figure this is OK.
%%
%% Original balance condition h(T) <= ceil(c * log(|T|)) has been
%% changed to the similar (but not quite equivalent) condition 2 ^ h(T)
%% <= |T| ^ c. I figure this should also be OK.
%%
%% Performance is comparable to the AVL trees in the Erlang book (and
%% faster in general due to less overhead); the difference is that
%% deletion works for these trees, but not for the book's trees.
%% Behaviour is logarithmic (as it should be).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some macros.

-define(p, 2). % It seems that p = 2 is optimal for sorted keys

-define(pow(A, _), A * A). % correct with exponent as defined above.

-define(div2(X), X bsr 1). 

-define(mul2(X), X bsl 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some types.

%% not a public type, only internal
-type gb_tree_node() :: 'nil' | {_, _, _, _}.

-opaque iter() :: [gb_tree_node()].
%% An iterator for a General Balanced Tree.

%% A declaration equivalent to the following is currently hard-coded
%% in erl_types.erl
%%
%% @type gb_tree(). A General Balanced Tree.
%% -opaque gb_tree() :: {non_neg_integer(), gb_tree_node()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec empty() -> gb_tree().

%% @doc Return a new empty tree.

empty() ->
    {0, nil}.

-spec is_empty(Tree) -> boolean() when
      Tree :: gb_tree().

%% @doc Test for empty tree. Returns `true' if `Tree' is an empty tree,
%% and `false' otherwise.

is_empty({0, nil}) ->
    true;
is_empty(_) ->
    false.

-spec size(Tree) -> non_neg_integer() when
      Tree :: gb_tree().

%% @doc Return the number of nodes in a tree. Returns zero if the tree
%% is empty.

size({Size, _}) when is_integer(Size), Size >= 0 ->
    Size.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec lookup(Key, Tree) -> 'none' | {'value', Val} when
      Key :: term(),
      Val :: term(),
      Tree :: gb_tree().

%% @doc Look up a key in a tree. Returns either `{value, Val}', where `Val'
%% is the value stored for `Key' in `Tree', or `none' if the key is not
%% present in the tree.
%% @deprecated Use {@link find/2} instead.

lookup(Key, {_, T}) ->
    lookup_1(Key, T).

%% The term order is an arithmetic total order, so we should not
%% test exact equality for the keys. (If we do, then it becomes
%% possible that neither `>', `<', nor `=:=' matches.) Testing '<'
%% and '>' first is statistically better than testing for
%% equality, and also allows us to skip the test completely in the
%% remaining case.

lookup_1(Key, {Key1, _, Smaller, _}) when Key < Key1 ->
    lookup_1(Key, Smaller);
lookup_1(Key, {Key1, _, _, Bigger}) when Key > Key1 ->
    lookup_1(Key, Bigger);
lookup_1(_, {_, Value, _, _}) ->
    {value, Value};
lookup_1(_, nil) ->
    none.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a variant of `lookup' for compatibility with dict.

-spec find(Key, Tree) -> 'error' | {'ok', Val} when
      Key :: term(),
      Val :: term(),
      Tree :: gb_tree().

%% @doc Look up a key in a tree. Returns either `{value, Val}', where `Val'
%% is the value stored for `Key' in `Tree', or `none' if the key is not
%% present in the tree.
%% @see values/2
%% @see lookup/2

find(Key, {_, T}) ->
    find_1(Key, T).

find_1(Key, {Key1, _, Smaller, _}) when Key < Key1 ->
    find_1(Key, Smaller);
find_1(Key, {Key1, _, _, Bigger}) when Key > Key1 ->
    find_1(Key, Bigger);
find_1(_, {_, Value, _, _}) ->
    {ok, Value};
find_1(_, nil) ->
    error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a specialized version of `lookup'.

-spec is_defined(Key, Tree) -> boolean() when
      Key :: term(),
      Tree :: gb_tree().

%% @doc Test for membership in a tree. Returns `true' if `Key' is
%% present in `Tree', and `false' otherwise.

is_defined(Key, {_, T}) ->
    is_defined_1(Key, T).

is_defined_1(Key, {Key1, _, Smaller, _}) when Key < Key1 ->
    is_defined_1(Key, Smaller);
is_defined_1(Key, {Key1, _, _, Bigger}) when Key > Key1 ->
    is_defined_1(Key, Bigger);
is_defined_1(_, {_, _, _, _}) ->
    true;
is_defined_1(_, nil) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a specialized version of `lookup'.

-spec get(Key, Tree) -> Val when
      Key :: term(),
      Tree :: gb_tree(),
      Val :: term().

%% @doc Retrieve a stored value. Returns the value stored for `Key' in
%% `Tree'. Throws an exception if the key is not present in the tree.

get(Key, {_, T}) ->
    get_1(Key, T).

get_1(Key, {Key1, _, Smaller, _}) when Key < Key1 ->
    get_1(Key, Smaller);
get_1(Key, {Key1, _, _, Bigger}) when Key > Key1 ->
    get_1(Key, Bigger);
get_1(_, {_, Value, _, _}) ->
    Value.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Like get/2, but returns default value if key not found in tree

-spec get(Key, Default, Tree) -> Val when
      Key :: term(),
      Default :: term(),
      Tree :: gb_tree(),
      Val :: term().

%% @doc Retrieve a stored value or use a default. Returns the value
%% stored for `Key' in `Tree', or returns `Default' if the key is not
%% present in the tree.

get(Key, Default, {_, T}) ->
    get_1(Key, Default, T).

get_1(Key, Default, {Key1, _, Smaller, _}) when Key < Key1 ->
    get_1(Key, Default, Smaller);
get_1(Key, Default, {Key1, _, _, Bigger}) when Key > Key1 ->
    get_1(Key, Default, Bigger);
get_1(_, _Default, {_, Value, _, _}) ->
    Value;
get_1(_, Default, _) ->
    Default.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec update(Key, Val, Tree1) -> Tree2 when
      Key :: term(),
      Val :: term(),
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree().

%% @doc Update the value of an entry. Replaces the value stored for
%% `Key' in `Tree' with `Val' and returns the new tree. Throws an
%% exception if the key is not present in the tree.

update(Key, Val, {S, T}) ->
    T1 = update_1(Key, Val, T),
    {S, T1}.

%% See `lookup' for notes on the term comparison order.

update_1(Key, Value, {Key1, V, Smaller, Bigger}) when Key < Key1 -> 
    {Key1, V, update_1(Key, Value, Smaller), Bigger};
update_1(Key, Value, {Key1, V, Smaller, Bigger}) when Key > Key1 ->
    {Key1, V, Smaller, update_1(Key, Value, Bigger)};
update_1(Key, Value, {_, _, Smaller, Bigger}) ->
    {Key, Value, Smaller, Bigger}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec insert(Key, Val, Tree1) -> Tree2 when
      Key :: term(),
      Val :: term(),
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree().

%% @doc Insert a new entry in a tree. Stores `Val' as the value for
%% `Key' in `Tree' and returns the new tree. Throws an exception if the
%% key is already present in the tree.

insert(Key, Val, {S, T}) when is_integer(S) ->
    S1 = S+1,
    {S1, insert_1(Key, Val, T, ?pow(S1, ?p))}.

insert_1(Key, Value, {Key1, V, Smaller, Bigger}, S) when Key < Key1 -> 
    case insert_1(Key, Value, Smaller, ?div2(S)) of
	{T1, H1, S1} ->
	    T = {Key1, V, T1, Bigger},
	    {H2, S2} = count(Bigger),
	    H = ?mul2(erlang:max(H1, H2)),
	    SS = S1 + S2 + 1,
	    P = ?pow(SS, ?p),
	    if
		H > P -> 
		    balance(T, SS);
		true ->
		    {T, H, SS}
	    end;
	T1 ->
	    {Key1, V, T1, Bigger}
    end;
insert_1(Key, Value, {Key1, V, Smaller, Bigger}, S) when Key > Key1 -> 
    case insert_1(Key, Value, Bigger, ?div2(S)) of
	{T1, H1, S1} ->
	    T = {Key1, V, Smaller, T1},
	    {H2, S2} = count(Smaller),
	    H = ?mul2(erlang:max(H1, H2)),
	    SS = S1 + S2 + 1,
	    P = ?pow(SS, ?p),
	    if
		H > P -> 
		    balance(T, SS);
		true ->
		    {T, H, SS}
	    end;
	T1 ->
	    {Key1, V, Smaller, T1}
    end;
insert_1(Key, Value, nil, S) when S =:= 0 ->
    {{Key, Value, nil, nil}, 1, 1};
insert_1(Key, Value, nil, _S) ->
    {Key, Value, nil, nil};
insert_1(Key, _, _, _) ->
    erlang:error({key_exists, Key}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec enter(Key, Val, Tree1) -> Tree2 when
      Key :: term(),
      Val :: term(),
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree().

%% @doc Insert or update an entry in a tree. Stores `Val' as the value
%% for `Key' in `Tree' and returns the new tree. If the key already
%% existed in the tree, the entry is updated, otherwise a new entry is
%% inserted.

enter(Key, Val, T) ->
    case is_defined(Key, T) of
	true ->
	    update(Key, Val, T);
	false ->
	    insert(Key, Val, T)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec map(Function, Key, Tree1) -> Tree2 when
      Function :: fun((V1 :: term()) -> V2 :: term()),
      Key :: term(),
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree().

%% @doc Apply a function to a single entry. Applies `Function' to the
%% current value for `Key', replacing the value with the result of the
%% function and returning the new tree. Throws an exception if the key
%% is not present in the tree.

map(F, Key, {S, T}) when is_function(F, 1) ->
    {S, map_1(F, Key, T)}.

map_1(F, Key, {Key1, V, Smaller, Bigger}) when Key < Key1 ->
    {Key1, V, map_1(F, Key, Smaller), Bigger};
map_1(F, Key, {Key1, V, Smaller, Bigger}) when Key > Key1 ->
    {Key1, V, Smaller, map_1(F, Key, Bigger)};
map_1(F, Key, {_, V, Smaller, Bigger}) ->
    {Key, F(V), Smaller, Bigger}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec map(Function, Key, Initial, Tree1) -> Tree2 when
      Function :: fun((V1 :: term()) -> V2 :: term()),
      Key :: term(),
      Initial :: number(),
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree().

%% @doc Apply a function to a single entry or use a default value.
%% Applies `Function' to the current value for `Key', if that key exists
%% in the tree, replacing the value with the result of the function and
%% returning the new tree. If the key is not present in the tree, a new
%% entry is created for `Key' with `Initial' as value.

map(F, Key, Initial, T) when is_function(F, 1) ->
    case is_defined(Key, T) of
	true ->
	    map(F, Key, T);
	false ->
	    insert(Key, Initial, T)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec increment(Key, Delta, Tree1) -> Tree2 when
      Key :: term(),
      Delta :: number(),
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree().

%% @doc Add a number to the value of an entry. Adds `Delta' to the
%% current value for `Key', if that key exists in the tree, and returns
%% the new tree. If the key is not present in the tree, a new entry is
%% created for `Key' with `Delta' as value, as if the key had existed
%% with the value 0.

increment(Key, Delta, Tree) when is_number(Delta) ->
    map(fun (V) -> V + Delta end, Key, Delta, Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

count({_, _, nil, nil}) ->
    {1, 1};
count({_, _, Sm, Bi}) ->
    {H1, S1} = count(Sm),
    {H2, S2} = count(Bi),
    {?mul2(erlang:max(H1, H2)), S1 + S2 + 1};
count(nil) ->
    {1, 0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec balance(Tree1) -> Tree2 when
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree().

%% @doc Rebalance a tree. Rebuilds the tree uniformly so that all leaf
%% nodes are at approximately the same depth, returning the new tree.
%%
%% Note that this is rarely needed, because trees usually balance
%% themselves automatically. Rebalancing may be useful to do if a large
%% number of entries have been deleted from the tree, without any
%% subsequent insertions. A forced rebalancing could then be done in
%% order to minimise lookup times, since deletions in themselves never
%% rebalance the tree.

balance({S, T}) ->
    {S, balance(T, S)}.

balance(T, S) ->
    balance_list(to_list_1(T), S).

balance_list(L, S) ->
    {T, []} = balance_list_1(L, S),
    T.

balance_list_1(L, S) when S > 1 ->
    Sm = S - 1,
    S2 = Sm div 2,
    S1 = Sm - S2,
    {T1, [{K, V} | L1]} = balance_list_1(L, S1),
    {T2, L2} = balance_list_1(L1, S2),
    T = {K, V, T1, T2},
    {T, L2};
balance_list_1([{Key, Val} | L], 1) ->
    {{Key, Val, nil, nil}, L};
balance_list_1(L, 0) ->
    {nil, L}.

-spec from_orddict(List) -> Tree when
      List :: [{Key :: term(), Val :: term()}],
      Tree :: gb_tree().

%% @doc Make a tree from an orddict. Turns an ordered list of key/value
%% pairs into a tree. The list must not contain duplicate keys, and must
%% be sorted on the first element of the pairs.

from_orddict(L) ->
    S = length(L),
    {S, balance_list(L, S)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_any(Key, Tree1) -> Tree2 when
      Key :: term(),
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree().

%% @doc Remove an entry from a tree, if it exists. Removes the entry for
%% `Key' from `Tree' if the key is present in the tree, returning the new
%% tree, or otherwise returns `Tree' unchanged.

delete_any(Key, T) ->
    case is_defined(Key, T) of
	true ->
	    delete(Key, T);
	false ->
	    T
    end.

-spec delete(Key, Tree1) -> Tree2 when
      Key :: term(),
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree().

%% @doc Remove an existing entry from a tree. Deletes the entry for `Key'
%% from `Tree' and returns the new tree. Throws an exception if the key is
%% not present in the tree.

delete(Key, {S, T}) when is_integer(S), S >= 0 ->
    {S - 1, delete_1(Key, T)}.

%% See `lookup' for notes on the term comparison order.

delete_1(Key, {Key1, Value, Smaller, Larger}) when Key < Key1 ->
    Smaller1 = delete_1(Key, Smaller),
    {Key1, Value, Smaller1, Larger};
delete_1(Key, {Key1, Value, Smaller, Bigger}) when Key > Key1 ->
    Bigger1 = delete_1(Key, Bigger),
    {Key1, Value, Smaller, Bigger1};
delete_1(_, {_, _, Smaller, Larger}) ->
    merge(Smaller, Larger).

merge(Smaller, nil) ->
    Smaller;
merge(nil, Larger) ->
    Larger;
merge(Smaller, Larger) ->
    {Key, Value, Larger1} = take_smallest1(Larger),
    {Key, Value, Smaller, Larger1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take(Key, Dict0) -> {Value, Dict1} when
      Key :: term(),
      Dict0 :: dict(),
      Dict1 :: dict(),
      Value :: term().

%% @doc Extract an entry from a tree. Returns a tuple with the value for
%% `Key' in `Tree' and a new tree with the entry for the key deleted. Throws
%% an exception if the key is not present in the tree.

take(Key, Dict) ->
    {get(Key, Dict), delete_any(Key, Dict)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec take_smallest(Tree1) -> {Key, Val, Tree2} when
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree(),
      Key :: term(),
      Val :: term().

%% @doc Extract the first entry in the tree. Returns the key and value for
%% the smallest key in `Tree' and a new tree with the entry for the key
%% deleted. Throws an exception if the tree is empty.
%% @deprecated Use {@link take_first/1} instead.

take_smallest({Size, Tree}) when is_integer(Size), Size >= 0 ->
    {Key, Value, Larger} = take_smallest1(Tree),
    {Key, Value, {Size - 1, Larger}}.

take_smallest1({Key, Value, nil, Larger}) ->
    {Key, Value, Larger};
take_smallest1({Key, Value, Smaller, Larger}) ->
    {Key1, Value1, Smaller1} = take_smallest1(Smaller),
    {Key1, Value1, {Key, Value, Smaller1, Larger}}.

-spec take_first(Tree1) -> {{Key, Val}, Tree2} | error when
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree(),
      Key :: term(),
      Val :: term().

%% @doc Extract the first entry in the tree. Returns the key/value pair for
%% the smallest key in `Tree' and a new tree with the entry for the key
%% deleted, or returns `error' if the tree is empty.

take_first(Dict) ->
    case first_key(Dict) of
        {ok, Key} ->
            {Val, Dict1} = take(Key, Dict),
            {{Key, Val}, Dict1};
        error ->
            error
    end.

-spec smallest(Tree) -> {Key, Val} when
      Tree :: gb_tree(),
      Key :: term(),
      Val :: term().

%% @doc Get the first entry in the tree. Returns the key/value pair for the
%% smallest key in `Tree'. Throws an exception if the tree is empty.

smallest({_, Tree}) ->
    smallest_1(Tree).

smallest_1({Key, Value, nil, _Larger}) ->
    {Key, Value};
smallest_1({_Key, _Value, Smaller, _Larger}) ->
    smallest_1(Smaller).

-spec first_key(Tree) -> {ok, Key} | error when
      Tree :: gb_tree(),
      Key :: term().

%% @doc Get the first key in the tree. Returns `{ok, Key}' where `Key' is
%% the smallest key in `Tree', or returns 'error' if the tree is empty.

first_key({_, nil}) ->
    error;
first_key({_, Tree}) ->
    first_key_1(Tree).

first_key_1({Key, _Value, nil, _Larger}) ->
    {ok, Key};
first_key_1({_Key, _Value, Smaller, _Larger}) ->
    first_key_1(Smaller).

-spec prev_key(Key, Tree) -> {ok, Key1} | error when
      Tree :: gb_tree(),
      Key :: term(),
      Key1 :: term().

%% @doc Get the next smaller key in the tree. Returns `{ok, Smaller}' where
%% `Smaller' is the largest key in `Tree' smaller than the given `Key', or
%% returns 'error' if `Key' is the smallest key in the tree. Throws an
%% exception if `Key' does not exist in the tree.

prev_key(Key, {_, Tree}) ->
    prev_key_1(Key, Tree).

prev_key_1(Key, {Key1, _Value, Smaller, _Larger}) when Key < Key1 ->
    prev_key_1(Key, Smaller);
prev_key_1(Key, {Key1, _Value, _Smaller, Larger}) when Key > Key1 ->
    prev_key_1(Key, Larger);
prev_key_1(_, {_, _, nil, _Larger}) ->
    error;
prev_key_1(_, {_, _, {Key1,_,_,_}, _Larger}) ->
    {ok, Key1}.

-spec take_largest(Tree1) -> {Key, Val, Tree2} when
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree(),
      Key :: term(),
      Val :: term().

%% @doc Extract the last entry in the tree. Returns the key and value for
%% the largest key in `Tree' and a new tree with the entry for the key
%% deleted. Throws an exception if the tree is empty.
%% @deprecated Use {@link take_last/1} instead.

take_largest({Size, Tree}) when is_integer(Size), Size >= 0 ->
    {Key, Value, Smaller} = take_largest1(Tree),
    {Key, Value, {Size - 1, Smaller}}.

take_largest1({Key, Value, Smaller, nil}) ->
    {Key, Value, Smaller};
take_largest1({Key, Value, Smaller, Larger}) ->
    {Key1, Value1, Larger1} = take_largest1(Larger),
    {Key1, Value1, {Key, Value, Smaller, Larger1}}.

-spec take_last(Tree1) -> {{Key, Val}, Tree2} | error when
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree(),
      Key :: term(),
      Val :: term().

%% @doc Extract the last entry in the tree. Returns the key/value pair for
%% the largest key in `Tree' and a new tree with the entry for the key
%% deleted, or returns `error' if the tree is empty.

take_last(Dict) ->
    case last_key(Dict) of
        {ok, Key} ->
            {Val, Dict1} = take(Key, Dict),
            {{Key, Val}, Dict1};
        error ->
            error
    end.

-spec largest(Tree) -> {Key, Val} when
      Tree :: gb_tree(),
      Key :: term(),
      Val :: term().

%% @doc Get the last entry in the tree. Returns the key/value pair for the
%% largest key in `Tree'. Throws an exception if the tree is empty.

largest({_, Tree}) ->
    largest_1(Tree).

largest_1({Key, Value, _Smaller, nil}) ->
    {Key, Value};
largest_1({_Key, _Value, _Smaller, Larger}) ->
    largest_1(Larger).

-spec last_key(Tree) -> {ok, Key} | error when
      Tree :: gb_tree(),
      Key :: term().

%% @doc Get the last key in the tree. Returns `{ok, Key}' where `Key' is
%% the largest key in `Tree', or returns 'error' if the tree is empty.

last_key({_, nil}) ->
    error;
last_key({_, Tree}) ->
    last_key_1(Tree).

last_key_1({Key, _Value, _Smaller, nil}) ->
    {ok, Key};
last_key_1({_Key, _Value, _Smaller, Larger}) ->
    last_key_1(Larger).

-spec next_key(Key, Tree) -> {ok, Key1} | error when
      Tree :: gb_tree(),
      Key :: term(),
      Key1 :: term().

%% @doc Get the next larger key in the tree. Returns `{ok, Larger}' where
%% `Larger' is the smallest key in `Tree' larger than the given `Key', or
%% returns 'error' if `Key' is the largest key in the tree. Throws an
%% exception if `Key' does not exist in the tree.

next_key(Key, {_, Tree}) ->
    next_key_1(Key, Tree).

next_key_1(Key, {Key1, _Value, Smaller, _Larger}) when Key < Key1 ->
    next_key_1(Key, Smaller);
next_key_1(Key, {Key1, _Value, _Smaller, Larger}) when Key > Key1 ->
    next_key_1(Key, Larger);
next_key_1(_, {_, _, _Smaller, nil}) ->
    error;
next_key_1(_, {_, _, _Larger, {Key1,_,_,_}}) ->
    {ok, Key1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec to_list(Tree) -> [{Key, Val}] when
      Tree :: gb_tree(),
      Key :: term(),
      Val :: term().

%% @doc: Convert a tree into an ordered list. Returns a list of pairs `{Key,
%% Val}' for all keys in `Tree', ordered by key.

to_list({_, T}) ->
    to_list(T, []).

to_list_1(T) -> to_list(T, []).

to_list({Key, Value, Small, Big}, L) ->
    to_list(Small, [{Key, Value} | to_list(Big, L)]);
to_list(nil, L) -> L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec keys(Tree) -> [Key] when
      Tree :: gb_tree(),
      Key :: term().

% @doc Get all keys in a tree. Returns the keys in `Tree' as an ordered
% list.

keys({_, T}) ->
    keys(T, []).

keys({Key, _Value, Small, Big}, L) ->
    keys(Small, [Key | keys(Big, L)]);
keys(nil, L) -> L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec values(Tree) -> [Val] when
      Tree :: gb_tree(),
      Val :: term().

%% @doc Get all values in a tree. Returns the values in `Tree' as an ordered
%% list, sorted by their corresponding keys. Duplicates are not removed.

values({_, T}) ->
    all_values(T, []).

all_values({_Key, Value, Small, Big}, L) ->
    all_values(Small, [Value | values(Big, L)]);
all_values(nil, L) -> L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a variant of `lookup' for compatibility with dict.

-spec values(Key, Tree) -> [] | [Val] when
      Key :: term(),
      Val :: term(),
      Tree :: gb_tree().

%% @doc List the values (if any) stored for a key. Returns either `[Val]',
%% where `Val' is the value stored for `Key' in `Tree', or `[]' if the key
%% is not present in the tree.
%% @see find/2

values(Key, {_, T}) ->
    key_values(Key, T).

key_values(Key, {Key1, _, Smaller, _}) when Key < Key1 ->
    key_values(Key, Smaller);
key_values(Key, {Key1, _, _, Bigger}) when Key > Key1 ->
    key_values(Key, Bigger);
key_values(_, {_, Value, _, _}) ->
    [Value];
key_values(_, nil) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec iterator(Tree) -> Iter when
      Tree :: gb_tree(),
      Iter :: iter().

%% @doc Get an iterator for a tree. Returns an iterator object that can be
%% used for traversing the entries of `Tree'; see {@link next/1}.
%%
%% The iterator implementation is very efficient; traversing the whole tree
%% using {@link next/1} is only slightly slower than using {@link to_list/1}
%% to get the list of all elements and traverse it. The main advantage of
%% the iterator approach is that it does not require the complete list of
%% all elements to be built in memory at one time, if the tree is large.

iterator({_, T}) ->
    iterator_1(T).

iterator_1(T) ->
    iterator(T, []).

%% The iterator structure is really just a list corresponding to
%% the call stack of an in-order traversal. This is quite fast.

iterator({_, _, nil, _} = T, As) ->
    [T | As];
iterator({_, _, L, _} = T, As) ->
    iterator(L, [T | As]);
iterator(nil, As) ->
    As.

-spec next(Iter1) -> 'none' | {Key, Val, Iter2} when
      Iter1 :: iter(),
      Iter2 :: iter(),
      Key :: term(),
      Val :: term().

%% @doc Traverse a tree with an iterator. Returns `{Key, Val, Iter2}' where
%% `Key' is the smallest remaining key in `Iter1', and `Iter2' is either a
%% new iterator to be used for traversing the remaining entries, or the atom
%% `none' if no entries remain.

next([{X, V, _, T} | As]) ->
    {X, V, iterator(T, As)};
next([]) ->
    none.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec map(Function, Tree1) -> Tree2 when
      Function :: fun((K :: term(), V1 :: term()) -> V2 :: term()),
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree().

%% @doc Apply a function to each entry of the tree. Applies `Function' to
%% every key/value pair of `Tree', replacing the value of each entry with
%% the result from the function, and returning the new tree.

map(F, {Size, Tree}) when is_function(F, 2) ->
    {Size, map_1(F, Tree)}.

map_1(_, nil) -> nil;
map_1(F, {K, V, Smaller, Larger}) ->
    {K, F(K, V), map_1(F, Smaller), map_1(F, Larger)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec filter(Function, Tree1) -> Tree2 when
      Function :: fun((K :: term(), V1 :: term()) -> B :: boolean()),
      Tree1 :: gb_tree(),
      Tree2 :: gb_tree().

%% @doc Filter the entries of a tree. Returns a new tree containing only
%% those entries in `Tree' for which `Function' returns `true'.

filter(F, {_Size, Tree}) when is_function(F, 2) ->
    filter_1(F, Tree).

filter_1(_, nil) -> {0, nil};
filter_1(F, {K, V, Smaller, Larger}) ->
    {N1, Smaller1} = filter_1(F, Smaller),
    {N2, Larger1} = filter_1(F, Larger),
    case F(K, V) of
        true -> {N1 + N2 + 1, {K, V, Smaller1, Larger1}};
        false -> {N1 + N2, merge(Smaller1, Larger1)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec foldl(Function, A0, Tree) -> A when
      Function :: fun((K::term(), V::term(), A1::term()) -> A2::term()),
      A :: term(),
      A0 :: term(),
      Tree :: gb_tree().

%% @doc Fold a function over a tree in key order. Calls `Function' on the
%% entries of the tree in ascending (left-to-right) key order, propagating
%% the output accumulator `A2' from each call into the next call as the
%% input accumulator `A1'. The initial accumulator value `A0' is used as
%% input to the first call.

foldl(F, A, {_Size, Tree}) when is_function(F, 3) ->
    foldl_1(F, A, Tree).

foldl_1(_, A, nil) -> A;
foldl_1(F, A, {K, V, Smaller, Larger}) ->
    foldl_1(F, F(K, V, foldl_1(F, A, Smaller)), Larger).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec foldr(Function, A0, Tree) -> A when
      Function :: fun((K::term(), V::term(), A1::term()) -> A2::term()),
      A :: term(),
      A0 :: term(),
      Tree :: gb_tree().

%% @doc Fold a function over a tree in reverse key order. Calls `Function'
%% on the entries of the tree in descending (right-to-left) key order,
%% propagating the output accumulator `A2' from each call into the next call
%% as the input accumulator `A1'. The initial accumulator value `A0' is used
%% as input to the first call.

foldr(F, A, {_Size, Tree}) when is_function(F, 3) ->
    foldr_1(F, A, Tree).

foldr_1(_, A, nil) -> A;
foldr_1(F, A, {K, V, Smaller, Larger}) ->
    foldr_1(F, F(K, V, foldr_1(F, A, Larger)), Smaller).

