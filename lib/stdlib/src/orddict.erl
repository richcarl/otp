%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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

%% @doc
%%
%% @see dict

-module(orddict).

-behaviour(dict).

%% Standard interface.
-export([new/0,new/1,is_key/2,size/1,is_empty/1,info/1,info/2]).
-export([to_list/1,to_orddict/1]).
-export([from_list/1,from_list/2,from_orddict/1,from_orddict/2]).
-export([get/2,get/3,find/2,keys/1,values/1,values/2,erase/2]).
-export([store/3,replace/3,increment/3]).
-export([foreach/2,map/2,map/3,map/4,filter/2,merge/3]).
-export([foldl/3,foldr/3,foldl1/2,foldr1/2,foldln/3,foldrn/3]).
-export([first_key/1,last_key/1,next_key/2,prev_key/2]).
-export([take_first/1,take_last/1,take/2]).

%% Nonstandard interface (not recommended)
-export([append/3,append_list/3]).

%% Deprecated interface
-export([fetch/2,fetch_keys/1,update/3,update/4,update_counter/3,fold/3]).


-compile({no_auto_import,[size/1]}).

-export_type([orddict/0]).

%%---------------------------------------------------------------------------

-type orddict() :: [{Key :: term(), Value :: term()}].

%%---------------------------------------------------------------------------

-spec new() -> orddict().

%% @doc Create a dictionary.

new() -> [].

-spec new(Opts :: [Option]) -> Orddict when
      Option :: any(),
      Orddict :: orddict().

%% @doc Create a dictionary. Currently, no options are available.

%% Note that options toward the end of the options list take precedence over
%% earlier ones.

new(_Opts) ->
    new().


-spec is_key(Key, Orddict) -> boolean() when
      Key :: term(),
      Orddict :: orddict().

%% @doc Test if a key is in a dictionary. This function tests if `Key' is
%% contained in the dictionary `Orddict'.

is_key(Key, [{K,_}|_]) when Key < K -> false;
is_key(Key, [{K,_}|Dict]) when Key > K -> is_key(Key, Dict);
is_key(_Key, [{_K,_Val}|_]) -> true;		%Key == K
is_key(_, []) -> false.

-spec to_list(Orddict) -> List when
      Orddict :: orddict(),
      List :: [{Key :: term(), Value :: term()}].

%% @doc Convert a dictionary to a list of pairs. Because orddicts are
%% already ordered lists of pairs, this implementation is simply the
%% identity function, returning `Orddict' unchanged.

to_list(Dict) -> Dict.

-spec to_orddict(Orddict) -> Orddict when
      Orddict :: orddict().

%% @doc Convert a dictionary to an orddict. This implementation is simply
%% the identity function, returning `Orddict' unchanged.

to_orddict(Dict) -> Dict.

-spec from_list(List) -> Orddict when
      List :: [{Key :: term(), Value :: term()}],
      Orddict :: orddict().

%% @doc Convert a list of pairs to a dictionary. This function converts the
%% `Key'-`Value' list `List' to an orddict. In case of duplicate keys in
%% the list, later entries take precedence.

from_list(Pairs) ->
    lists:foldl(fun ({K,V}, D) -> store(K, V, D) end, [], Pairs).

-spec from_list(List, Opts :: [Option]) -> Orddict when
      List :: [{Key :: term(), Value :: term()}],
      Option :: any(),
      Orddict :: orddict().

%% @doc Convert a list of pairs to a dictionary. Like {@link from_list/1},
%% but takes an option list just like {@link new/1}.

from_list(L, _Opts) ->
    from_list(L).

-spec from_orddict(Orddict) -> Orddict when
      Orddict :: orddict().

%% @doc Convert an ordered list of pairs to a dictionary. This
%% implementation is simply the identity function, returning `Orddict'
%% unchanged.

from_orddict(L) -> L.

-spec from_orddict(Orddict, Opts :: [Option]) -> Orddict when
      Option :: any(),
      Orddict :: orddict().

%% @doc Convert an ordered list of pairs to a dictionary. Like {@link
%% from_orddict/1}, but takes an option list just like {@link new/1}.

from_orddict(L, _Opts) ->
    from_orddict(L).

-spec size(Orddict) -> non_neg_integer() when
      Orddict :: orddict().

%% @doc Return the number of elements in a dictionary. Returns the number of
%% elements in `Orddict'. Note that this takes time proportional to the
%% number of elements, as it has to traverse the entire `Orddict'.

size(D) -> length(D).

-spec is_empty(Orddict) -> boolean() when
      Orddict :: orddict().

%% @doc Test for empty dictionary. Returns `true' if `Orddict' is empty,
%% and `false' otherwise. This is a constant time operation.

is_empty(Dict) -> Dict =:= [].

-spec info(Orddict) -> [InfoTuple] when
      Orddict :: orddict(),
      InfoTuple :: {InfoTag, Value},
      InfoTag :: 'size' | 'type',
      Value :: term().

%% @doc Get information about a dictionary. Returns a list of tagged tuples
%% corresponding to individual calls to {@link info/2} for all allowed tags.
%% Note that this takes time proportional to the number of elements.

info(Dict) ->
    Items = [size,type],
    [info(Item, Dict) || Item <- Items].

-spec info(InfoTag, Orddict) -> Value when
      Orddict :: orddict(),
      InfoTag :: 'size' | atom(),
      Value :: term().

%% @doc Get information about a dictionary. Note that getting the size of an
%% orddict takes time proportional to the number of elements.

info(size, Dict) -> size(Dict);
info(_, _) -> undefined.

-spec values(Orddict) -> [Val] when
      Orddict :: dict(),
      Val :: term().

%% @doc Get all values in a dictionary. Returns the values in `Orddict' as a
%% list, in the order they occur in `Orddict'. Duplicates are not removed.

values(Dict) ->
    %% don't use a list comprehension here - crash on malformed elements
    lists:map(fun ({_Key, Val}) -> Val end, Dict).

-spec values(Key, Orddict) -> [Value] | [] when
      Key :: term(),
      Orddict :: dict(),
      Value :: term().

%% @doc List the values (if any) stored for a key. Returns either a list
%% `[Value]' of length 1, where `Value' is the value stored for `Key' in
%% `Orddict', or `[]' if the key is not present in the dictionary.
%% @see find/2

values(Key, [{K,_}|D]) when Key > K -> values(Key, D);
values(Key, [{K,_}|_]) when Key < K -> [];
values(_Key, [{_K,Value}|_]) -> [Value];	%Key == K
values(_, []) -> [].

-spec fetch(Key, Orddict) -> Value when
      Key :: term(),
      Value :: term(),
      Orddict :: orddict().

%% @doc Get a value from a dictionary.
%% @deprecated This is an old name for {@link get/2}
%% @see get/2

fetch(Key, D) ->
    get(Key, D).

-spec get(Key, Orddict) -> Value when
      Key :: term(),
      Value :: term(),
      Orddict :: orddict().

%% @doc Get a value from a dictionary. This function returns the value
%% associated with `Key' in the dictionary `Orddict'. `get' assumes that
%% `Key' is present in the dictionary; if `Key' is not present, an exception
%% is generated instead.

get(Key, [{K,_}|D]) when Key > K -> get(Key, D);
get(Key, [{K,Value}|_]) when Key == K -> Value.

-spec get(Key, Default, Orddict) -> Value when
      Key :: term(),
      Default :: term(),
      Orddict :: orddict(),
      Value :: term().

%% @doc Get a value from a dictionary, or use a default. Returns the value
%% associated with `Key' in the dictionary `Orddict', or returns `Default'
%% if the key is not present in `OrdDict'.

get(Key, Def, [{K,_}|D]) when Key > K -> get(Key, Def, D);
get(Key, _Def, [{K,Value}|_]) when Key == K -> Value;
get(_Key, Def, []) -> Def.

-spec find(Key, Orddict) -> {'ok', Value} | 'error' when
      Key :: term(),
      Orddict :: orddict(),
      Value :: term().

%% @doc Search for a key in a dictionary. This function searches for a key
%% in a dictionary. Returns `{ok, Value}' where `Value' is the value
%% associated with `Key', or `error' if the key is not present in the
%% dictionary.

find(Key, [{K,_}|D]) when Key > K -> find(Key, D);
find(Key, [{K,_}|_]) when Key < K -> error;
find(_Key, [{_K,Value}|_]) -> {ok,Value};	%Key == K
find(_, []) -> error.

-spec take(Key, Orddict0) -> {Value, Orddict1} when
      Key :: term(),
      Orddict0 :: orddict(),
      Orddict1 :: orddict(),
      Value :: term().

%% @doc Extract an entry from a dictionary. Returns a tuple with the value
%% for `Key' in `Dict0' and a new dictionary with the entry for the key
%% deleted. Throws an exception if the key is not present in `Dict0'.

take(Key, Dict) ->
    {get(Key, Dict), erase(Key,Dict)}.

-spec fetch_keys(Orddict) -> Keys when
      Orddict :: orddict(),
      Keys :: [term()].

%% @doc Return all keys in a dictionary.
%% @deprecated This is an old name for {@link keys/1}.
%% @see keys/1

fetch_keys(Dict) -> keys(Dict).

-spec keys(Orddict) -> Keys when
      Orddict :: orddict(),
      Keys :: [term()].

%% @doc Return all keys in a dictionary. This function returns a list of all
%% keys in `Dict'. The result is also an ordered list.

keys([{Key,_}|Dict]) ->
    [Key|keys(Dict)];
keys([]) -> [].

-spec erase(Key, Orddict1) -> Orddict2 when
      Key :: term(),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

%% @doc Erase a key from a dictionary. This function erases all items with a
%% given key from a dictionary.

%% (note that this builds a new list even if Key is not present)
erase(Key, [{K,_}=E|Dict]) when Key > K -> [E|erase(Key, Dict)];
erase(Key, [{K,_}=E|Dict]) when Key < K -> [E|Dict];
erase(_Key, [{_K,_Val}|Dict]) -> Dict;		%Key == K
erase(_, []) -> [].

-spec store(Key, Value, Orddict1) -> Orddict2 when
      Key :: term(),
      Value :: term(),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

%% @doc Store a value in a dictionary. This function stores a `Key'-`Value'
%% pair in a dictionary. If `Key' already exists in `Orddict1', the
%% associated value is replaced by `Value'.

store(Key, New, [{K,_}=E|Dict]) when Key > K ->
    [E|store(Key, New, Dict)];
store(Key, New, [{K,_}=E|Dict]) when Key < K ->
    [{Key,New},E|Dict];
store(Key, New, [{_K,_Old}|Dict]) ->		%Key == K
    [{Key,New}|Dict];
store(Key, New, []) -> [{Key,New}].

-spec replace(Key, Value, Orddict1) -> Orddict2 when
      Key :: term(),
      Value :: term(),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

%% @doc Replace the value for a key in a dictionary. This function replaces
%% the current value for `Key' in `Orddict1' with `Value'. If `Key' does not
%% exist in `Orddict1', the dictionary is returned unchanged.

replace(Key, Val, Dict) ->
    case is_key(Key, Dict) of
        true -> store(Key, Val, Dict);
        false -> Dict
    end.

-spec append(Key, Value, Orddict1) -> Orddict2 when
      Key :: term(),
      Value :: term(),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

%% @doc Append a value to keys in a dictionary. This function appends a new
%% `Value' to the current list of values associated with `Key'. Note that it
%% is generally a bad idea to accumulate a set of values by appending to a
%% list, if there may be more than just a few elements per key.

append(Key, New, [{K,_}=E|Dict]) when Key > K ->
    [E|append(Key, New, Dict)];
append(Key, New, [{K,_}=E|Dict]) when Key < K ->
    [{Key,[New]},E|Dict];
append(Key, New, [{_K,Old}|Dict]) ->		%Key == K
    [{Key,Old ++ [New]}|Dict];
append(Key, New, []) -> [{Key,[New]}].

-spec append_list(Key, ValList, Orddict1) -> Orddict2 when
      Key :: term(),
      ValList :: [Value :: term()],
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

%% @doc Append new values to keys in a dictionary. This function appends a
%% list of values `ValList' to the current list of values associated with
%% `Key'. An exception is generated if the initial value associated with
%% `Key' is not a list of values. Note that it is generally a bad idea to
%% accumulate a set of values by appending to a list, if there may be more
%% than just a few elements per key.

append_list(Key, NewList, [{K,_}=E|Dict]) when Key > K ->
    [E|append_list(Key, NewList, Dict)];
append_list(Key, NewList, [{K,_}=E|Dict]) when Key < K ->
    [{Key,NewList},E|Dict];
append_list(Key, NewList, [{_K,Old}|Dict]) ->		%Key == K
    [{Key,Old ++ NewList}|Dict];
append_list(Key, NewList, []) ->
    [{Key,NewList}].

-spec first_key(Orddict) -> {ok, Key} | error when
      Orddict :: orddict(),
      Key :: term().

%% @doc Get the first key in the dictionary. Returns `{ok, Key}' where `Key'
%% is the smallest key in `Orddict', or returns 'error' if `Orddict' is
%% empty. This is a constant time operation.

first_key([{K, _}|_]) -> {ok,K};
first_key([]) -> error.

-spec take_first(Orddict1) -> {{Key, Val}, Orddict2} | error when
      Orddict1 :: orddict(),
      Orddict2 :: orddict(),
      Key :: term(),
      Val :: term().

%% @doc Extract the first entry in the dictionary. Returns the key/value
%% pair for the smallest key in `Orddict1' and a new dictionary with the
%% entry for the key deleted, or returns `error' if `Orddict1' is empty.
%% This is a constant time operation.

take_first([E|Es]) -> {E,Es};
take_first([]) -> error.

-spec next_key(Key, Orddict) -> {ok, Key1} | error when
      Orddict :: orddict(),
      Key :: term(),
      Key1 :: term().

%% @doc Get the next larger key in the dictionary. Returns `{ok, Larger}'
%% where `Larger' is the smallest key in `Orddict' larger than the given
%% `Key', or returns 'error' if `Key' is the last key in `Orddict'. Throws
%% an exception if `Key' does not exist in `Orddict'.
%%
%% Note that this takes time proportional to the position of `Key' in
%% `Orddict'.

next_key(Key, Dict) -> next_key(Key, Dict, Dict).

next_key(Key, [{K,_}|D], D0) when Key > K -> next_key(Key, D, D0);
next_key(Key, [{K,_}|_], D0) when Key < K -> erlang:error(badarg,[Key,D0]);
next_key(_Key, [{_K0,_},{K1,_}|_], _D0) -> {ok,K1};   %Key == K0
next_key(_Key, [{_K0,_}], _D0) -> error;
next_key(Key, [], D0) -> erlang:error(badarg, [Key,D0]).

-spec last_key(Orddict) -> {ok, Key} | error when
      Orddict :: orddict(),
      Key :: term().

%% @doc Get the last key in the dictionary. Returns `{ok, Key}' where `Key'
%% is the largest key in `Orddict', or returns 'error' if `Orddict' is
%% empty.
%%
%% Note that this takes time proportional to the size of `Orddict'.

last_key([{K, _}]) -> {ok,K};
last_key([{_, _}|D]) -> last_key(D);
last_key([]) -> error.

-spec take_last(Orddict1) -> {{Key, Val}, Orddict2} | error when
      Orddict1 :: orddict(),
      Orddict2 :: orddict(),
      Key :: term(),
      Val :: term().

%% @doc Extract the last entry in the dictionary. Returns the key/value pair
%% for the largest key in `Orddict1' and a new dictionary with the entry for
%% the key deleted, or returns `error' if `Orddict1' is empty.
%%
%% Note that this takes time proportional to the size of `Orddict'.

take_last([]) -> error.
take_last(Dict) -> foldrn(fun (E, {Last,Es}) -> {Last,[E|Es]} end,
                          fun (Last) -> {Last,[]} end, Dict).

-spec prev_key(Key, Orddict) -> {ok, Key1} | error when
      Orddict :: orddict(),
      Key :: term(),
      Key1 :: term().

%% @doc Get the next smaller key in the dictionary. Returns `{ok, Smaller}'
%% where `Smaller' is the largest key in `Dict' smaller than the given
%% `Key', or returns 'error' if `Key' is the smallest key in `Dict'. Throws
%% an exception if `Key' does not exist in `Dict'.

prev_key(Key, [E|Es]=Dict) -> prev_key(Key, Es, E, Dict);
prev_key(Key, []) -> erlang:error(badarg,[Key, []]).

prev_key(Key, [{K,_}=E|D], _E0, D0) when Key > K ->
    prev_key(Key, D, E, D0);
prev_key(Key, [{K,_}|_], _E0, D0) when Key < K ->
    erlang:error(badarg,[Key,D0]);
prev_key(_Key, [{_K,_}|_], {K0,_},  _D0) ->
    {ok,K0};   %Key == K
prev_key(Key, [], _E0, D0) ->
    erlang:error(badarg,[Key,D0]).

-spec update(Key, Fun, Orddict1) -> Orddict2 when
      Key :: term(),
      Fun :: fun((Value1 :: term()) -> Value2 :: term()),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

update(Key, Fun, [{K,_}=E|Dict]) when Key > K ->
    [E|update(Key, Fun, Dict)];
update(Key, Fun, [{K,Val}|Dict]) when Key == K ->
    [{Key,Fun(Val)}|Dict].

-spec update(Key, Fun, Initial, Orddict1) -> Orddict2 when
      Key :: term(),
      Initial :: term(),
      Fun :: fun((Value1 :: term()) -> Value2 :: term()),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

update(Key, _, Init, [{K,_}=E|Dict]) when Key < K ->
    [{Key,Init},E|Dict];
update(Key, Fun, Init, [{K,_}=E|Dict]) when Key > K ->
    [E|update(Key, Fun, Init, Dict)];
update(Key, Fun, _Init, [{_K,Val}|Dict]) ->		%Key == K
    [{Key,Fun(Val)}|Dict];
update(Key, _, Init, []) -> [{Key,Init}].

-spec update_counter(Key, Increment, Orddict1) -> Orddict2 when
      Key :: term(),
      Increment :: number(),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

update_counter(Key, Incr, [{K,_}=E|Dict]) when Key < K ->
    [{Key,Incr},E|Dict];
update_counter(Key, Incr, [{K,_}=E|Dict]) when Key > K ->
    [E|update_counter(Key, Incr, Dict)];
update_counter(Key, Incr, [{_K,Val}|Dict]) ->		%Key == K
    [{Key,Val+Incr}|Dict];
update_counter(Key, Incr, []) -> [{Key,Incr}].

-spec fold(Fun, Acc0, Orddict) -> Acc1 when
      Fun :: fun((Key :: term(), Value :: term(), AccIn :: term()) -> AccOut :: term()),
      Acc0 :: term(),
      Acc1 :: term(),
      Orddict :: orddict().

fold(F, Acc, [{Key,Val}|D]) ->
    fold(F, F(Key, Val, Acc), D);
fold(F, Acc, []) when is_function(F, 3) -> Acc.

-spec map(Fun, Orddict1) -> Orddict2 when
      Fun :: fun((Key :: term(), Value1 :: term()) -> Value2 :: term()),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

map(F, [{Key,Val}|D]) ->
    [{Key,F(Key, Val)}|map(F, D)];
map(F, []) when is_function(F, 2) -> [].

-spec filter(Pred, Orddict1) -> Orddict2 when
      Pred :: fun((Key :: term(), Value :: term()) -> boolean()),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

filter(F, [{Key,Val}=E|D]) ->
    case F(Key, Val) of
	true -> [E|filter(F, D)]; 
	false -> filter(F, D)
    end;
filter(F, []) when is_function(F, 2) -> [].

-spec merge(Fun, Orddict1, Orddict2) -> Orddict3 when
      Fun :: fun((Key :: term(), Value1 :: term(), Value2 :: term()) -> Value :: term()),
      Orddict1 :: orddict(),
      Orddict2 :: orddict(),
      Orddict3 :: orddict().

merge(F, [{K1,_}=E1|D1], [{K2,_}=E2|D2]) when K1 < K2 ->
    [E1|merge(F, D1, [E2|D2])];
merge(F, [{K1,_}=E1|D1], [{K2,_}=E2|D2]) when K1 > K2 ->
    [E2|merge(F, [E1|D1], D2)];
merge(F, [{K1,V1}|D1], [{_K2,V2}|D2]) ->	%K1 == K2
    [{K1,F(K1, V1, V2)}|merge(F, D1, D2)];
merge(F, [], D2) when is_function(F, 3) -> D2;
merge(F, D1, []) when is_function(F, 3) -> D1.
