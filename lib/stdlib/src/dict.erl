%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2011. All Rights Reserved.
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

%% We use the dynamic hashing techniques by Per-Åke Larsson as
%% described in "The Design and Implementation of Dynamic Hashing for
%% Sets and Tables in Icon" by Griswold and Townsend.  Much of the
%% terminology comes from that paper as well.
%%
%% The segments are all of the same fixed size and we just keep
%% increasing the size of the top tuple as the table grows.  At the
%% end of the segments tuple we keep an empty segment which we use
%% when we expand the segments.  The segments are expanded by doubling
%% every time n reaches maxn instead of increasing the tuple one
%% element at a time.  It is easier and does not seem detrimental to
%% speed.  The same applies when contracting the segments.
%%
%% Note that as the order of the keys is undefined we may freely
%% reorder keys within a bucket.

-module(dict).

-compile({no_auto_import,[size/1]}).

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

%% Low-level interface.
%%-export([get_slot/2,get_bucket/2,on_bucket/3,foldl/3,
%%	 maybe_expand/2,maybe_contract/2]).

%% This module also defines the standard dict behaviour.
-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].

behaviour_info(callbacks) ->
    [{new,0},{new,1},{from_orddict,1},{from_orddict,2},
     {from_list,1},{from_list,2},{to_orddict,1},{to_list,1},
     {is_empty,1},{size,1},{info,1},{info,2},
     {keys,1},
     {values,1},{values,2},% always return lists, possibly empty
     {get,2},{get,3},% for multimaps, return a list (empty if key not present)
     {find,2},% for multimaps, return {ok,List}|error, List nonempty
     {store,3},
     {replace,3},% (no effect if key not present)
     {erase,2},
     {map,2},{map,3},{map,4},% (map all/single entry/default)
     {increment,3},
     {filter,2},
     {foldl,3},{foldr,3},{foldl1,2},{foldr1,2},{foldln,3},{foldrn,3},
     {foreach,2},
     {merge,3},
     {first_key,1},{last_key,1},{next_key,2},{prev_key,2},
     {take_first,1},{take_last,2},{take,2}
    ];
behaviour_info(_Other) ->
    undefined.

%% Note: mk_seg/1 must be changed too if seg_size is changed.
-define(seg_size, 16).
-define(max_seg, 32).
-define(expand_load, 5).
-define(contract_load, 3).
-define(exp_size, (?seg_size * ?expand_load)).
-define(con_size, (?seg_size * ?contract_load)).

%% Define a hashtable.  The default values are the standard ones.
-record(dict,
	{size=0		      :: non_neg_integer(),   	% Number of elements
	 n=?seg_size	      :: non_neg_integer(),   	% Number of active slots
	 maxn=?seg_size	      :: non_neg_integer(),	% Maximum slots
	 bso=?seg_size div 2  :: non_neg_integer(),   	% Buddy slot offset
	 exp_size=?exp_size   :: non_neg_integer(),   	% Size to expand at
	 con_size=?con_size   :: non_neg_integer(),   	% Size to contract at
	 empty		      :: tuple(),		% Empty segment
	 segs		      :: tuple()	      	% Segments
	}).
%% A declaration equivalent to the following one is hard-coded in erl_types.
%% That declaration contains hard-coded information about the #dict{}
%% structure and the types of its fields.  So, please make sure that any
%% changes to its structure are also propagated to erl_types.erl.
%%
%% -opaque dict() :: #dict{}.

-define(kv(K,V), [K|V]).			% Key-Value pair format
%%-define(kv(K,V), {K,V}).			% Key-Value pair format

%% use this pattern to match for gb_trees
-define(gb(N,T), {N,T}).

%% Note: options are processed in the order they occur in the list, i.e.,
%% later options have higher precedence.

-record(opts, {type=hash}).

opts(Options) when is_list(Options) -> opts_0(Options);
opts(Option) -> opts_0([Option]).

opts_0(Options) ->
    opts_1(Options, #opts{}).

opts_1([hash | Options], _Type) ->
    opts_1(Options, #opts{type=hash});
opts_1([ordered | Options], _Type) ->
    opts_1(Options, #opts{type=ordered});
opts_1([], Type) ->
    Type.

-spec new() -> dict().

new() -> new([]).

new(Opts) ->
    try opts(Opts) of
        R -> new_1(R)
    catch
        _:_ -> erlang:error(badarg, [Opts])
    end.

new_1(#opts{type=hash}) ->
    new_dict();
new_1(#opts{type=ordered}) ->
    gb_trees:empty().

new_dict() ->
    Empty = mk_seg(?seg_size),
    #dict{empty=Empty,segs={Empty}}.

-spec is_key(Key, Dict) -> boolean() when
      Key :: term(),
      Dict :: dict().

is_key(Key, #dict{}=D) ->
    Slot = get_slot(D, Key),
    Bkt = get_bucket(D, Slot),
    find_key(Key, Bkt);
is_key(Key, ?gb(_,_)=D) ->
    gb_trees:is_defined(Key, D).

find_key(K, [?kv(K,_Val)|_]) -> true;
find_key(K, [_|Bkt]) -> find_key(K, Bkt);
find_key(_, []) -> false.

-spec to_list(Dict) -> List when
      Dict :: dict(),
      List :: [{Key :: term(), Value :: term()}].

to_list(#dict{}=D) ->
    %% list in default traversal order, hence foldr, not foldl
    foldr(fun (Key, Val, List) -> [{Key,Val}|List] end, [], D);
to_list(?gb(_,_)=D) ->
    gb_trees:to_list(D).

to_orddict(#dict{}=D) ->
    lists:keysort(1, to_list(D));
to_orddict(?gb(_,_)=D) ->
    gb_trees:to_list(D). % always ordered

-spec from_list(List) -> Dict when
      List :: [{Key :: term(), Value :: term()}],
      Dict :: dict().

%% note that for duplicate keys, later entries take precedence here
from_list(L) -> from_list(L, []).

from_list(L, Opts) ->
    try opts(Opts) of
        R -> from_list_1(L, R)
    catch
        _:_ -> erlang:error(badarg, [L, Opts])
    end.

from_list_1(L, #opts{type=hash}) ->
    lists:foldl(fun ({K,V}, D) -> store_dict(K, V, D) end, new_dict(), L);
from_list_1(L, #opts{type=ordered}) ->
    %% NOTE: using from_orddict(lists:ukeysort(1, L)) would be nice, but
    %% causes earlier entries to take precedence in case of duplicate keys
    lists:foldl(fun ({K,V}, D) -> gb_trees:enter(K, V, D) end,
                gb_trees:empty(), L).

from_orddict(L) -> from_orddict(L, []).

from_orddict(L, Opts) ->
    try opts(Opts) of
        R -> from_orddict_1(L, R)
    catch
        _:_ -> erlang:error(badarg, [L, Opts])
    end.

from_orddict_1(L, #opts{type=ordered}) ->
    gb_trees:from_orddict(L); % the list *must* be ordered for this!
from_orddict_1(L, Opts) ->
    from_list_1(L, Opts).

-spec size(Dict) -> non_neg_integer() when
      Dict :: dict().

size(#dict{size=N}) when is_integer(N), N >= 0 -> N;
size(?gb(N,_)) when is_integer(N), N >= 0 -> N.

is_empty(Dict) -> size(Dict) =< 0.

info(Dict) ->
    Items = [size],
    [info(Item, Dict) || Item <- Items].

info(size, Dict) -> size(Dict).

%% result is ordered (by keys) only if dict is ordered
values(Dict) ->
    %% list in default traversal order, hence foldr, not foldl
    foldr(fun (_Key, Val, Acc) -> [Val|Acc] end, [], Dict).

%% a variant of find/2 that returns a list of values, possibly empty
-spec values(Key, Dict) -> [Value] | [] when
      Key :: term(),
      Dict :: dict(),
      Value :: term().
values(Key, #dict{}=Dict) ->
    Slot = get_slot(Dict, Key),
    Bkt = get_bucket(Dict, Slot),
    values_1(Key, Bkt);
values(Key, ?gb(_,_)=Dict) ->
    gb_trees:values(Key,Dict).

values_1(K, [?kv(K,Val)|_]) -> [Val];
values_1(K, [_|Bkt]) -> find_val(K, Bkt);
values_1(_, []) -> [].

-spec fetch(Key, Dict) -> Value when
      Key :: term(),
      Dict :: dict(),
      Value :: term().

%% deprecated version of get/2
fetch(Key, D) ->
    get(Key, D).

get(Key, #dict{}=D) ->
    Slot = get_slot(D, Key),
    Bkt = get_bucket(D, Slot),
    try get_val(Key, Bkt)
    catch
	badarg -> erlang:error(badarg, [Key, D])
    end;
get(Key, ?gb(_,_)=D) ->
    gb_trees:get(Key, D).

get_val(K, [?kv(K,Val)|_]) -> Val;
get_val(K, [_|Bkt]) -> get_val(K, Bkt);
get_val(_, []) -> throw(badarg).

get(Key, Def, #dict{}=D) ->
    Slot = get_slot(D, Key),
    Bkt = get_bucket(D, Slot),
    get_val(Key, Bkt, Def);
get(Key, Def, ?gb(_,_)=D) ->
    gb_trees:get(Key, Def, D).

get_val(K, [?kv(K,Val)|_], _Def) -> Val;
get_val(K, [_|Bkt], Def) -> get_val(K, Bkt, Def);
get_val(_, [], Def) -> Def.

-spec find(Key, Dict) -> {'ok', Value} | 'error' when
      Key :: term(),
      Dict :: dict(),
      Value :: term().

find(Key, #dict{}=D) ->
    Slot = get_slot(D, Key),
    Bkt = get_bucket(D, Slot),
    find_val(Key, Bkt);
find(Key, ?gb(_,_)=D) ->
    gb_trees:find(Key, D).

find_val(K, [?kv(K,Val)|_]) -> {ok,Val};
find_val(K, [_|Bkt]) -> find_val(K, Bkt);
find_val(_, []) -> error.

-spec take(Key, Dict0) -> {Value, Dict1} when
      Key :: term(),
      Dict0 :: dict(),
      Dict1 :: dict(),
      Value :: term().
take(Key, Dict) ->
    {get(Key, Dict), erase(Key,Dict)}.

%% deprecated version of keys/1
fetch_keys(Dict) -> keys(Dict).

-spec keys(Dict) -> Keys when
      Dict :: dict(),
      Keys :: [term()].

%% result is ordered only if dict is ordered
keys(#dict{}=D) ->
    %% list in default traversal order, hence foldr, not foldl
    foldr(fun (Key, _Val, Keys) -> [Key|Keys] end, [], D);
keys(?gb(_,_)=D) ->
    gb_trees:keys(D).

-spec erase(Key, Dict1) -> Dict2 when
      Key :: term(),
      Dict1 :: dict(),
      Dict2 :: dict().
%%  Erase all elements with key Key.

%% Note: this builds a new data structure even if Key is not present...
erase(Key, #dict{}=D0) ->
    Slot = get_slot(D0, Key),
    {D1,Dc} = on_bucket(fun (B0) -> erase_key(Key, B0) end,
			D0, Slot),
    maybe_contract(D1, Dc);
erase(Key, ?gb(_,_)=D0) ->
    gb_trees:delete_any(Key, D0).

erase_key(Key, [?kv(Key,_Val)|Bkt]) -> {Bkt,1};
erase_key(Key, [E|Bkt0]) ->
    {Bkt1,Dc} = erase_key(Key, Bkt0),
    {[E|Bkt1],Dc};
erase_key(_, []) -> {[],0}.

-spec store(Key, Value, Dict1) -> Dict2 when
      Key :: term(),
      Value :: term(),
      Dict1 :: dict(),
      Dict2 :: dict().

store(Key, Val, #dict{}=D0) ->
    store_dict(Key, Val, D0);
store(Key, Val, ?gb(_,_)=D0) ->
    gb_trees:enter(Key, Val, D0).

store_dict(Key, Val, D0) ->
    Slot = get_slot(D0, Key),
    {D1,Ic} = on_bucket(fun (B0) -> store_bkt_val(Key, Val, B0) end,
			D0, Slot),
    maybe_expand(D1, Ic).

%% store_bkt_val(Key, Val, Bucket) -> {NewBucket,PutCount}.

store_bkt_val(Key, New, [?kv(Key,_Old)|Bkt]) -> {[?kv(Key,New)|Bkt],0};
store_bkt_val(Key, New, [Other|Bkt0]) ->
    {Bkt1,Ic} = store_bkt_val(Key, New, Bkt0),
    {[Other|Bkt1],Ic};
store_bkt_val(Key, New, []) -> {[?kv(Key,New)],1}.

%% no effect if key not present
replace(Key, Val, Dict) ->
    case is_key(Key, Dict) of
        true -> store(Key, Val, Dict);
        false -> Dict
    end.

-spec append(Key, Value, Dict1) -> Dict2 when
      Key :: term(),
      Value :: term(),
      Dict1 :: dict(),
      Dict2 :: dict().

append(Key, Val, #dict{}=D0) ->
    Slot = get_slot(D0, Key),
    {D1,Ic} = on_bucket(fun (B0) -> append_bkt(Key, Val, B0) end,
			D0, Slot),
    maybe_expand(D1, Ic);
append(Key, Val, ?gb(_,_)=D0) ->
    case gb_trees:lookup(Key, D0) of
        {value, Bag} -> gb_trees:update(Key, Bag ++ [Val], D0);
        none -> gb_trees:insert(Key, [Val], D0)
    end.

%% append_bkt(Key, Val, Bucket) -> {NewBucket,PutCount}.

append_bkt(Key, Val, [?kv(Key,Bag)|Bkt]) -> {[?kv(Key,Bag ++ [Val])|Bkt],0};
append_bkt(Key, Val, [Other|Bkt0]) ->
    {Bkt1,Ic} = append_bkt(Key, Val, Bkt0),
    {[Other|Bkt1],Ic};
append_bkt(Key, Val, []) -> {[?kv(Key,[Val])],1}.

-spec append_list(Key, ValList, Dict1) -> Dict2 when
      Key :: term(),
      ValList :: [Value :: term()],
      Dict1 :: dict(),
      Dict2 :: dict().

append_list(Key, L, #dict{}=D0) ->
    Slot = get_slot(D0, Key),
    {D1,Ic} = on_bucket(fun (B0) -> app_list_bkt(Key, L, B0) end,
			D0, Slot),
    maybe_expand(D1, Ic);
append_list(Key, L, ?gb(_,_)=D0) ->
    case gb_trees:lookup(Key, D0) of
        {value, Bag} -> gb_trees:update(Key, Bag ++ L, D0);
        none -> gb_trees:insert(Key, L, D0)
    end.

%% app_list_bkt(Key, L, Bucket) -> {NewBucket,PutCount}.

app_list_bkt(Key, L, [?kv(Key,Bag)|Bkt]) -> {[?kv(Key,Bag ++ L)|Bkt],0};
app_list_bkt(Key, L, [Other|Bkt0]) ->
    {Bkt1,Ic} = app_list_bkt(Key, L, Bkt0),
    {[Other|Bkt1],Ic};
app_list_bkt(Key, L, []) -> {[?kv(Key,L)],1}.

%% first_key(Table) -> {ok,Key} | error.
%% Find the "first" key in a Table (in the order defined by foldl).

first_key(#dict{}=T) ->
    case next_bucket(T, T#dict.n) of
 	[?kv(K,_Val)|_Bkt] -> {ok,K};
 	[] -> error				%No elements
    end;
first_key(?gb(_,_)=T) ->
    gb_trees:first_key(T).

%% see foldl for details on why this goes from higher bucket numbers to lower
next_bucket(_T, Slot) when Slot < 1 -> [];
next_bucket(T, Slot) ->
    case get_bucket(T, Slot) of
 	[] -> next_bucket(T, Slot-1);		%Empty bucket
 	B -> B
    end.

take_first(Dict) ->
    case first_key(Dict) of
        {ok, Key} ->
            {Val, Dict1} = take(Key, Dict),
            {{Key, Val}, Dict1};
        error ->
            error
    end.

%% next_key(Key, Table) -> {ok,NextKey} | error.
%% Find next key *following* Key in Table (in the order defined by foldl).

next_key(Key, #dict{}=T) ->
    Slot = get_slot(T, Key),
    B = get_bucket(T, Slot),
    %% Find a bucket with something in it.
    Bkt = case bucket_after_key(Key, B) of
 	      error -> erlang:error(badarg,[Key, T]);
 	      [] -> next_bucket(T, Slot-1);
 	      Found -> Found
 	  end,
    case Bkt of
 	[?kv(Next,_Val)|_] -> {ok,Next};
 	[] -> error				%We have reached the end!
    end;
next_key(Key, ?gb(_,_)=T) ->
    gb_trees:next_key(Key, T).

bucket_after_key(Key, [?kv(Key,_Val)|Bkt]) -> Bkt;
bucket_after_key(Key, [_Other|Bkt]) ->
    bucket_after_key(Key, Bkt);
bucket_after_key(_Key, []) -> error.		%Key not found!

%% last_key(Table) -> {ok,Key} | error.
%% Find the "last" key in a Table (in the order defined by foldl).

last_key(#dict{}=T) ->
    case prev_bucket(T, 1) of
 	[] -> error;				%No elements
 	Bkt ->
            ?kv(K,_Val) = lists:last(Bkt),
            {ok,K}
    end;
last_key(?gb(_,_)=T) ->
    gb_trees:last_key(T).

%% see foldl for details on why this goes from lower bucket numbers to higher
prev_bucket(T, Slot) when Slot > T#dict.n -> [];
prev_bucket(T, Slot) ->
    case get_bucket(T, Slot) of
 	[] -> prev_bucket(T, Slot+1);		%Empty bucket
 	B -> B
    end.

take_last(Dict) ->
    case last_key(Dict) of
        {ok, Key} ->
            {Val, Dict1} = take(Key, Dict),
            {{Key, Val}, Dict1};
        error ->
            error
    end.

%% prev_key(Key, Table) -> {ok,PrevKey} | error.
%% Find next key *before* Key in Table (in the order defined by foldl).

prev_key(Key, #dict{}=T) ->
    Slot = get_slot(T, Key),
    B = get_bucket(T, Slot),
    %% Find a bucket with something in it.
    Bkt = case bucket_before_key(Key, B) of
 	      error -> erlang:error(badarg,[Key, T]);
 	      [] -> prev_bucket(T, Slot+1);
 	      Found -> Found
 	  end,
    case Bkt of
 	[] -> error;				%We have reached the end!
        Bkt ->
            ?kv(Prev,_Val) = lists:last(Bkt),
            {ok,Prev}
    end;
prev_key(Key, ?gb(_,_)=T) ->
    gb_trees:prev_key(Key, T).

bucket_before_key(Key, [?kv(Key,_Val)|_]) -> []; % key was first in bucket
bucket_before_key(Key, [KV|Bkt]) ->
    bucket_before_key(Key, Bkt, KV);
bucket_before_key(_Key, []) -> error. %Key not found!

bucket_before_key(Key, [?kv(Key,_Val)|_],KV) -> [KV]; % return as a bucket
bucket_before_key(Key, [KV|Bkt],_PrevKV) ->
    bucket_before_key(Key, Bkt, KV);
bucket_before_key(_Key, [], _PrevKV) -> error. %Key not found!

%% deprecated version of map/3
-spec update(Fun, Key, Dict1) -> Dict2 when
      Key :: term(),
      Fun :: fun((Value1 :: term()) -> Value2 :: term()),
      Dict1 :: dict(),
      Dict2 :: dict().
update(Key, F, D0) ->
    map(F, Key, D0).

%% map fun to a particular key only, fail if key not present
-spec map(Fun, Key, Dict1) -> Dict2 when
      Key :: term(),
      Fun :: fun((Value1 :: term()) -> Value2 :: term()),
      Dict1 :: dict(),
      Dict2 :: dict().
map(F, Key, #dict{}=D0) ->
    Slot = get_slot(D0, Key),
    try on_bucket(fun (B0) -> update_bkt(Key, F, B0) end, D0, Slot) of
	{D1,_Uv} -> D1
    catch
	badarg -> erlang:error(badarg, [F, Key, D0])
    end;
map(F, Key, ?gb(_,_)=D0) ->
    gb_trees:map(F, Key, D0).

update_bkt(Key, F, [?kv(Key,Val)|Bkt]) ->
    Upd = F(Val),
    {[?kv(Key,Upd)|Bkt],Upd};
update_bkt(Key, F, [Other|Bkt0]) ->
    {Bkt1,Upd} = update_bkt(Key, F, Bkt0),
    {[Other|Bkt1],Upd};
update_bkt(_Key, _F, []) ->
    throw(badarg).

%% deprecated version of map/4
-spec update(Fun, Key, Initial, Dict1) -> Dict2 when
      Key :: term(),
      Initial :: term(),
      Fun :: fun((Value1 :: term()) -> Value2 :: term()),
      Dict1 :: dict(),
      Dict2 :: dict().
update(Key, F, Init, D0) ->
    map(F, Key, Init, D0).

%% map fun to a particular key only, insert default if key not present
-spec map(Fun, Key, Initial, Dict1) -> Dict2 when
      Key :: term(),
      Initial :: term(),
      Fun :: fun((Value1 :: term()) -> Value2 :: term()),
      Dict1 :: dict(),
      Dict2 :: dict().
map(F, Key, Init, #dict{}=D0) ->
    Slot = get_slot(D0, Key),
    {D1,Ic} = on_bucket(fun (B0) -> update_bkt(Key, F, Init, B0) end,
			D0, Slot),
    maybe_expand(D1, Ic);
map(F, Key, Init, ?gb(_,_)=D0) ->
    gb_trees:map(F, Key, Init, D0).

update_bkt(Key, F, _, [?kv(Key,Val)|Bkt]) ->
    {[?kv(Key,F(Val))|Bkt],0};
update_bkt(Key, F, I, [Other|Bkt0]) ->
    {Bkt1,Ic} = update_bkt(Key, F, I, Bkt0),
    {[Other|Bkt1],Ic};
update_bkt(Key, F, I, []) when is_function(F, 1) -> {[?kv(Key,I)],1}.

%% deprecated version of increment/3
-spec update_counter(Key, Increment, Dict1) -> Dict2 when
      Key :: term(),
      Increment :: number(),
      Dict1 :: dict(),
      Dict2 :: dict().
update_counter(Key, Incr, D0) ->
    increment(Key, Incr, D0).

-spec increment(Key, Increment, Dict1) -> Dict2 when
      Key :: term(),
      Increment :: number(),
      Dict1 :: dict(),
      Dict2 :: dict().

increment(Key, Incr, #dict{}=D0) when is_number(Incr) ->
    Slot = get_slot(D0, Key),
    {D1,Ic} = on_bucket(fun (B0) -> counter_bkt(Key, Incr, B0) end,
			D0, Slot),
    maybe_expand(D1, Ic);
increment(Key, Incr, ?gb(_,_)=D0) when is_number(Incr) ->
    gb_trees:increment(Key, Incr, D0).

counter_bkt(Key, I, [?kv(Key,Val)|Bkt]) ->
    {[?kv(Key,Val+I)|Bkt],0};
counter_bkt(Key, I, [Other|Bkt0]) ->
    {Bkt1,Ic} = counter_bkt(Key, I, Bkt0),
    {[Other|Bkt1],Ic};
counter_bkt(Key, I, []) -> {[?kv(Key,I)],1}.


%% get_slot(Hashdb, Key) -> Slot.
%%  Get the slot.  First hash on the new range, if we hit a bucket
%%  which has not been split use the unsplit buddy bucket.

get_slot(T, Key) ->
    H = erlang:phash(Key, T#dict.maxn),
    if
	H > T#dict.n -> H - T#dict.bso;
	true -> H
    end.

%% get_bucket(Hashdb, Slot) -> Bucket.

get_bucket(T, Slot) -> get_bucket_s(T#dict.segs, Slot).

%% on_bucket(Fun, Hashdb, Slot) -> {NewHashDb,Result}.
%%  Apply Fun to the bucket in Slot and replace the returned bucket.

on_bucket(F, T, Slot) ->
    SegI = ((Slot-1) div ?seg_size) + 1,
    BktI = ((Slot-1) rem ?seg_size) + 1,
    Segs = T#dict.segs,
    Seg = element(SegI, Segs),
    B0 = element(BktI, Seg),
    {B1,Res} = F(B0),				%Op on the bucket.
    {T#dict{segs=setelement(SegI, Segs, setelement(BktI, Seg, B1))},Res}.

%%  Work functions for fold, map and filter operations.  These
%%  traverse the hash structure rebuilding as necessary.  Note we
%%  could have implemented map and filter using fold but these are
%%  faster.  We hope!

%%  Fold function Fun over all "bags" in Table and return Accumulator.

%% deprecated version of foldl
-spec fold(Fun, Acc0, Dict) -> Acc1 when
      Fun :: fun((Key, Value, AccIn) -> AccOut),
      Key :: term(),
      Value :: term(),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      Dict :: dict().
fold(Fun, Acc0, Dict) -> foldl(Fun, Acc0, Dict).

%% fold in default traversal order (not key order unless dict is ordered)
-spec foldl(Fun, Acc0, Dict) -> Acc1 when
      Fun :: fun((Key, Value, AccIn) -> AccOut),
      Key :: term(),
      Value :: term(),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      Dict :: dict().
foldl(Fun, Acc, #dict{}=Dict) ->
    Segs = Dict#dict.segs,
    foldl_segs(Fun, Acc, Segs, tuple_size(Segs));
foldl(Fun, Acc, ?gb(_,_)=Dict) ->
    gb_trees:foldl(Fun, Acc, Dict).

%% Note that this traversal *defines* what the default order is - even if it
%% traverses the individual tuples from right to left, it's still "foldl"!
%% This order must match the functions first_key/last_key/next_key/prev_key!
%% (Rather than change the legacy traversal order of the default fold, we
%% have chosen to implement first/last/next/prev to go from higher bucket
%% indices towards lower, just like foldl.)
foldl_segs(F, Acc, Segs, I) when I >= 1 ->
    Seg = element(I, Segs),
    foldl_segs(F, foldl_seg(F, Acc, Seg, tuple_size(Seg)), Segs, I-1);
foldl_segs(F, Acc, _, 0) when is_function(F, 3) -> Acc.

foldl_seg(F, Acc, Seg, I) when I >= 1 ->
    foldl_seg(F, foldl_bucket(F, Acc, element(I, Seg)), Seg, I-1);
foldl_seg(F, Acc, _, 0) when is_function(F, 3) -> Acc.

foldl_bucket(F, Acc, [?kv(Key,Val)|Bkt]) ->
    foldl_bucket(F, F(Key, Val, Acc), Bkt);
foldl_bucket(F, Acc, []) when is_function(F, 3) -> Acc.

%% fold in reverse traversal order
foldr(Fun, Acc, #dict{}=Dict) ->
    Segs = Dict#dict.segs,
    foldr_segs(Fun, Acc, Segs, 1, tuple_size(Segs));
foldr(Fun, Acc, ?gb(_,_)=Dict) ->
    gb_trees:foldr(Fun, Acc, Dict).

foldr_segs(F, Acc, Segs, I, N) when I =< N ->
    Seg = element(I, Segs),
    foldr_segs(F, foldr_seg(F, Acc, Seg, 1, tuple_size(Seg)), Segs, I+1, N);
foldr_segs(F, Acc, _, _I, _N) when is_function(F, 3) -> Acc.

foldr_seg(F, Acc, Seg, I, N) when I =< N ->
    foldr_seg(F, foldr_bucket(F, Acc, element(I, Seg)), Seg, I+1, N);
foldr_seg(F, Acc, _, _I, _N) when is_function(F, 3) -> Acc.

foldr_bucket(F, Acc, [?kv(Key,Val)|Bkt]) ->
    F(Key, Val, foldr_bucket(F, Acc, Bkt));
foldr_bucket(F, Acc, []) when is_function(F, 3) -> Acc.

%% uses first element as initial accumulator, fails if list is empty
-spec foldl1(Fun, Dict) -> Acc when
      Fun :: fun((Key, Value, AccIn) -> AccOut),
      Key :: term(),
      Value :: term(),
      Acc :: term(),
      AccIn :: term(),
      AccOut :: term(),
      Dict :: dict().
foldl1(Fun, Dict) ->
    case take_first(Dict) of
        {{_Key, Val}, Dict1} -> foldl(Fun, Val, Dict1);
        error -> erlang:error(badarg, [Fun, Dict])
    end.

%% uses last element as initial accumulator, fails if list is empty
foldr1(Fun, Dict) ->
    case take_last(Dict) of
        {{_Key, Val}, Dict1} -> foldr(Fun, Val, Dict1);
        error -> erlang:error(badarg, [Fun, Dict])
    end.

%% applies InitFun to first element to get initial accumulator
-spec foldln(Fun, InitFun, Dict) -> Acc when
      Fun :: fun((Key, Value, AccIn) -> AccOut),
      InitFun :: fun((Key, Value) -> Acc0),
      Key :: term(),
      Value :: term(),
      Acc :: term(),
      Acc0 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      Dict :: dict().
foldln(Fun, InitFun, Dict) ->
    case take_first(Dict) of
        {{_Key, Val}, Dict1} -> foldl(Fun, InitFun(Val), Dict1);
        error -> erlang:error(badarg, [Fun, InitFun, Dict])
    end.

%% applies InitFun to last element to get initial accumulator
foldrn(Fun, InitFun, Dict) ->
    case take_last(Dict) of
        {{_Key, Val}, Dict1} -> foldr(Fun, InitFun(Val), Dict1);
        error -> erlang:error(badarg, [Fun, InitFun, Dict])
    end.

-spec map(Fun, Dict1) -> Dict2 when
      Fun :: fun((Key :: term(), Value1 :: term()) -> Value2 :: term()),
      Dict1 :: dict(),
      Dict2 :: dict().

map(F, #dict{}=D) ->
    Segs0 = tuple_to_list(D#dict.segs),
    Segs1 = map_seg_list(F, Segs0),
    D#dict{segs=list_to_tuple(Segs1)};
map(F, ?gb(_,_)=D) ->
    gb_trees:map(F, D).

map_seg_list(F, [Seg|Segs]) ->
    Bkts0 = tuple_to_list(Seg),
    Bkts1 = map_bkt_list(F, Bkts0),
    [list_to_tuple(Bkts1)|map_seg_list(F, Segs)];
map_seg_list(F, []) when is_function(F, 2) -> [].

map_bkt_list(F, [Bkt0|Bkts]) ->
    [map_bucket(F, Bkt0)|map_bkt_list(F, Bkts)];
map_bkt_list(F, []) when is_function(F, 2) -> [].

map_bucket(F, [?kv(Key,Val)|Bkt]) ->
    [?kv(Key,F(Key, Val))|map_bucket(F, Bkt)];
map_bucket(F, []) when is_function(F, 2) -> [].

-spec filter(Pred, Dict1) -> Dict2 when
      Pred :: fun((Key :: term(), Value :: term()) -> boolean()),
      Dict1 :: dict(),
      Dict2 :: dict().

filter(F, #dict{}=D) ->
    Segs0 = tuple_to_list(D#dict.segs),
    {Segs1,Fc} = filter_seg_list(F, Segs0, [], 0),
    maybe_contract(D#dict{segs=list_to_tuple(Segs1)}, Fc);
filter(F, ?gb(_,_)=D) ->
    gb_trees:filter(F, D).

filter_seg_list(F, [Seg|Segs], Fss, Fc0) ->
    Bkts0 = tuple_to_list(Seg),
    {Bkts1,Fc1} = filter_bkt_list(F, Bkts0, [], Fc0),
    filter_seg_list(F, Segs, [list_to_tuple(Bkts1)|Fss], Fc1);
filter_seg_list(F, [], Fss, Fc) when is_function(F, 2) ->
    {lists:reverse(Fss, []),Fc}.

filter_bkt_list(F, [Bkt0|Bkts], Fbs, Fc0) ->
    {Bkt1,Fc1} = filter_bucket(F, Bkt0, [], Fc0),
    filter_bkt_list(F, Bkts, [Bkt1|Fbs], Fc1);
filter_bkt_list(F, [], Fbs, Fc) when is_function(F, 2) ->
    {lists:reverse(Fbs),Fc}.

filter_bucket(F, [?kv(Key,Val)=E|Bkt], Fb, Fc) ->
    case F(Key, Val) of
	true -> filter_bucket(F, Bkt, [E|Fb], Fc);
	false -> filter_bucket(F, Bkt, Fb, Fc+1)
    end;
filter_bucket(F, [], Fb, Fc) when is_function(F, 2) ->
    {lists:reverse(Fb),Fc}.

-spec foreach(Fun, Dict) -> ok when
      Fun :: fun((Key :: term(), Value :: term()) -> term()),
      Dict :: dict().
foreach(Fun, Dict) ->
    foldl(fun (Key, Val, _Acc) -> Fun(Key, Val), ok end, ok, Dict).

-spec merge(Fun, Dict1, Dict2) -> Dict3 when
      Fun :: fun((Key :: term(), Value1 :: term(), Value2 :: term()) -> Value :: term()),
      Dict1 :: dict(),
      Dict2 :: dict(),
      Dict3 :: dict().

merge(Fun, Dict1, Dict2) ->
    case size(Dict1) < size(Dict2) of
        true ->
            merge_1(Fun, Dict1, Dict2);
        false ->
            merge_2(Fun, Dict1, Dict2)
    end.

merge_1(Fun, Dict1, Dict2) ->
    foldl(fun (K, V1, D) ->
                  update(K, fun (V2) -> Fun(K, V1, V2) end, V1, D)
          end, Dict2, Dict1).

merge_2(Fun, Dict1, Dict2) ->
    foldl(fun (K, V2, D) ->
                  update(K, fun (V1) -> Fun(K, V1, V2) end, V2, D)
          end, Dict1, Dict2).

%% get_bucket_s(Segments, Slot) -> Bucket.
%% put_bucket_s(Segments, Slot, Bucket) -> NewSegments.

get_bucket_s(Segs, Slot) ->
    SegI = ((Slot-1) div ?seg_size) + 1,
    BktI = ((Slot-1) rem ?seg_size) + 1,
    element(BktI, element(SegI, Segs)).

put_bucket_s(Segs, Slot, Bkt) ->
    SegI = ((Slot-1) div ?seg_size) + 1,
    BktI = ((Slot-1) rem ?seg_size) + 1,
    Seg = setelement(BktI, element(SegI, Segs), Bkt),
    setelement(SegI, Segs, Seg).

%% In maybe_expand(), the variable Ic only takes the values 0 or 1,
%% but type inference is not strong enough to infer this. Thus, the
%% use of explicit pattern matching and an auxiliary function.

maybe_expand(T, 0) -> maybe_expand_aux(T, 0);
maybe_expand(T, 1) -> maybe_expand_aux(T, 1).

maybe_expand_aux(T0, Ic) when T0#dict.size + Ic > T0#dict.exp_size ->
    T = maybe_expand_segs(T0),			%Do we need more segments.
    N = T#dict.n + 1,				%Next slot to expand into
    Segs0 = T#dict.segs,
    Slot1 = N - T#dict.bso,
    B = get_bucket_s(Segs0, Slot1),
    Slot2 = N,
    [B1|B2] = rehash(B, Slot1, Slot2, T#dict.maxn),
    Segs1 = put_bucket_s(Segs0, Slot1, B1),
    Segs2 = put_bucket_s(Segs1, Slot2, B2),
    T#dict{size=T#dict.size + Ic,
	   n=N,
	   exp_size=N * ?expand_load,
	   con_size=N * ?contract_load,
	   segs=Segs2};
maybe_expand_aux(T, Ic) -> T#dict{size=T#dict.size + Ic}.

maybe_expand_segs(T) when T#dict.n =:= T#dict.maxn ->
    T#dict{maxn=2 * T#dict.maxn,
	   bso=2 * T#dict.bso,
	   segs=expand_segs(T#dict.segs, T#dict.empty)};
maybe_expand_segs(T) -> T.

maybe_contract(T, Dc) when T#dict.size - Dc < T#dict.con_size,
			   T#dict.n > ?seg_size ->
    N = T#dict.n,
    Slot1 = N - T#dict.bso,
    Segs0 = T#dict.segs,
    B1 = get_bucket_s(Segs0, Slot1),
    Slot2 = N,
    B2 = get_bucket_s(Segs0, Slot2),
    Segs1 = put_bucket_s(Segs0, Slot1, B1 ++ B2),
    Segs2 = put_bucket_s(Segs1, Slot2, []),	%Clear the upper bucket
    N1 = N - 1,
    maybe_contract_segs(T#dict{size=T#dict.size - Dc,
			       n=N1,
			       exp_size=N1 * ?expand_load,
			       con_size=N1 * ?contract_load,
			       segs=Segs2});
maybe_contract(T, Dc) -> T#dict{size=T#dict.size - Dc}.

maybe_contract_segs(T) when T#dict.n =:= T#dict.bso ->
    T#dict{maxn=T#dict.maxn div 2,
	   bso=T#dict.bso div 2,
	   segs=contract_segs(T#dict.segs)};
maybe_contract_segs(T) -> T.

%% rehash(Bucket, Slot1, Slot2, MaxN) -> [Bucket1|Bucket2].
%%  Yes, we should return a tuple, but this is more fun.

rehash([?kv(Key,_Bag)=KeyBag|T], Slot1, Slot2, MaxN) ->
    [L1|L2] = rehash(T, Slot1, Slot2, MaxN),
    case erlang:phash(Key, MaxN) of
	Slot1 -> [[KeyBag|L1]|L2];
	Slot2 -> [L1|[KeyBag|L2]]
    end;
rehash([], _Slot1, _Slot2, _MaxN) -> [[]|[]].

%% mk_seg(Size) -> Segment.

mk_seg(16) -> {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}.

%% expand_segs(Segs, EmptySeg) -> NewSegs.
%% contract_segs(Segs) -> NewSegs.
%%  Expand/contract the segment tuple by doubling/halving the number
%%  of segments.  We special case the powers of 2 upto 32, this should
%%  catch most case.  N.B. the last element in the segments tuple is
%%  an extra element containing a default empty segment.

expand_segs({B1}, Empty) ->
    {B1,Empty};
expand_segs({B1,B2}, Empty) ->
    {B1,B2,Empty,Empty};
expand_segs({B1,B2,B3,B4}, Empty) ->
    {B1,B2,B3,B4,Empty,Empty,Empty,Empty};
expand_segs({B1,B2,B3,B4,B5,B6,B7,B8}, Empty) ->
    {B1,B2,B3,B4,B5,B6,B7,B8,
     Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty};
expand_segs({B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16}, Empty) ->
    {B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16,
     Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
     Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty};
expand_segs(Segs, Empty) ->
    list_to_tuple(tuple_to_list(Segs)
    ++ lists:duplicate(tuple_size(Segs), Empty)).

contract_segs({B1,_}) ->
    {B1};
contract_segs({B1,B2,_,_}) ->
    {B1,B2};
contract_segs({B1,B2,B3,B4,_,_,_,_}) ->
    {B1,B2,B3,B4};
contract_segs({B1,B2,B3,B4,B5,B6,B7,B8,_,_,_,_,_,_,_,_}) ->
    {B1,B2,B3,B4,B5,B6,B7,B8};
contract_segs({B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16,
	       _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_}) ->
    {B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16};
contract_segs(Segs) ->
    Ss = tuple_size(Segs) div 2,
    list_to_tuple(lists:sublist(tuple_to_list(Segs), 1, Ss)).
