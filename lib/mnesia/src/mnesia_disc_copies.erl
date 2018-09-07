%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-compile([nowarn_unused_vars]).

%% @doc ram copies storage backend for Mnesia.

%% Initialization: register() or register(Alias)
%% Usage: mnesia:create_table(Tab, [{disc_copies, Nodes}, ...]).

-module(mnesia_disc_copies).


%% ----------------------------------------------------------------------------
%% BEHAVIOURS
%% ----------------------------------------------------------------------------

%%-behaviour(mnesia_backend_type).
%%-behaviour(gen_server).  %% CURRENTLY UNUSED


%% ----------------------------------------------------------------------------
%% EXPORTS
%% ----------------------------------------------------------------------------

%%
%% CONVENIENCE API
%%

-export([register/0,
         register/1]).

%%
%% DEBUG API
%%

-export([show_table/1,
         show_table/2,
         show_table/3]).

%%
%% BACKEND CALLBACKS
%%

%% backend management
-export([init_backend/0,
         add_aliases/1,
         remove_aliases/1]).

%% schema level callbacks
-export([semantics/2,
	 check_definition/4,
	 create_table/3,
	 load_table/4,
	 close_table/2,
	 sync_close_table/2,
	 delete_table/2,
	 info/3]).

%% table synch calls
-export([sender_init/4,
         sender_handle_info/5,
         receiver_first_message/4,
         receive_data/5,
         receive_done/4]).

%% low-level accessor callbacks.
-export([delete/3,
         first/2,
         fixtable/3,
         insert/3,
         last/2,
         lookup/3,
         match_delete/3,
         next/3,
         prev/3,
         repair_continuation/2,
         select/1,
         select/3,
         select/4,
         slot/3,
         update_counter/4]).

%% Index consistency
-export([index_is_consistent/3,
         is_index_consistent/2]).

%% record and key validation
-export([validate_key/6,
         validate_record/6]).

%% file extension callbacks
-export([real_suffixes/0,
         tmp_suffixes/0]).

%%
%% GEN SERVER CALLBACKS AND CALLS
%%

%% -export([start_proc/4,
%%          init/1,
%%          handle_call/3,
%%          handle_info/2,
%%          handle_cast/2,
%%          terminate/2,
%%          code_change/3]).

%% ----------------------------------------------------------------------------
%% DEFINES
%% ----------------------------------------------------------------------------

-define(leveldb, 'FIXME'). % FIXME: delete this when all uses are removed

-define(ALIAS, disc_copies).

%% enable debugging messages through mnesia:set_debug_level(debug)
-ifndef(MNESIA_DISC_COPIES_NO_DBG).
-define(dbg(Fmt, Args),
        %% avoid evaluating Args if the message will be dropped anyway
        case mnesia_monitor:get_env(debug) of
            none -> ok;
            verbose -> ok;
            _ -> mnesia_lib:dbg_out("~p:~p: "++(Fmt),[?MODULE,?LINE|Args])
        end).
-else.
-define(dbg(Fmt, Args), ok).
-endif.

%% ----------------------------------------------------------------------------
%% RECORDS
%% ----------------------------------------------------------------------------


%% ----------------------------------------------------------------------------
%% CONVENIENCE API
%% ----------------------------------------------------------------------------

%% not really useful for a built-in storage type but we keep this for now

register() ->
    register(?ALIAS).

register(Alias) ->
    Module = ?MODULE,
    case mnesia:add_backend_type(Alias, Module) of
        {atomic, ok} ->
            {ok, Alias};
        {aborted, {backend_type_already_exists, _}} ->
            {ok, Alias};
        {aborted, Reason} ->
            {error, Reason}
    end.


%% ----------------------------------------------------------------------------
%% DEBUG API
%% ----------------------------------------------------------------------------

%% A debug function that shows the raw table content
show_table(Tab) ->
    show_table(?ALIAS, Tab).

show_table(Alias, Tab) ->
    show_table(Alias, Tab, 100).

%% FIXME: maybe implement later

show_table(Alias, Tab, Limit) ->
    ok.
    %% {Ref, _Type} = get_ref(Alias, Tab),
    %% with_iterator(Ref, fun(I) -> i_show_table(I, first, Limit) end).

%% i_show_table(_, _, 0) ->
%%     {error, skipped_some};
%% i_show_table(I, Move, Limit) ->
%%     case ?leveldb:iterator_move(I, Move) of
%%         {ok, EncKey, EncVal} ->
%%             {Type,Val} =
%%                 case EncKey of
%%                     << "INFO_TAG", K/binary >> ->
%%                         {info,{decode_key(K),decode_val(EncVal)}};
%%                     _ ->
%%                         K = decode_key(EncKey),
%%                         V = decode_val(EncVal),
%%                         V2 = setelement(2,V,K),
%%                         {data,V2}
%%                 end,
%%             io:fwrite("~p: ~p~n", [Type, Val]),
%%             i_show_table(I, next, Limit-1);
%%         _ ->
%%             ok
%%     end.


%% ----------------------------------------------------------------------------
%% BACKEND CALLBACKS
%% ----------------------------------------------------------------------------

%% backend management

init_backend() ->
    ok.

add_aliases(_Aliases) ->
    ok.

remove_aliases(_Aliases) ->
    %% currently not called directly by Mnesia?
    ok.

%% schema level callbacks

%% This function is used to determine what the plugin supports
%% semantics(Alias, storage)   ->
%%    ram_copies | disc_copies | disc_only_copies  (mandatory)
%% semantics(Alias, types)     ->
%%    [bag | set | ordered_set]                    (mandatory)
%% semantics(Alias, index_types) ->
%%    [bag | ordered]                              (mandatory)
%% semantics(Alias, index_fun) ->
%%    fun(Alias, Tab, Pos, Obj) -> [IxValue]       (optional)
%% semantics(Alias, _) ->
%%    undefined.
%%
semantics(_Alias, storage) -> disc_copies;
semantics(_Alias, types  ) -> [set, ordered_set, bag];
semantics(_Alias, index_types) -> [bag, ordered];
semantics(_Alias, index_fun) -> fun index_f/4;
semantics(_Alias, _) -> undefined.

%% not exported
index_f(_Alias, _Tab, Pos, Obj) ->
    [element(Pos, Obj)].

is_index_consistent(Alias, {_Tab, index, _PosInfo}) ->
    true.

index_is_consistent(Alias, {_Tab, index, _PosInfo}, Bool)
  when is_boolean(Bool) ->
    ok.

%% For now, only verify that the type is set or ordered_set.
%% set is OK as ordered_set is a kind of set.
check_definition(Alias, Tab, Nodes, Props) ->
    %% FIXME: rewrite for disc_copies
    Id = {Alias, Nodes},
    Props1 = lists:map(
               fun({type, T} = P) ->
                       if T==set; T==ordered_set; T==bag ->
                               P;
                          true ->
                               mnesia:abort({combine_error,
                                             Tab,
                                             [Id, {type,T}]})
                       end;
                  ({user_properties, _} = P) ->
                       %% should perhaps verify eleveldb options...
                       P;
                  (P) -> P
               end, Props),
    {ok, Props1}.

%% -> ok | {error, exists}
create_table(_Alias, Tab, _Props) ->
    %% FIXME: rewrite for disc_copies
    ok.
    %% create_mountpoint(Tab).

load_table(Alias, Tab, _LoadReason, Opts) ->
    %% FIXME: rewrite for disc_copies
    ok.
    %% Type = proplists:get_value(type, Opts),
    %% LdbUserProps = proplists:get_value(
    %%                  leveldb_opts, proplists:get_value(
    %%                                  user_properties, Opts, []), []),
    %% StorageProps = proplists:get_value(
    %%                  leveldb, proplists:get_value(
    %%                             storage_properties, Opts, []), LdbUserProps),
    %% LdbOpts = mnesia_eleveldb_params:lookup(Tab, StorageProps),
    %% ProcName = proc_name(Alias, Tab),
    %% case whereis(ProcName) of
    %%     undefined ->
    %%         load_table_(Alias, Tab, Type, LdbOpts);
    %%     Pid ->
    %%         gen_server:call(Pid, {load, Alias, Tab, Type, LdbOpts}, infinity)
    %% end.

%% load_table_(Alias, Tab, Type, LdbOpts) ->
%%     ShutdownTime = proplists:get_value(
%%                      owner_shutdown_time, LdbOpts, 120000),
%%     case mnesia_ext_sup:start_proc(
%%            Tab, ?MODULE, start_proc, [Alias,Tab,Type, LdbOpts],
%%            [{shutdown, ShutdownTime}]) of
%%         {ok, _Pid} ->
%%             ok;

%%         %% TODO: This reply is according to the manual, but we dont get it.
%%         {error, {already_started, _Pid}} ->
%%             %% TODO: Is it an error if the table already is
%%             %% loaded. This printout is triggered when running
%%             %% transform_table on a leveldb_table that has indexing.
%%             ?dbg("ERR: table:~p already loaded pid:~p~n",
%%                  [Tab, _Pid]),
%%             ok;

%%         %% TODO: This reply is not according to the manual, but we get it.
%%         {error, {{already_started, _Pid}, _Stack}} ->
%%             %% TODO: Is it an error if the table already is
%%             %% loaded. This printout is triggered when running
%%             %% transform_table on a leveldb_table that has indexing.
%%             ?dbg("ERR: table:~p already loaded pid:~p stack:~p~n",
%%                  [Tab, _Pid, _Stack]),
%%             ok
%%     end.

close_table(Alias, Tab) ->
    %% FIXME: rewrite for disc_copies
    ok.
    %% ?dbg("~p: close_table(~p, ~p);~n Trace: ~s~n",
    %%      [self(), Alias, Tab, pp_stack()]),
    %% if is_atom(Tab) ->
    %%         [close_table(Alias, R)
    %%          || {R, _} <- related_resources(Tab)];
    %%    true ->
    %%         ok
    %% end,
    %% close_table_(Alias, Tab).

%% close_table_(Alias, Tab) ->
%%     case opt_call(Alias, Tab, close_table) of
%%         {error, noproc} ->
%%             ?dbg("~p: close_table_(~p) -> noproc~n",
%%                  [self(), Tab]),
%%             ok;
%%         {ok, _} ->
%%             ok;
%%         _Other ->
%%             ?dbg("~p: close_table_(~p) -> _Other = ~p~n",
%%                  [self(), Tab, _Other]),
%%             mnesia_ext_sup:stop_proc(Tab),
%%             ok
%%     end.

sync_close_table(Alias, Tab) ->
    %% FIXME: rewrite for disc_copies
    ok.
    %% ?dbg("~p: sync_close_table(~p, ~p);~n Trace: ~s~n",
    %%      [self(), Alias, Tab, pp_stack()]),
    %% close_table(Alias, Tab).

delete_table(Alias, Tab) ->
    %% FIXME: rewrite for disc_copies
    ok.
    %% ?dbg("~p: delete_table(~p, ~p);~n Trace: ~s~n",
    %%      [self(), Alias, Tab, pp_stack()]),
    %% delete_table(Alias, Tab, data_mountpoint(Tab)).

%% delete_table(Alias, Tab, MP) ->
%%     if is_atom(Tab) ->
%%             [delete_table(Alias, T, M) || {T,M} <- related_resources(Tab)];
%%        true ->
%%             ok
%%     end,
%%     case opt_call(Alias, Tab, delete_table) of
%%         {error, noproc} ->
%%             do_delete_table(Tab, MP);
%%         {ok, _} ->
%%             ok
%%     end.

%% do_delete_table(Tab, MP) ->
%%     assert_proper_mountpoint(Tab, MP),
%%     destroy_db(MP, []).


%% FIXME: rewrite for disc_copies
%% info(_Alias, Tab, memory) ->
%%     try ets:info(tab_name(icache, Tab), memory)
%%     catch
%%         error:_ ->
%%             0
%%     end;
%% info(Alias, Tab, size) ->
%%     case retrieve_size(Alias, Tab) of
%% 	{ok, Size} ->
%% 	    if Size < 10000 -> ok;
%% 	       true -> size_warning(Alias, Tab)
%% 	    end,
%% 	    Size;
%% 	Error ->
%% 	    Error
%%     end;
info(_Alias, Tab, Item) ->
    case try_read_info(Tab, Item, undefined) of
        {ok, Value} ->
            Value;
        Error ->
            Error
    end.

%% retrieve_size(_Alias, Tab) ->
%%     case try_read_info(Tab, size, 0) of
%% 	{ok, Size} ->
%% 	    {ok, Size};
%% 	Error ->
%% 	    Error
%%     end.

try_read_info(Tab, Item, Default) ->
    try
        {ok, read_info(Item, Default, tab_name(icache, Tab))}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_info(_Item, _Default, _TabName) ->
    {ok, []}.

%% write_info(Alias, Tab, Key, Value) ->
%%     ok.
%%     %% call(Alias, Tab, {write_info, Key, Value}).

tab_name(icache, Tab) ->
    list_to_atom("mnesia_ext_icache_" ++ tabname(Tab));
tab_name(info, Tab) ->
    list_to_atom("mnesia_ext_info_" ++ tabname(Tab)).

tabname({Tab, index, {{Pos},_}}) ->
    atom_to_list(Tab) ++ "-=" ++ atom_to_list(Pos) ++ "=-_ix";
tabname({Tab, index, {Pos,_}}) ->
    atom_to_list(Tab) ++ "-" ++ integer_to_list(Pos) ++ "-_ix";
tabname(Tab) when is_atom(Tab) ->
    atom_to_list(Tab) ++ "-_tab".

%% proc_name(_Alias, Tab) ->
%%     list_to_atom("mnesia_ext_proc_" ++ tabname(Tab)).

%% table synch calls

%% ===========================================================
%% Table synch protocol
%% Callbacks are
%% Sender side:
%%  1. sender_init(Alias, Tab, RemoteStorage, ReceiverPid) ->
%%        {standard, InitFun, ChunkFun} | {InitFun, ChunkFun} when
%%        InitFun :: fun() -> {Recs, Cont} | '$end_of_table'
%%        ChunkFun :: fun(Cont) -> {Recs, Cont1} | '$end_of_table'
%%
%%       If {standard, I, C} is returned, the standard init message will be
%%       sent to the receiver. Matching on RemoteStorage can reveal if a
%%       different protocol can be used.
%%
%%  2. InitFun() is called
%%  3a. ChunkFun(Cont) is called repeatedly until done
%%  3b. sender_handle_info(Msg, Alias, Tab, ReceiverPid, Cont) ->
%%        {ChunkFun, NewCont}
%%
%% Receiver side:
%% 1. receiver_first_message(SenderPid, Msg, Alias, Tab) ->
%%        {Size::integer(), State}
%% 2. receive_data(Data, Alias, Tab, _Sender, State) ->
%%        {more, NewState} | {{more, Msg}, NewState}
%% 3. receive_done(_Alias, _Tab, _Sender, _State) ->
%%        ok
%%
%% The receiver can communicate with the Sender by returning
%% {{more, Msg}, St} from receive_data/4. The sender will be called through
%% sender_handle_info(Msg, ...), where it can adjust its ChunkFun and
%% Continuation. Note that the message from the receiver is sent once the
%% receive_data/4 function returns. This is slightly different from the
%% normal mnesia table synch, where the receiver acks immediately upon
%% reception of a new chunk, then processes the data.
%%

sender_init(Alias, Tab, _RemoteStorage, _Pid) ->
    %% FIXME: rewrite for disc_copies
    %% Need to send a message to the receiver. It will be handled in
    %% receiver_first_message/4 below. There could be a volley of messages...
    {standard,
     fun() ->
             select(Alias, Tab, [{'_',[],['$_']}], 100)
     end,
     chunk_fun()}.

sender_handle_info(_Msg, _Alias, _Tab, _ReceiverPid, Cont) ->
    %% FIXME: rewrite for disc_copies
    %% ignore - we don't expect any message from the receiver
    {chunk_fun(), Cont}.

receiver_first_message(_Pid, {first, Size} = _Msg, _Alias, _Tab) ->
    %% FIXME: rewrite for disc_copies
    {Size, _State = []}.

receive_data(Data, Alias, Tab, _Sender, State) ->
    %% FIXME: rewrite for disc_copies
    [insert(Alias, Tab, Obj) || Obj <- Data],
    {more, State}.

receive_done(_Alias, _Tab, _Sender, _State) ->
    %% FIXME: rewrite for disc_copies
    ok.

%% End of table synch protocol
%% ===========================================================

%% PRIVATE

chunk_fun() ->
    fun(Cont) ->
            select(Cont)
    end.

%% low-level accessor callbacks.

delete(Alias, Tab, Key) ->
    %% FIXME: rewrite for disc_copies
    %%opt_call(Alias, Tab, {delete, encode_key(Key)}),
    ok.

first(Alias, Tab) ->
    %% FIXME: rewrite for disc_copies
    ok.
    %% %% {Ref, _Type} = get_ref(Alias, Tab),
    %% with_keys_only_iterator(Ref, fun i_first/1).

%% %% PRIVATE ITERATOR
%% i_first(I) ->
%%     case ?leveldb:iterator_move(I, <<"DATA_START">>) of
%% 	{ok, First} ->
%% 	    decode_key(First);
%% 	_ ->
%% 	    '$end_of_table'
%%     end.

%% Not relevant for an ordered_set
fixtable(_Alias, _Tab, _Bool) ->
    %% FIXME: rewrite for disc_copies
    true.

%% To save storage space, we avoid storing the key twice. We replace the key
%% in the record with []. It has to be put back in lookup/3.
insert(Alias, Tab, Obj) ->
    %% FIXME: rewrite for disc_copies
    ok.
    %% Pos = keypos(Tab),
    %% EncKey = encode_key(element(Pos, Obj)),
    %% EncVal = encode_val(setelement(Pos, Obj, [])),
    %% call(Alias, Tab, {insert, EncKey, EncVal}).

last(Alias, Tab) ->
    %% FIXME: rewrite for disc_copies
    ok.
    %% {Ref, _Type} = get_ref(Alias, Tab),
    %% with_keys_only_iterator(Ref, fun i_last/1).

%% %% PRIVATE ITERATOR
%% i_last(I) ->
%%     case ?leveldb:iterator_move(I, last) of
%% 	{ok, << "INFO_TAG", _/binary >>} ->
%% 	    '$end_of_table';
%% 	{ok, Last} ->
%% 	    decode_key(Last);
%% 	_ ->
%% 	    '$end_of_table'
%%     end.

%% Since we replace the key with [] in the record, we have to put it back
%% into the found record.
lookup(Alias, Tab, Key) ->
    %% FIXME: rewrite for disc_copies
    ok.
%%     Enc = encode_key(Key),
%%     {Ref, Type} = call(Alias, Tab, get_ref),
%%     case Type of
%% 	bag -> lookup_bag(Ref, Key, Enc, keypos(Tab));
%% 	_ ->
%% 	    case ?leveldb:get(Ref, Enc, []) of
%% 		{ok, EncVal} ->
%% 		    [setelement(keypos(Tab), decode_val(EncVal), Key)];
%% 		_ ->
%% 		    []
%% 	    end
%%     end.

%% lookup_bag(Ref, K, Enc, KP) ->
%%     Sz = byte_size(Enc),
%%     with_iterator(
%%       Ref, fun(I) ->
%% 		   lookup_bag_(Sz, Enc, ?leveldb:iterator_move(I, Enc),
%% 			       K, I, KP)
%% 	   end).

%% lookup_bag_(Sz, Enc, {ok, Enc, _}, K, I, KP) ->
%%     lookup_bag_(Sz, Enc, ?leveldb:iterator_move(I, next), K, I, KP);
%% lookup_bag_(Sz, Enc, Res, K, I, KP) ->
%%     case Res of
%% 	{ok, <<Enc:Sz/binary, _:42>>, V} ->
%% 	    [setelement(KP, decode_val(V), K)|
%% 	     lookup_bag_(Sz, Enc, ?leveldb:iterator_move(I, next), K, I, KP)];
%% 	_ ->
%% 	    []
%%     end.

%% FIXME: rewrite for disc_copies
match_delete(Alias, Tab, Pat) when is_atom(Pat) ->
    ok;
    %% %do_match_delete(Alias, Tab, '_'),
    %% case is_wild(Pat) of
    %%     true ->
    %%         call(Alias, Tab, clear_table),
    %%         ok;
    %%     false ->
    %%         %% can this happen??
    %%         error(badarg)
    %% end;
match_delete(Alias, Tab, Pat) when is_tuple(Pat) ->
    %% KP = keypos(Tab),
    %% Key = element(KP, Pat),
    %% case is_wild(Key) of
    %%     true ->
    %%         call(Alias, Tab, clear_table);
    %%     false ->
    %%         call(Alias, Tab, {match_delete, Pat})
    %% end,
    ok.


next(Alias, Tab, Key) ->
    %% FIXME: rewrite for disc_copies
    ok.
    %% {Ref, _Type} = get_ref(Alias, Tab),
    %% EncKey = encode_key(Key),
    %% with_keys_only_iterator(Ref, fun(I) -> i_next(I, EncKey, Key) end).

%% %% PRIVATE ITERATOR
%% i_next(I, EncKey, Key) ->
%%     case ?leveldb:iterator_move(I, EncKey) of
%%         {ok, EncKey} ->
%%             i_next_loop(?leveldb:iterator_move(I, next), I, Key);
%%         Other ->
%%             i_next_loop(Other, I, Key)
%%     end.

%% i_next_loop({ok, EncKey}, I, Key) ->
%%     case decode_key(EncKey) of
%%         Key ->
%%             i_next_loop(?leveldb:iterator_move(I, next), I, Key);
%%         NextKey ->
%%             NextKey
%%     end;
%% i_next_loop(_, _I, _Key) ->
%%     '$end_of_table'.

prev(Alias, Tab, Key0) ->
    %% FIXME: rewrite for disc_copies
    ok.
    %% {Ref, _Type} = call(Alias, Tab, get_ref),
    %% Key = encode_key(Key0),
    %% with_keys_only_iterator(Ref, fun(I) -> i_prev(I, Key) end).

%% %% PRIVATE ITERATOR
%% i_prev(I, Key) ->
%%     case ?leveldb:iterator_move(I, Key) of
%% 	{ok, _} ->
%% 	    i_move_to_prev(I, Key);
%% 	{error, invalid_iterator} ->
%% 	    i_last(I)
%%     end.

%% %% PRIVATE ITERATOR
%% i_move_to_prev(I, Key) ->
%%     case ?leveldb:iterator_move(I, prev) of
%% 	{ok, << "INFO_TAG", _/binary >>} ->
%% 	    '$end_of_table';
%% 	{ok, Prev} when Prev < Key ->
%% 	    decode_key(Prev);
%% 	{ok, _} ->
%% 	    i_move_to_prev(I, Key);
%% 	_ ->
%% 	    '$end_of_table'
%%     end.

repair_continuation(Cont, _Ms) ->
    %% FIXME: rewrite for disc_copies
    Cont.

select(Cont) ->
    %% FIXME: rewrite for disc_copies
    %% Handle {ModOrAlias, Cont} wrappers for backwards compatibility with
    %% older versions of mnesia_ext (before OTP 20).
    case Cont of
        {_, '$end_of_table'} -> '$end_of_table';
        {_, Cont1}           -> Cont1();
        '$end_of_table'      -> '$end_of_table';
        _                    -> Cont()
    end.

select(Alias, Tab, Ms) ->
    %% FIXME: rewrite for disc_copies
    case select(Alias, Tab, Ms, infinity) of
        {Res, '$end_of_table'} ->
            Res;
        '$end_of_table' ->
            '$end_of_table'
    end.

select(Alias, Tab, Ms, Limit) when Limit==infinity; is_integer(Limit) ->
    %% FIXME: rewrite for disc_copies
    ok.
    %% {Ref, Type} = get_ref(Alias, Tab),
    %% do_select(Ref, Tab, Type, Ms, Limit).

%% FIXME: rewrite for disc_copies
slot(Alias, Tab, Pos) when is_integer(Pos), Pos >= 0 ->
    ok;
    %% {Ref, Type} = get_ref(Alias, Tab),
    %% First = fun(I) -> ?leveldb:iterator_move(I, <<"DATA_START">>) end,
    %% F = case Type of
    %%         bag -> fun(I) -> slot_iter_set(First(I), I, 0, Pos) end;
    %%         _   -> fun(I) -> slot_iter_set(First(I), I, 0, Pos) end
    %%     end,
    %% with_iterator(Ref, F);
slot(_, _, _) ->
    error(badarg).

%% %% Exactly which objects Mod:slot/2 is supposed to return is not defined,
%% %% so let's just use the same version for both set and bag. No one should
%% %% use this function anyway, as it is ridiculously inefficient.
%% slot_iter_set({ok, K, V}, _I, P, P) ->
%%     [setelement(2, decode_val(V), decode_key(K))];
%% slot_iter_set({ok, _, _}, I, P1, P) when P1 < P ->
%%     slot_iter_set(?leveldb:iterator_move(I, next), I, P1+1, P);
%% slot_iter_set(Res, _, _, _) when element(1, Res) =/= ok ->
%%     '$end_of_table'.

update_counter(Alias, Tab, C, Val) when is_integer(Val) ->
    %% FIXME: rewrite for disc_copies
    ok.
    %% case call(Alias, Tab, {update_counter, C, Val}) of
    %%     badarg ->
    %%         mnesia:abort(badarg);
    %%     Res ->
    %%         Res
    %% end.

%% %% server-side part
%% do_update_counter(C, Val, Ref) ->
%%     Enc = encode_key(C),
%%     case ?leveldb:get(Ref, Enc, [{fill_cache, true}]) of
%% 	{ok, EncVal} ->
%% 	    case decode_val(EncVal) of
%% 		{_, _, Old} = Rec when is_integer(Old) ->
%% 		    Res = Old+Val,
%% 		    ?leveldb:put(Ref, Enc,
%% 				 encode_val(
%% 				   setelement(3, Rec, Res)),
%% 				 []),
%% 		    Res;
%% 		_ ->
%% 		    badarg
%% 	    end;
%% 	_ ->
%% 	    badarg
%%     end.

%% %% PRIVATE

%% %% key+data iterator: iterator_move/2 returns {ok, EncKey, EncVal}
%% with_iterator(Ref, F) ->
%%     {ok, I} = ?leveldb:iterator(Ref, []),
%%     try F(I)
%%     after
%%         ?leveldb:iterator_close(I)
%%     end.

%% %% keys_only iterator: iterator_move/2 returns {ok, EncKey}
%% with_keys_only_iterator(Ref, F) ->
%%     {ok, I} = ?leveldb:iterator(Ref, [], keys_only),
%%     try F(I)
%%     after
%%         ?leveldb:iterator_close(I)
%%     end.


%% record and key validation

validate_key(_Alias, _Tab, RecName, Arity, Type, _Key) ->
    %% FIXME: rewrite for disc_copies
    {RecName, Arity, Type}.

validate_record(_Alias, _Tab, RecName, Arity, Type, _Obj) ->
    %% FIXME: rewrite for disc_copies
    {RecName, Arity, Type}.

%% file extension callbacks

%% Extensions for files that are permanent. Needs to be cleaned up
%% e.g. at deleting the schema.
real_suffixes() ->
    %% FIXME: rewrite for disc_copies
    [".extldb"].

%% Extensions for temporary files. Can be cleaned up when mnesia
%% cleans up other temporary files.
tmp_suffixes() ->
    %% FIXME: rewrite for disc_copies
    [].


%% ----------------------------------------------------------------------------
%% GEN SERVER CALLBACKS AND CALLS
%% ----------------------------------------------------------------------------

%% %% callback for starting processes under mnesia_ext_sup
%% start_proc(Alias, Tab, Type, LdbOpts) ->
%%     ok.
    %% ProcName = proc_name(Alias, Tab),
    %% gen_server:start_link({local, ProcName}, ?MODULE,
    %%                       {Alias, Tab, Type, LdbOpts}, []).

%% init({Alias, Tab, Type, LdbOpts}) ->
%%     process_flag(trap_exit, true),
%%     ok.
    %% {ok, Ref, Ets} = do_load_table(Tab, LdbOpts),
    %% St = #st{ ets = Ets
    %%         , ref = Ref
    %%         , alias = Alias
    %%         , tab = Tab
    %%         , type = Type
    %%         , size_warnings = 0
    %%         , maintain_size = should_maintain_size(Tab)
    %%         },
    %% {ok, recover_size_info(St)}.

%% do_load_table(Tab, LdbOpts) ->
%%     MPd = data_mountpoint(Tab),
%%     ?dbg("** Mountpoint: ~p~n ~s~n", [MPd, os:cmd("ls " ++ MPd)]),
%%     Ets = ets:new(tab_name(icache,Tab), [set, protected, named_table]),
%%     {ok, Ref} = open_leveldb(MPd, LdbOpts),
%%     leveldb_to_ets(Ref, Ets),
%%     {ok, Ref, Ets}.

%% handle_call({load, Alias, Tab, Type, LdbOpts}, _From,
%%             #st{type = Type, alias = Alias, tab = Tab} = St) ->
%%     {ok, Ref, Ets} = do_load_table(Tab, LdbOpts),
%%     {reply, ok, St#st{ref = Ref, ets = Ets}};
%% handle_call(get_ref, _From, #st{ref = Ref, type = Type} = St) ->
%%     {reply, {Ref, Type}, St};
%% handle_call({write_info, Key, Value}, _From, #st{} = St) ->
%%     _ = write_info_(Key, Value, St),
%%     {reply, ok, St};
%% handle_call({update_counter, C, Incr}, _From, #st{ref = Ref} = St) ->
%%     {reply, do_update_counter(C, Incr, Ref), St};
%% handle_call({insert, Key, Val}, _From, St) ->
%%     do_insert(Key, Val, St),
%%     {reply, ok, St};
%% handle_call({delete, Key}, _From, St) ->
%%     do_delete(Key, St),
%%     {reply, ok, St};
%% handle_call(clear_table, _From, #st{ets = Ets, tab = Tab, ref = Ref} = St) ->
%%     MPd = data_mountpoint(Tab),
%%     ?dbg("Attempting clear_table(~p)~n", [Tab]),
%%     _ = eleveldb_close(Ref),
%%     {ok, NewRef} = destroy_recreate(MPd, leveldb_open_opts(Tab)),
%%     ets:delete_all_objects(Ets),
%%     leveldb_to_ets(NewRef, Ets),
%%     {reply, ok, St#st{ref = NewRef}};
%% handle_call({match_delete, Pat}, _From, #st{} = St) ->
%%     Res = do_match_delete(Pat, St),
%%     {reply, Res, St};
%% handle_call(close_table, _From, #st{ref = Ref, ets = Ets} = St) ->
%%     _ = eleveldb_close(Ref),
%%     ets:delete(Ets),
%%     {reply, ok, St#st{ref = undefined}};
%% handle_call(delete_table, _From, #st{tab = T, ref = Ref, ets = Ets} = St) ->
%%     _ = (catch eleveldb_close(Ref)),
%%     _ = (catch ets:delete(Ets)),
%%     do_delete_table(T, data_mountpoint(T)),
%%     {stop, normal, ok, St#st{ref = undefined}}.

%% handle_cast(size_warning, #st{tab = T, size_warnings = W} = St) when W < 10 ->
%%     mnesia_lib:warning("large size retrieved from table: ~p~n", [T]),
%%     if W =:= 9 ->
%%             OneHrMs = 60 * 60 * 1000,
%%             erlang:send_after(OneHrMs, self(), unmute_size_warnings);
%%        true ->
%%             ok
%%     end,
%%     {noreply, St#st{size_warnings = W + 1}};
%% handle_cast(size_warning, #st{size_warnings = W} = St) when W >= 10 ->
%%     {noreply, St#st{size_warnings = W + 1}};
%% handle_cast(_, St) ->
%%     {noreply, St}.

%% handle_info(unmute_size_warnings, #st{tab = T, size_warnings = W} = St) ->
%%     C = W - 10,
%%     if C > 0 ->
%%             mnesia_lib:warning("warnings suppressed~ntable: ~p, count: ~p~n",
%%                                [T, C]);
%%        true ->
%%             ok
%%     end,
%%     {noreply, St#st{size_warnings = 0}};
%% handle_info({'EXIT', _, _} = _EXIT, St) ->
%%     ?dbg("leveldb owner received ~p~n", [_EXIT]),
%%     {noreply, St};
%% handle_info(_, St) ->
%%     {noreply, St}.

%% code_change(_FromVsn, St, _Extra) ->
%%     {ok, St}.

%% terminate(_Reason, #st{ref = Ref}) ->
%%     if Ref =/= undefined ->
%% 	    ?leveldb:close(Ref);
%%        true -> ok
%%     end,
%%     ok.



%% ----------------------------------------------------------------------------
%% DEBUGGING
%% ----------------------------------------------------------------------------

-ifndef(MNESIA_ELEVELDB_NO_DBG).
%% pp_stack() ->
%%     Trace = try throw(true)
%%             catch
%%                 _:_ ->
%%                     case erlang:get_stacktrace() of
%%                         [_|T] -> T;
%%                         [] -> []
%%                     end
%%             end,
%%     pp_calls(10, Trace).

%% pp_calls(I, [{M,F,A,Pos} | T]) ->
%%     Spc = lists:duplicate(I, $\s),
%%     Pp = fun(Mx,Fx,Ax,Px) ->
%%                 [atom_to_list(Mx),":",atom_to_list(Fx),"/",integer_to_list(Ax),
%%                  pp_pos(Px)]
%%         end,
%%     [Pp(M,F,A,Pos)|[["\n",Spc,Pp(M1,F1,A1,P1)] || {M1,F1,A1,P1} <- T]].

%% pp_pos([]) -> "";
%% pp_pos(L) when is_integer(L) ->
%%     [" (", integer_to_list(L), ")"];
%% pp_pos([{file,_},{line,L}]) ->
%%     [" (", integer_to_list(L), ")"].
-endif.
