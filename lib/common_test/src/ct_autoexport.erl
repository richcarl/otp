%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2006-2010 Richard Carlsson
%% @private
%% @see ct
%% @doc Parse transform for automatic exporting of test functions.

-module(ct_autoexport).

-export([parse_transform/2]).

-ignore_xref([parse_transform/2]).

parse_transform(Forms, _Options) ->
    {S, T, Z} = lists:foldl(fun form/2,
                            {sets:new(), sets:new(), sets:new()},
                            Forms),
    Exports = lists:sort(sets:to_list(lists:foldl(fun sets:add_element/2,
                                                  sets:union(S,T),
                                                  [{all,0},{all,1},
                                                   {all_tests,0}]))),
    Tests = [F || {F,1} <- lists:sort(sets:to_list(T))],
    HaveInfo = ordsets:intersection(lists:sort([F || F <- sets:to_list(Z)]),
                                    Tests),
    Exports1 = ordsets:union(Exports, [{F,0} || F <- HaveInfo]),
    try rewrite(Forms, Tests, Exports1)
    catch
        no_suite ->
            Forms  % only take effect on _SUITE modules
    end.

%% TODO: should detect groups and list in all/0, along with tests not in a group

%% Detect 'special functions', test functions, and zero-arity functions
form({function, _L, 'all'=Name, 0=Arity, _Cs}, {S, T, Z}) ->
    {sets:add_element({Name,Arity},S), T, Z};
form({function, _L, 'all'=Name, 1=Arity, _Cs}, {S, T, Z}) ->
    {sets:add_element({Name,Arity},S), T, Z};
form({function, _L, 'suite'=Name, 0=Arity, _Cs}, {S, T, Z}) ->
    {sets:add_element({Name,Arity},S), T, Z};
form({function, _L, 'groups'=Name, 0=Arity, _Cs}, {S, T, Z}) ->
    {sets:add_element({Name,Arity},S), T, Z};
form({function, _L, 'init_per_suite'=Name, 1=Arity, _Cs}, {S, T, Z}) ->
    {sets:add_element({Name,Arity},S), T, Z};
form({function, _L, 'end_per_suite'=Name, 1=Arity, _Cs}, {S, T, Z}) ->
    {sets:add_element({Name,Arity},S), T, Z};
form({function, _L, 'init_per_testcase'=Name, 2=Arity, _Cs}, {S, T, Z}) ->
    {sets:add_element({Name,Arity},S), T, Z};
form({function, _L, 'end_per_testcase'=Name, 2=Arity, _Cs}, {S, T, Z}) ->
    {sets:add_element({Name,Arity},S), T, Z};
form({function, _L, Name, 0, _Cs}, {S, T, Z}) ->
    {S, T, sets:add_element(Name,Z)};  % only keep name for 0-arity functions
form({function, _L, Name, 1=Arity, Cs}, {S, T, Z}=A) ->
    case looks_like_test_case(Cs) of
        true ->
	    {S, sets:add_element({Name,Arity},T), Z};
	false ->
            A
    end;
form(_, A) ->
    A.

%% note that we have already checked for all/1 above
looks_like_test_case([{clause, _L, [{atom, _, 'suite'}], _G, _B} | _Cs]) ->
    true;
looks_like_test_case([{clause, _L, [{atom, _, 'doc'}], _G, _B} | _Cs]) ->
    true;
looks_like_test_case([_ | Cs]) ->
    looks_like_test_case(Cs);
looks_like_test_case([]) ->
    false.

rewrite([{attribute,_,module,{_Name,_Ps}}=_M | _Fs], _Tests, _Exports) ->
    %% module_decl(M, Fs, Name, Tests, Exports);  % parameterized module
    throw(no_suite);  % parameterized modules can't be test suites, currently
rewrite([{attribute,_,module,Name}=M | Fs], Tests, Exports) ->
    module_decl(M, Fs, Name, Tests, Exports);
rewrite([F | Fs], Tests, Exports) ->
    [F | rewrite(Fs, Tests, Exports)];
rewrite([], _Tests, _Exports) ->
    throw(no_suite).  % no module declaration found

module_decl(M, Fs, Module, Tests, Exports) ->
    case lists:reverse(atom_to_list(Module)) of
        "ETIUS_" ++ _ ->
            Fs1 = rewrite(Fs, [], Module, Tests, Exports, undefined, undefined),
            [M, {attribute,0,export,Exports} | lists:reverse(Fs1)];
        _ ->
            throw(no_suite)
    end.

rewrite([{function,_,all,0,_}=F | Fs], As, Module, Tests, Exports,
        undefined, All1) ->
    rewrite(Fs, As, Module, Tests, Exports, F, All1);
rewrite([{function,_,all,1,_}=F | Fs], As, Module, Tests, Exports,
        All0, undefined) ->
    rewrite(Fs, As, Module, Tests, Exports, All0, F);
rewrite([{attribute,L,export,Xs} | Fs], As, Module, Tests, Exports,
        All0, All1) ->
    %% avoid warnings for multiple exports due to autoexports
    Xs1 = ordsets:subtract(lists:sort(Xs), Exports),
    rewrite(Fs, [{attribute,L,export,Xs1} | As],
            Module, Tests, Exports, All0, All1);
rewrite([F | Fs], As, Module, Tests, Exports, All0, All1) ->
    rewrite(Fs, [F | As], Module, Tests, Exports, All0, All1);
rewrite([], As0, Module, Tests, _Exports, All0, All1) ->
    %% generate exported all_tests() function returning a list of all the
    %% test functions in the module, that can be called by code in all() or
    %% all(suite), in particular if the user defines these functions
    As1 = [{function,0,all_tests,0,
            [{clause,0,[],[],
              [lists:foldl(fun (F, L) ->
                                   {cons,0, {atom,0,F}, L}
                           end,
                           {nil,0},
                           Tests)
              ]
             }]}
           | As0],
    As2 = case All0 of
              undefined ->
                  %% if it doesn't already exist, generate an exported all/0
                  %% function that calls all(suite)
                  [{function,0,all,0,
                    [{clause,0,[],[],
                      [{call,0,{atom,0,all},[{atom,0,'suite'}]}]}]}
                   | As1];
              _ ->
                  [All0 | As1]  % keep user-defined all/0 function as is
          end,
    case All1 of
        undefined ->
            %% if it doesn't already exist, generate an exported all/1
            %% function whose 'suite' clause calls all_tests() and whose
            %% 'doc' clause returns a reasonably informative string
            [{function,0,all,1,
              [doc_clause(Module),
               suite_clause()]}
             | As2];
        {function,L1,all,1,Cs} ->
            %% if all/1 already exists, ensure that it has a 'suite' clause
            %% as well as a 'doc' clause
            {Suite, Doc} = check_all1(Cs, false, false),
            [{function,L1,all,1,
              (if Doc -> [];
                  true -> [doc_clause(Module)]
               end
               ++
               if Suite -> [];
                  true -> [suite_clause()]
               end
               ++ Cs)}
             | As2]
    end.

doc_clause(Module) ->
    Text = lists:flatten(io_lib:format("test suite module '~s'", [Module])),
    {clause,0,[{atom,0,'doc'}],[], [{string,0,Text}]}.

suite_clause() ->
    {clause,0,[{atom,0,'suite'}],[],[{call,0,{atom,0,all_tests},[]}]}.

%% If the user-defined all/1 function has no 'all(suite)' clause, generate
%% one (as the first clause) that calls all_tests/0.
check_all1([{clause,_,[{atom,_,'suite'}],_,_Body} | Cs], _Suite, Doc) ->
    check_all1(Cs, true, Doc);
check_all1([{clause,_,[{atom,_,'doc'}],_,_Body} | Cs], Suite, _Doc) ->
    check_all1(Cs, Suite, true);
check_all1([_ | Cs], Suite, Doc) ->
    check_all1(Cs, Suite, Doc);
check_all1([], Suite, Doc) ->
    {Suite, Doc}.
