%% =====================================================================
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright (C) 2020, Richard Carlsson
%% @doc

-module(erl_syntax_tests).

-include_lib("eunit/include/eunit.hrl").

-export([]).
-define(input_file, "").
big_smoke_test() ->
    TestDir = ".", % case code:lib_dir(syntax_tools) of
                  %{error, _} -> ".";
                 % Dir -> Dir
              %end,
    {ok, Forms} = epp_dodger:parse_file(filename:join(TestDir, "smoke_input.erl")),
    Tree0 = erl_syntax:form_list(Forms),
    PP0 = erl_prettypr:format(Tree0),
    file:write_file("/tmp/pp0.erl", PP0),
    Tree1 = erl_syntax_lib:map(fun (T) -> erl_syntax:add_ann(x, T) end, Tree0),
    PP1 = erl_prettypr:format(Tree1),
    file:write_file("/tmp/pp1.erl", PP1),
    "" = os:cmd("diff /tmp/pp0.erl /tmp/pp1.erl"),
    Abst = erl_syntax:revert_forms(Tree1),
    PP2 = erl_prettypr:format(erl_syntax:form_list(Abst)),
    file:write_file("/tmp/pp2.erl", PP2),
    Diff = os:cmd("diff /tmp/pp1.erl /tmp/pp2.erl"),
    io:put_chars(Diff),
    "" = Diff,
    ok.
