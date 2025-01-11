%% TODO: lint must check for same new bindings in each (may have different use data)
%% TODO: handle multiple patterns in a clause (functions, funs), not just case/receive
%% TODO: handle single clauses becoming multiple, as in LC or P=E
%% TODO: avoid duplicating body by creating a function (unless trivial body)
%% TODO: handle multi-pattern clauses; maybe warn if too many combinations
%% TODO: handle nested alternatives, don't just split at top level
%% TODO: a group of ground terms `a or b or c` should become guard tests
%% =====================================================================
%% @author Richard Carlsson <richard@cipater>
%% @copyright (C) 2024, Richard Carlsson
%% @doc

-module(foo).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

f(X) ->
    %% plain case
    case X of
        {a, Y} or {b, Y} or {c, Y} ->
            {one, Y};
        {d, {x, Y}} or {e, Y=z} ->
            {two, Y};
        _ -> three
    end.

g() ->
    %% receive
    Pid = spawn(fun () ->
                        receive
                            {hello, From} or {stop, From} ->
                                From ! {ok, self()}
                        after 3000 -> ok
                        end
                end),
    Pid ! {stop, self()},
    receive
        {ok, Pid} -> done
        after 1000 -> exit(Pid, kill)
    end.

    %% self() ! {a, X},
    %% receive
    %%     (Z={a, Y})
    %%     or (Z={b, Y})
    %%     or (Z={c, Y}) ->
    %%         {yes, Y, Z}
    %% after 10000 -> {no, X}
    %% end.

    %% try throw(X)
    %% catch
    %%     {a, Y} or {b, Y} or {c, Y} ->
    %%         {yes, Y};
    %%     _ -> no
    %% end.

    %[Y || {a,Y} or {b,Y} <- X].  %% todo: single clause to multiple

    %% {a,Y} or {b,y} = X.  %% todo: single clause to multiple (maybe don't allow in =)
