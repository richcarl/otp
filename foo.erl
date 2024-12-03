%% =====================================================================
%% @author Richard Carlsson <richard@cipater>
%% @copyright (C) 2024, Richard Carlsson
%% @doc

-module(foo).

-include_lib("eunit/include/eunit.hrl").

-export([f/1]).

f(X) ->
    case X of
        {a, Y} or {b, Y} or {c, Y} ->
            {yes, Y};
        _ -> no
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

    %% [Y || {a,Y} or {b,Y} <- X].  %% todo: single clause to multiple

    %% {a,Y} or {b,y} = X.  %% todo: single clause to multiple (maybe don't allow in =)
