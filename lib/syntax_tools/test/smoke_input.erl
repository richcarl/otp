%% =====================================================================
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright (C) 2020, Richard Carlsson
%% @doc

-module('smoke_input').

-include_lib("eunit/include/eunit.hrl").

-export([foo/0, foo/1, foo/2]).

%% function with zero parameters
foo() ->
    foo([]).  % single expression body, function call with 1 arg

%% multi-clause function
%% function with one parameter
foo(Xs) when is_list(Xs) -> % simple guard
    foo(Xs, []);
foo(X) when (element(2, X) + 1) > (element(3, X) - 1) -> % guard with subexpressions
    foo(X, []);
foo(X) when is_integer(X), X >= $A, X =< $Z ->  % conjunction guard
    foo([X], []);
foo(X) when is_pid(X) ; is_port(X) ; is_reference(X) ->  % disjunction guard
    foo([X], []);
foo(X) when is_atom(X), X =/= true, X =/= false
          ; is_float(X), X > 0, X < 1
          ; is_binary(X), X =/= <<>> ->  % disjunction of conjunctions
    foo([X], []);
foo(X) ->
    foo(X, []).  % function call with 2 args


num() ->
    %% constants
    [
     %% strings and characters
     "",     % empty string
     "abc",  % string
     "abc"
     "def"
     "ghi",  % multi-segment string
     $A,     % character
     $\n,    % escape character

     %% integers
     +4711, % signed positive
     %4_711, % with underscore separator
     4711,  % positive
     0,     % zero 
     -17,   % negative

     %% floats
     1.0e+80,   % positive with signed positive exponential
     6.023e23,  % positive with positive exponential
     +3.14159,  % signed positive
     2.71828,   % positive
     6.626e-34, % positive with negative exponential
     0.0,       % zero
     -273.15,   % negative

     %% atoms
     true,
     false,
     undefined,
     foo_bar,
     'Hello',
     '!@#$',

     %% binaries
     <<>>,
     <<"abcde">>,
     <<"abcde"/utf8>>,
     <<"abcde","fghij","klmno">>

    ].

%% record definition
-record(r, {a, b=42, c}).

%% typed record definition
-record(t, {x :: atom(),
            y :: integer(),
            f :: fun ((atom()) -> #t{})
           }).

t(X) ->
    t(X, 0).  %% function call with 2 parameters

t(X, Y) ->
    #t{x = X, y = Y, f = fun t/1}.  % record

%% function with multiple parameters
foo(X, Ys) ->
    %% multiple expression body

    %% lists
    [],
    [1|[]],
    [1|[2|[]]],
    [1,2,3,4],
    [1,2,3|[4]],

    %% tuples
    {},
    {1},
    {3,2,1,0},
    {0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,
     0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,
     0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,
     0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9},

    %% records
    #r{},                                % record instance, only defaults
    #r{a = X, c = fun t/1},              % record instance, some defaults
    Rec = #r{a = X, b=42, c = fun t/1},  % record instance, all fields given

    Rec#r{},                              % record update, no fields specified
    Rec#r{b = 17},                        % record update, no fields specified
    Rec#r{a = foo, b=11, c = undefined},  % record update, all fields given

    Rec#r.b,    % record field access
    #r.c,       % record field index

    %% maps
    #{},                                    % empty map instance
    #{a => 1},                              % map instance with one field
    Map = #{a => X+1, b => X+2, c => X+3},  % map instance with multiple fields

    Map#{b => 42, d => undefined },   % map update with possibly new keys
    Map#{b := 42, c := undefined },   % map update with only existing keys

    %% operators
    -4711,    % unary operator
    X + 4711, % binary operator

    %% blocks
    begin X + 1 end,   % single expression block

    begin  % multi-expression block
        X + 1,
        X - 1,
        (X + 1) * 2  % parenthesized subexpression for precedence
    end,

    %% function calls
    num(),       % local call, zero arguments
    t(foo),      % local call, one argument
    t(bar, 42),  % local call, multiple arguments
    erlang:halt(),                % remote call, zero arguments
    lists:reverse([1,2,3]),       % remote call, one argument
    lists:reverse([1,2], [3,4]),  % remote call, multiple arguments

    %% funs
    fun t/1,                     % local implicit fun
    fun smoke_input:t/1,         % remote implicit fun
    fun () -> ok end,            % fun expression, zero parameters
    fun (P) -> P end,         % fun expression, one parameters
    fun (P1,P2,P3) -> P end,  % fun expression, multiple parameters
    fun FunFun(P1) -> FunFun(P1+1) end,  % named fun expression

    %% try/catch
    catch (X+1),  % old style catch

    try X+1 after X+2 end,  %% try-after

    try X+1  %% try-catch
    catch
        foo -> foo;                             % implicit throw class
        error:foo_bar=Term -> {bar, Term};      % explicit class, no trace
        error:Term:Trace -> {baz, Term, Trace}  % explicit class and trace
    end,

    try X+1 of  %% try-of-catch, single clause
        Success -> ok
    catch
        foo -> bar
    end,

    try X+1 of  %% try-of-catch, multi-clause
        {one, A1} -> {ok, A1};
        {two, A2} -> {ok, A2};
        {three, A3} -> {ok, A3}
    catch
        foo -> bar;
        bar -> baz
    end,

    try X+1 of  %% try-of-after
        _ -> ok
    after
        X+2
    end,

    try X+1 of  %% try-of-catch-after
        _ -> ok
    catch
        foo -> bar;
        error:baz -> baz
    after
        X+2
    end,

    %% match, if, and case expressions
    {_,{D,E}} = {B,C} = A = X,  % chained pattern match

    if X =:= 0 -> ok end,  % single-clause if

    if length(X) > 0 -> yes;  % multi-clause if
       X > 0 -> yes;
       true -> no
    end,

    case E of  % single-clause case expression
        true -> 42, E+1, exit(bye)  %% multi-expression clause body
    end,

    Y = case X of  % multi-clause case expression
            %% constant patterns
            foo -> 0;  % atom pattern
            4711 -> 0;  % integer pattern
            3.14159 -> 0;  % float pattern

            %% list patterns
            [] -> 1;
            [H | T] -> 2;
            [H1, H2 | T] -> 3;
            [H1, H2, H3 | T] -> 4;
            [H1 | [H2 | [H3 | T]]] -> 5;

            %% append patterns
            "XYZ" ++ T -> 6;
            "" ++ T -> 7;

            %% tuple patterns
            {} -> 8;
            {foo,Bar} -> 9;
            {P,Q,R} -> 10;

            %% variable pattern
            V when is_boolean(V) -> ok;

            %% underscore pattern
            _ -> ok
        end,

    %% receive
    receive after 1000 -> ok end,  % receive-as-sleep

    receive Message -> {ok, Message} end,  % single clause receive

    receive  % multiple clause receive
        {a, M1} -> {ok, M1};
        {b, M2} -> {ok, M2};
        {c, M3} -> {ok, M3}
    end,

    receive  % receive with timeout
        {x, M} -> M
    after infinity ->
            X + 1,         % multiple expressions in timeout body
            X - 1,
            error(foobar)
    end,

    %% list comprehensions
    [N+1 || N <- lists:seq(0,99)],                              % one generator
    [{N+1,M-1} || N <- lists:seq(0,99), M <- lists:seq(1,10)],  % two generators
    [{N+1,M-1} || N <- lists:seq(0,99), N rem 2 =:= 0,
                  M <- lists:seq(1,10), M rem 2 =:= 1],  % generators and guards
    [N || <<N>> <= <<1,2,3,4,5,6>>, M rem 2 =:= 1],  % binary generator in list comp

    %% binary comprehensions
    << <<(N+1)>> || <<N>> <= <<1,2,3,4,5,6>> >>,  % one generator
    << <<(N+M)>> || <<N>> <= <<1,2,3,4,5,6>>,
                    <<M>> <= <<0,1,2,3,4,5>> >>,  % two generators

    << <<(N+M)>> || <<N>> <= <<1,2,3,4,5,6>>, N rem 2 =:= 0,
                    <<M>> <= <<0,1,2,3,4,5>>, M rem 2 =:= 1 >>,  % generators and guards

    << <<(N+M)>> || N <- lists:seq(0,99), N rem 2 =:= 0 >>,  % list generator

    ok.
