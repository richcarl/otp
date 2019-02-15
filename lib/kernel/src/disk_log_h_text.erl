%%% @doc Functions for formatting events
%%%
%%% This module contains code for formatting events from disk_log_h

-module(disk_log_h_text).

%% Log formatting functions.
-export([ form_func/1
        , form_no_warning/1
        , form_no_info/1
        , form_no_progress/1
        , form_application/1
        , form_all/1
        ]).

%% Used to pass log formatting functions to disk_log_h.
form_func(error)       -> fun disk_log_h_text:form_no_warning/1;
form_func(warning)     -> fun disk_log_h_text:form_no_info/1;
form_func(info)        -> fun disk_log_h_text:form_no_progress/1;
form_func(application) -> fun disk_log_h_text:form_application/1;
form_func(all)         -> fun disk_log_h_text:form_all/1.

%% no warnings or info messages (includes progress)
form_no_warning({warning_report, _, _}) ->
    false;
form_no_warning({warning_msg, _, _}) ->
    false;
form_no_warning(Event) ->
    form_no_info(Event).

%% no info messages (includes progress)
form_no_info({info_report, _, _}) ->
    false;
form_no_info({info_msg, _, _}) ->
    false;
form_no_info({info, _, _}) ->
    false;
form_no_info(Event) ->
    form_all(Event).

%% all info messages except progress
form_no_progress({info_report, _, {_, progress, _}}) ->
    false;
form_no_progress(Event) ->
    form_all(Event).

%% all info messages including application progress, but no other progress
form_application({info_report, _, {_, progress,[{application,_}|_]}}=Event) ->
    form_all(Event);
form_application(Event) ->
    form_no_progress(Event).

form_all({_Type, GL, _Msg}) when node(GL) /= node() ->
    false;
form_all(Event) ->
    Str = try format_event(Event)
          catch
              Class:Reason ->
                  Trace = erlang:get_stacktrace(),
                  [mk_hdr("BAD LOG EVENT", undefined, undefined),
                   io_lib:format("~P~n~P~n",
                                 [nobin(Event), 30,
                                  nobin({Class, Reason, Trace}), 30])]
          end,
    list_to_binary([Str, "\n"]).

format_event(Event) ->
    case Event of
        {error_report, _GL, {Pid, Type, Report}} ->
            [mk_hdr("ERROR REPORT", Type, Pid),
             io_lib:format("~p\n", [nobin(Report)])];
        {error, _GL, {Pid, Format, Args}} ->
            [mk_hdr("ERROR", undefined, Pid),
             io_lib:format(Format, nobin(Args))];
        {warning_report, _GL, {Pid, Type, Report}} ->
            [mk_hdr("WARNING REPORT", Type, Pid),
             io_lib:format("~p\n", [nobin(Report)])];
        {warning_msg, _GL, {Pid, Format, Args}} ->
            [mk_hdr("WARNING MSG", undefined, Pid),
             io_lib:format(Format, nobin(Args))];
        {info_report, _GL, {Pid, Type, Report}} ->
            [mk_hdr("INFO REPORT", Type, Pid),
             io_lib:format("~p\n", [nobin(Report)])];
        {info_msg, _GL, {Pid, Format, Args}} ->
            [mk_hdr("INFO MSG", undefined, Pid),
             io_lib:format(Format, nobin(Args))];
        {info, _GL, {Pid, Term, _Empty}} ->
            [mk_hdr("INFO", undefined, Pid),
             io_lib:format("~p\n", [nobin(Term)])];
        {emulator, _GL, EStr} ->
            [mk_hdr("EMULATOR", undefined, undefined),
             nobin(EStr)];
        _ ->
            [mk_hdr("UNKNOWN", undefined, undefined),
             io_lib:format("~P\n", [Event, 50])]
    end.

mk_hdr(HStr, Type, Who) ->
    VTDiff = ktime:virtual_time_diff(),
    VirtualTime =
	if VTDiff /= 0 ->
		" [" ++ t2s(datetime:local_time()) ++ "]";
	   true -> ""
	end,
    ["== ", t2s(erlang:localtime()), VirtualTime, " == ", HStr, " - ",
     pstr(Type), " ", pstr(Who), "\n"].

pstr(undefined) -> "";
pstr(T) -> io_lib:format("~p", [T]).

t2s({{Year,Month,Day},{Hour,Minute,Second}}) ->
    io_lib:format("~w-~s-~w::~2..0w:~2..0w:~2..0w",
		  [Day,m2s(Month),Year,Hour,Minute,Second]).

m2s(1) -> "Jan";
m2s(2) -> "Feb";
m2s(3) -> "Mar";
m2s(4) -> "Apr";
m2s(5) -> "May";
m2s(6) -> "Jun";
m2s(7) -> "Jul";
m2s(8) -> "Aug";
m2s(9) -> "Sep";
m2s(10) -> "Oct";
m2s(11) -> "Nov";
m2s(12) -> "Dec".


nobin(B) when is_binary(B), size(B) > 32 ->
    <<ShortBin:32/binary, _/binary>> = B,
    lists:flatten(io_lib:format("~p(~w)", [ShortBin, size(B)]));
nobin(L) when is_list(L) ->
    map2(fun(X) -> nobin(X) end, L);
nobin(T) when is_tuple(T) ->
    list_to_tuple(nobin(tuple_to_list(T)));
nobin(X) ->
    X.

%% handles non-proper lists
map2(F, [H | T]) ->
    [F(H) | map2(F, T)];
map2(_F, []) ->
    [];
map2(_F, T) ->
    T.
