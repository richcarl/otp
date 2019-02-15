%%%----------------------------------------------------------------------
%%% File    : disk_log_h_sup.erl
%%% Author  : Magnus Fr|berg <magnus@bluetail.com>
%%% Purpose : Implements a wrapping ascii error_logger handler.  Uses
%%%           disk_log_h as the handler.  Almost like the sasl error logger,
%%%           but formats the logs to ascii instead of binary.
%%%
%%%           Use in supervisor as e.g.
%%%             {logger, {disk_log_h_sup, start_link, []},
%%%              permanent, 2000, worker, [logger]},
%%%
%%%           Initiate/deactivate system logging.
%%%           Owns the error log.
%%% Created : 13 Apr 1999 by Magnus Fr|berg <magnus@bluetail.com>
%%% Modified: 26 May 1999 by Martin Bjorklund <mbj@bluetail.com>
%%% Modified: 04 Dec 2000 by Martin Bjorklund <mbj@bluetail.com>
%%% Modified: 13 Nov 2003 by Martin Bjorklund <mbj@bluetail.com>
%%%           Cleanup for jungerl.
%%%
%%% Modified: 10 Jan 2019 Renamed to disk_log_h_sup to be compatible
%%% with OTP 21.
%%% ----------------------------------------------------------------------
%%%
%%% @private
-module(disk_log_h_sup).

-behaviour(gen_server).

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

%% Used as handler id, see e.g. gen_event:add_handler/3.
-define(LOGGER, { disk_log_h     %% handler callback module
                , disk_log_h_sup %% handler id, can be any term
                }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%% Starts a gen_event supervisor process for the disk_log_h handler, so that
%% the handler is restarted by the supervisor.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------
init([]) ->
    %% disabled if error_logger_mf_file is undefined or set to 'false'
    case get_mf_file() of
        false -> ignore;
        _ ->
            process_flag(trap_exit, true),
            ok = add_error_logger_mf(),
            error_logger:info_msg("Logging via disk_log_h enabled~n"),
            {Name, Vsn} = init:script_id(),
            error_logger:info_msg("Boot script ID: ~s-~s~n", [Name, Vsn]),
            start_tell_started(),
            {ok, []}
    end.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_call(_Req, _, S) ->
    {reply, unknown_request, S}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_cast(_, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_info({gen_event_EXIT, ?LOGGER, Reason}, S) ->
    ok = add_error_logger_mf(),
    error_logger:info_msg("Restarted disk_log_h in error_logger.~n"
                          "Crashed due to ~p~n", [Reason]),
    {noreply, S};

handle_info(_, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
terminate(_Reason, _S) ->
    delete_error_logger_mf(),
    ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%% check when the entire system is up and running
start_tell_started() ->
    spawn(fun tell_started/0).

tell_started() ->
    case init:get_status() of
	{started, started} ->
            error_logger:info_msg("Erlang/OTP boot sequence finished; "
                                  "system is started~n", []);
	_ ->
	    timer:sleep(250),
	    tell_started()
    end.

%% --------------------------------------------------------------
%% We have the disk_log_h instead of sasl as we don't want progress
%% reports to appear in the log.
%% --------------------------------------------------------------
add_error_logger_mf() ->
    Type = get_error_logger_mf_type(),
    Mf = get_error_logger_mf(),
    ok = add_error_logger_mf(Mf, Type).

add_error_logger_mf(undefined, _) -> ok;
add_error_logger_mf({File, MaxB, MaxF}, Type) ->
    case nolog() of
	false ->
            {_, Name} = ?LOGGER,
	    Opts = [{name, Name},
		    {file, File},
		    {type, wrap},
		    {format, external},
                    {force_size, true},
		    {size, {MaxB, MaxF}}],
            FormatFun = disk_log_h_text:form_func(Type),
            Args = disk_log_h:init(FormatFun, Opts),
            gen_event:add_sup_handler(error_logger, ?LOGGER, Args);
	true ->
	    ok
    end.

delete_error_logger_mf() ->
    gen_event:delete_handler(error_logger, ?LOGGER, stop).

get_env(Key) ->
    application:get_env(?APPLICATION, Key).

nolog() ->
    case get_env(nolog) of
	{ok, true} ->
	    true;
	_ ->
	    false
    end.

get_error_logger_mf_type() ->
    case get_env(errlog_type) of
	{ok, error} -> error;
	{ok, info} -> info;
	{ok, warning} -> warning;
	{ok, application} -> application;
	{ok, all} -> all;
	{ok, Bad} -> exit({bad_config, {errlog_type, Bad}});
	_ -> all
    end.

get_error_logger_mf() ->
    case catch get_mf() of
	{'EXIT', Reason} ->
	    exit(Reason);
	Mf ->
	    Mf
    end.

get_mf() ->
    File = get_mf_file(),
    MaxB = get_mf_maxb(),
    MaxF = get_mf_maxf(),
    {File, MaxB, MaxF}.

get_mf_file() ->
    case get_env(error_logger_mf_file) of
	{ok, false} -> false;
	{ok, File} when is_list(File) -> File;
	undefined -> false;
	{ok, Bad} -> exit({bad_config, {error_logger_mf_file, Bad}})
    end.

get_mf_maxb() ->
    case get_env(error_logger_mf_maxbytes) of
	{ok, MaxB} when is_integer(MaxB) -> MaxB;
	undefined -> 128000;
	{ok, Bad} -> exit({bad_config, {error_logger_mf_maxbytes, Bad}})
    end.

get_mf_maxf() ->
    case get_env(error_logger_mf_maxfiles) of
	{ok, MaxF} when is_integer(MaxF), MaxF > 0, MaxF < 256 -> MaxF;
	undefined -> 8;
	{ok, Bad} -> exit({bad_config, {error_logger_mf_maxfiles, Bad}})
    end.
