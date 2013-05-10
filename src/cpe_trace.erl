%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%  CPE tracing facilities
%%% @end
%%% Created : 11 Nov 2012 by vlad <lib.aca55a@gmail.com>

-module(cpe_trace).

-export([report_event/4,
	 report_event/5]).

-include("host_internal.hrl").

%% API
-export([enable/1, enable/2, disable/0,
	 set_level/1, set_level/2]).

-export([start_ev/0, start_ev/1, clear_ev/1, stop_ev/1]).


%%====================================================================
%% API
%%====================================================================

%FIXME: need to sync ev_collector with lager traces

start_ev() ->
    start_ev([
	   {actors, [cwmp_cli, cpe_rpc, cpe_rpc_session, cpe_http, cpe_rpc_session, ibrowse] }
	  ]).

start_ev(ExtraOptions) ->
    Options =
        [{event_order, event_ts},
         {scale, 2},
         {max_actors, 10},
         {detail_level, 90},
         {trace_pattern, {cpe_trace, max}},
         {trace_global, true},
         {title, "CPE tracer"} | ExtraOptions],
    et_viewer:start(Options).

clear_ev(P) ->
    ColPid = et_viewer:get_collector_pid(P),
    ok = et_collector:clear_table(ColPid).

stop_ev(P) ->
    et_viewer:stop(P).



%%-----------------------------------------------------------------
%% enable(Level) -> void()
%% enable(Level, Service) -> void()
%%
%% Parameters:
%% Level -> max | min | integer()
%% Service -> atom()
%% File -> string()
%%
%% Description:
%% This function is used to start tracing at level Level and send
%% the result either to the file File, trace handler.
%%
%% Note that it starts a tracer server.
%% When Destination is the atom lager all (printable) inets trace
%% events will be written to log using 'info' severity.
%%
%%-----------------------------------------------------------------
enable(Level) ->
    enable(Level, all).

enable(Level, Service) ->
    ok = lager:info("start trace at ~s~n", [format_timestamp(now())]),
    Levels = lager:get_loglevels(),
    %% MaxLogLevel = lists:max(Levels),
    %% ?EXPECT(MaxLogLevel =:= 6 orelse MaxLogLevel =:= 7,
    %% 	    "Some of Lager handlers should be in 'debug', 'info'"),

    HandleSpec = {fun handle_trace/2, Service},
    case dbg:tracer(process, HandleSpec) of
	{ok, _} ->
	    {ok, _} = set_level(Level),
	    ok;
	Error ->
	    Error
    end.

%%-----------------------------------------------------------------
%% disable() -> void()
%%
%% Description:
%% This function is used to stop tracing.
%%-----------------------------------------------------------------
disable() ->
    %% This is to make handle_trace/2 close the output file (if the
    %% event gets there before dbg closes)
    cpe_trace:report_event(100, stop_trace, "stop trace", [stop_trace]),
    dbg:stop().

%%-----------------------------------------------------------------
%% set_level(Level) -> void()
%%
%% Parameters:
%% Level -> max | min | integer()
%%
%% Description:
%% This function is used to change the trace level when tracing has
%% already been started.
%%-----------------------------------------------------------------
set_level(Level) ->
    set_level(Level, all).

set_level(Level, Service) ->
    Pat = make_pattern(?MODULE, Service, Level),
    change_pattern(Pat).

make_pattern(Mod, Service, Level)
  when is_atom(Mod) andalso is_atom(Service) ->
    case Level of
        min ->
            {Mod, Service, []};
        max ->
            Head = ['$1', '_', '_', '_', '_'],
            Body = [],
            Cond = [],
            {Mod, Service, [{Head, Cond, Body}]};
        DetailLevel when is_integer(DetailLevel) ->
            Head = ['$1', '_', '_', '_', '_'],
            Body = [],
            Cond = [{ '=<', '$1', DetailLevel}],
            {Mod, Service, [{Head, Cond, Body}]};
        _ ->
            exit({bad_level, Level})
    end.

change_pattern({Mod, Service, Pattern})
  when is_atom(Mod) andalso is_atom(Service) ->
    MFA = {Mod, report_event, 5},
    case Pattern of
        [] ->
	    try
		{ok,_} = error_to_exit(ctp, dbg:ctp(MFA)),
		{ok,_} = error_to_exit(p,   dbg:p(all, clear))
	    catch
		exit:{Where, Reason} ->
		    {error, {Where, Reason}}
	    end;
        List when is_list(List) ->
	    try
		{ok,_} = error_to_exit(ctp, dbg:ctp(MFA)),
		{ok,_} = error_to_exit(tp,  dbg:tp(MFA, Pattern)),
		{ok,_} = error_to_exit(p,   dbg:p(all, [call, timestamp]))
	    catch
		exit:{Where, Reason} ->
		    {error, {Where, Reason}}
	    end
    end.

error_to_exit(_Where, {ok, _} = OK) ->
    OK;
error_to_exit(Where, {error, Reason}) ->
    exit({Where, Reason}).

%%-----------------------------------------------------------------
%% report_event(Severity, Label, Service, Content)
%%
%% Parameters:
%% Severity -> 0 =< integer() =< 100
%% Label -> string()
%% Service -> httpd | httpc | ftp | tftp
%% Content -> [{tag, term()}]
%%
%% Description:
%% This function is used to generate trace events, that is,
%% put trace on this function.
%%-----------------------------------------------------------------
report_event(DetailLevel, FromTo, Label, Contents) ->
    %% N.B External call
    ?MODULE:report_event(DetailLevel, FromTo, FromTo, Label, Contents).

report_event(_DetailLevel, _From, _To, _Label, _Contents) ->
    hopefully_traced.

%% ----------------------------------------------------------------------
%% handle_trace(Event, Fd) -> Verbosity
%%
%% Parameters:
%% Event -> The trace event
%% Fd -> lager | file_descriptor()
%%
%% Description:
%% This function is used to "receive" and pretty print the trace events.
%% Events are printed if:
%%   - Verbosity is max
%%   - Severity is =< Verbosity (e.g. Severity = 30, and Verbosity = 40)
%% Events are not printed if:
%%   - Verbosity is min
%%   - Severity is > Verbosity
%%-----------------------------------------------------------------

handle_trace({trace_ts, _Who, call,
              {?MODULE, report_event,
               [_Sev, stop_trace, stop_trace, "stop trace", [stop_trace]]},
              Timestamp},
             Filter) ->
    (catch lager:info("stop trace at ~s~n", [format_timestamp(Timestamp)])),
    Filter;
handle_trace({trace_ts, Who, call,
              {?MODULE, report_event,
               [Sev, Service, Service, Label, Content]}, Timestamp},
             Filter) ->
    (catch print_host_trace(Filter, Sev, Timestamp, Who, Label, Service, Content)),
    Filter;
handle_trace({trace_ts, Who, call,
              {?MODULE, report_event,
               [Sev, From, To, Label, Content]}, Timestamp},
             Filter) ->
    (catch print_host_message(Filter, Sev, Timestamp, Who, From, To, Label, Content)),
    Filter;
handle_trace(Event, Filter) ->
    (catch print_trace(Event, Filter)),
    Filter.

print_host_trace(Filter, Sev, Timestamp, Who, Label, Service, Content)
  when (Filter =:= all) ->
    do_print_host_trace(Sev, Timestamp, Who, Label, Service, Content);
print_host_trace(Filter, _Sev, _Timestamp, _Who, _Label, Service, _Content)
  when Filter =/= Service ->
    ok;
print_host_trace(_Filter, Sev, Timestamp, Who, Label, Service, Content) ->
    do_print_host_trace(Sev, Timestamp, Who, Label, Service, Content).

do_print_host_trace(Sev, Timestamp, Who, Label, Service, Content) ->
    Ts = format_timestamp(Timestamp),
    lager:info("[EVT ~p(~p) trace(~p) ~p] ~s"
	       "~n   Content: ~p"
	       "~n",
	       [Service, Who, Sev, Ts, Label, Content]).


print_host_message(Filter, Sev, Timestamp, Who, From, To, Label, Content)
  when (Filter =:= all) ->
    do_print_host_message(Sev, Timestamp, Who, From, To, Label, Content);
print_host_message(_Filter, Sev, Timestamp, Who, From, To, Label, Content) ->
    do_print_host_message(Sev, Timestamp, Who, From, To, Label, Content).


do_print_host_message(_Sev, Timestamp, Who, From, To, Label, Content) ->
    Ts = format_timestamp(Timestamp),
    lager:info("[MSG ~p(~p):~p -> ~p, ~p]"
	       "~n   Content: ~p"
	       "~n",
	       [Label, Who, From, To, Ts, Content]).

print_trace(Event, Filter) ->
    lager:info("TRC ~p [trace] ~p ~n", [Filter, Event]).


format_timestamp({_N1, _N2, N3} = Now) ->
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),
    lists:flatten(FormatDate).



%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("host_internal.hrl").

report_event_test_() ->
    { setup,
      fun () ->
	      ok = lager:start(),
	      lager:set_loglevel(lager_console_backend, info)
      end,
      fun (_O)->
	      ok
      end,
      [?_test(begin
		  enable(max, all)
		      , ?MODULE:report_event(50, ?MODULE, ?MODULE,  "Message", "args")
		      , ?MODULE:report_event(50, ?MODULE,  "Label", "Contents")
		      , cpe_trace:disable()
	      end)]}.


trace_macro_test_() ->
    { setup,
      fun () ->
	      ok = lager:start(),
	      lager:set_loglevel(lager_console_backend, info)
      end,
      fun (_O)->
	      ok
      end,
      [?_test(begin
		  cpe_trace:enable(max, all),
		  ?hostri("Important", "Don't forget your glasses"),
		  ?hostrv("Verbose", "You talk too much"),  
		  ?hostrd("Debug", "No bugs allowed"),
		  ?hostrt("Trace", "Catch me if you can"),
		  ?message(60, host, httpd, 'ORLY?', "I have it"),
		  cpe_trace:disable()
	      end)]}.




-endif.
