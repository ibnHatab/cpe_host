%%% File    : cpr_host.erl
%%% Description :
%%%   Manual startup of the HOST application

-module(cpe_host).

-compile({parse_transform, lager_transform}).


-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, Reason} ->
	    erlang:error({app_start_failed, App, Reason}),
            {error, Reason}
    end.

%% @spec start() -> ok
%% @doc Start the HOST for testing.
start() ->
    ok = ensure_started(sasl),
    ok = lager:start(),
    ok = ensure_started(gproc),
    ok = application:start(cpe_host).

%% @spec stop() -> ok
%% @doc Stop the acs_core server.
stop() ->
    ok = application:stop(cpe_host),
    ok = application:stop(gproc),
    ok = application:stop(lager),
    ok = application:stop(sasl).

