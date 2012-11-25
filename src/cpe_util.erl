%%% File    : cpe_util.erl
%%% Description : Miscellaneous utilities for protocol processing.


-module(cpe_util).

-include("host_internal.hrl").

-export([is_arch/1, sleep/1]).


%% Returns a forced-lowercase architecture for this node
-spec get_arch () -> string().
get_arch () -> string:to_lower(erlang:system_info(system_architecture)).

%% Checks if this node is of a given architecture
-spec is_arch (atom()) -> boolean().
is_arch (linux) -> string:str(get_arch(),"linux") > 0;
is_arch (darwin) -> string:str(get_arch(),"darwin") > 0;
is_arch (sunos) -> string:str(get_arch(),"sunos") > 0;
is_arch (osx) -> is_arch(darwin);
is_arch (solaris) -> is_arch(sunos);
is_arch (Arch) -> throw({unsupported_architecture,Arch}).


sleep(T) ->
    ?DBG({sleep, T}),
    receive after T -> ok end.



%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_arch_test() ->
    ?assert(is_arch (linux)).

-endif.
