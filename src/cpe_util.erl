%%% File    : cpe_util.erl
%%% Description : Miscellaneous utilities for protocol processing.


-module(cpe_util).

-include("host_internal.hrl").

-export([is_arch/1, sleep/1, validate_options/2]).


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

%% This function extracts and validates the connect options from opption list.
-spec validate_options(list(Option),  list(Validator)) -> list(Option) when
      Option    :: {Key :: atom(),
		    Valie :: term()},
      Validator :: {Key :: atom(),
		    Validate :: fun((term()) -> true | false),
		    Mandatory :: true | false,
		    Default :: term()}.
validate_options(Options, ValidOptions) ->
    
    %% Validate in Options against validation template
    CheckedUpOptions =
	lists:foldl(fun({Key, Value}, AccOption) ->
			    case lists:keysearch(Key, 1, ValidOptions) of
				{value, {Key, Validate, _, Default}} ->
				    case (catch Validate(Value)) of
					true ->
					    [{Key, Value} | AccOption];
					_ ->
					    ?hostrt("validate_options -> check - reject",
						    [{default, Default}]),
					    throw({error, {Key, Value}})
				    end;
				false ->
				    ?hostrt("validate_options -> unknown - ignore",
					    [{key, Key}]),
				    AccOption
			    end
		    end, [], Options),
    %% Check if any mandatory options are missing! (Throw reason) 
    [case lists:keysearch(Key, 1, CheckedUpOptions) of
	 {value, _} -> ok;
	 _ ->
	     ?hostrt("validate_options -> missing - mandatory",
		     [{key, Key}]),	     
	     throw({error, Reason})
     end || {Key, _, true, Reason} <- ValidOptions],
    
    CheckedUpOptions.


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_arch_test() ->
    ?assert(is_arch (linux)).

validate_options_null_test() ->
    Option = {op1, "value1"},
    Option2 = {op2, "value2"},
    ValidTemplate = {op1, fun(_Any) -> true end, false, default},
    InvalidTemplate = {op1, fun(_Any) -> false end, false, default},
    MandatoryTemplate = {op1, fun(_Any) -> true end, true, missing},

    %% Emmty option list
    ?assertMatch([], validate_options([], [])),
    ?assertMatch([], validate_options([], [ValidTemplate])),
    ?assertMatch([], validate_options([], [InvalidTemplate])),
    ?assertException(throw, {error, missing}, validate_options([], [MandatoryTemplate])),

    
    ?assertMatch([Option], validate_options([Option], [ValidTemplate])),
    ?assertMatch([], validate_options([Option2], [InvalidTemplate, ValidTemplate])),
    
    ?assertException(throw, {error,{op1,"value1"}},
    		     validate_options([Option2, Option, {op3, ""}], [InvalidTemplate])),
    
    ?assertException(throw, {error, missing},
     		     validate_options([Option2], [MandatoryTemplate])),    
    ok.

-endif.
