%%% @author Gratiela Ghergu <Gratiela.Ghergu@Alcatel-Lucent.com>
%%% @copyright (C) 2012, Gratiela Ghergu
%%% @doc
%%%  GPROC tests
%%% @end
%%% Created : 13 Nov 2012 by Gratiela Ghergu <Gratiela.Ghergu@Alcatel-Lucent.com>

-module(test_gproc).

-include("cpe_host/src/host_internal.hrl").

-include_lib("eunit/include/eunit.hrl").

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

gproc_register_test_() ->
    { setup,
      fun () ->
	      ensure_started(gproc)
      end,
      fun (_O)->
	      application:stop(gproc)
      end,

      [
       %% register/unregister
       ?_test(begin
		  gproc:reg({n, l, "rpc"}, "Grati")
	      end),
       ?_test(begin
		  Pid = spawn_link(fun() ->
					   _B = gproc:reg({n, l, "http"}, "Grati"),
					   ?DBG(self()),
					   receive
					       stop ->
						   ?DBG(stoped)
					   end
				   end),
		  sleep(1000),
		  ?assertEqual(Pid, gproc:where({n, l, "http"})),
		  Pid ! stop
	      end),
       ?_test(begin
		  Pid = spawn_link(fun() ->
					   receive
					     register  ->
						   _B = gproc:reg({n, l, "http"}, "Grati"),
						   ?DBG({wait_register, self()})
					   end,
					   receive
					       stop ->
						   ?DBG(stoped)
					   end
				   end),
		  Pid ! register,
		  gproc:await({n, l, "http"}, 1000),
		  ?assertEqual(Pid, gproc:where({n, l, "http"})),
		  Pid ! stop
	      end),

       %% set/get properties
       ?_test(begin
		  _B = gproc:reg({n, l, "http"}, "Grati"),
		  ?DBG(self()),
		  ?assertEqual("Grati", gproc:get_value({n, l, "http"})),
		  gproc:set_value({n, l, "http"}, "Vlad"),
		  ?assertEqual("Vlad", gproc:get_value({n, l, "http"}))
	      end),

       %% transfer to process
       ?_test(begin
		  Pid = spawn_link(fun() ->
					   receive
					     register  ->
						   _B = gproc:reg({n, l, "cdb"}, "running"),
						   ?DBG({wait_register, self()})
					   end,
					   receive
					      {transfer, ToPid} ->
						   gproc:give_away({n, l, "cdb"}, ToPid),
						   ?DBG({transfer, self(), ToPid}),
						   ToPid ! tranfer_complete
					   end,
					   receive
					       stop ->
						   ?DBG(stoped)
					   end
				   end),
		  Pid ! register,
		  gproc:await({n, l, "cdb"}, 1000),
		  ?assertEqual(Pid, gproc:where({n, l, "cdb"})),
		  %% I'm CDB running now !!
		  Pid ! {transfer, self()},
		  receive
		      tranfer_complete ->
			  ?DBG(tranfer_complete),
			  ?assertEqual(self(), gproc:where({n, l, "cdb"}))
		  after 
		      1000 -> ?assert(false orelse "transfer timeout")
		  end,

		  Pid ! stop
	      end)
      ]}.

sleep(T) -> receive after T -> ok end.

