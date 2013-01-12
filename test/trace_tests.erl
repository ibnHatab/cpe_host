%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%
%%% @end
%%% Created : 22 Nov 2012 by vlad <lib.aca55a@gmail.com>


-module(trace_tests).

-include_lib("eunit/include/eunit.hrl").

-include("cpe_host/src/host_internal.hrl").


report_event_test_() ->
    { setup,
      fun () ->
	      ok = lager:start(),
	      lager:set_loglevel(lager_console_backend, info)
      end,
      fun (_O)->
	      ok
      end,

      [
       ?_test(begin
       		  cpe_trace:enable(max, all),
       		  cpe_trace:disable()
       		      ,
       		  cpe_trace:enable(max, all),
       		  cpe_trace:disable()
       	      end)
       ,
       ?_test(begin
       		   cpe_trace:enable(max, all)
       		       , cpe_trace:report_event(50, ?MODULE,  "START", "UTEST")
       		       , cpe_trace:report_event(50, ?MODULE, http,  'Message', "args")
       		       , cpe_trace:report_event(50, ?MODULE,  "Label", "Contents")
       		       , cpe_trace:disable()
       	       end)
       ,
       ?_test(begin

       		  lager:set_loglevel(lager_console_backend, error),

       		  cpe_trace:enable(max, all)
       		      , cpe_trace:report_event(50, ?MODULE,  "lager:set_loglevel", "lager_console_backend, error")
       		      , cpe_trace:disable()
       		      , lager:set_loglevel(lager_console_backend, info)
       	       end)
       ,
       ?_test(begin
       		  cpe_trace:enable(max, all),
       		  cpe_trace:report_event(50, ?MODULE,  "separator",
       					  "--------------------------------------"),
       		  cpe_trace:report_event(50, cpe, "not a module", "? error"),
       		  cpe_trace:disable()
       	      end)
       ,

       %% enable per module
       ?_test(begin
       		  cpe_trace:enable(max, cwmp_cli),
       		  cpe_trace:report_event(50,  cwmp_cli, "cwmp_cli", "? OK"),
       		  cpe_trace:disable()
       	      end)
       ,

       %% enable level
       ?_test(begin
       		  cpe_trace:enable(60, cwmp_cli),
       		  cpe_trace:report_event(50,  cwmp_cli, "cwmp_cli", "? OK"),
       		  cpe_trace:report_event(70,  cwmp_cli, "cwmp_cli", "? NOK"),
       		  cpe_trace:disable()
       	      end)

      ]

    }.


unprotocol_test_() ->
    { setup,
      fun () ->
	      ok = lager:start(),
	      lager:set_loglevel(lager_console_backend, info)
      end,
      fun (_O)->
	      ok
      end,
      [?_test(begin
%%		  cpe_trace:start_ev([{actors, [client,  server] }]),
		  cpe_trace:enable(max, all),
  		  ?message(60, client, server, 'OHAI',
  			   "sent by a client to a server when it wants to initiate a dialog."), 
		  		  ?message(60, server, client, 'ORYL?',
 			   "sent by a server to a client when it wants to get authentication information such as a password hash."),
 		  ?message(60, client, server, 'YARLY',
 			   "sent by a client to a server when it wants to authenticate with some information."),
 		  ?message(60, client, server, 'ICANHAZ?',
 			   "sent by a client to a server, along with some arguments, to request some resource from the server."),
 		  ?message(60, server, client, 'CHEEZBURGER',
 			   "sent by a server to a client, along with some properties, to indicate a resource delivered from server to client."),
 		  ?message(60, client, server, 'NOM',
 			   "sent by a client to a server to acknowledge that it's happily received some resource."),
 		  ?message(60, client, server, 'LOL',
 			   "sent by a client to a server to reject a resource as inedible."),
 		  ?message(60, server, client, 'KITTEH',
 			   "used in general to indicate a client or server peer. Note: considered rather too cute for srs urban protocols."),
 		  ?message(60, server, client, 'KTHXBAI',
 			   "sent by a server to a client, or a client to a server, when it finishes some dialog."),
 		  ?message(60, server, client, 'HUGZ',
 			   "sent by a server to a client, or a client to a server, to indicate that it's still around and not going anywhere just now."
 			   "Kind of like a keep-alive heartbeat."),
 		  ?message(60, server, client, 'SRSLY?',
 			   "sent by a server to a client when it refuses some operation due to access rights."),
 		  ?message(60, server, client, 'RTFM',
 			   "sent by a server to a client that uses an invalid value or argument."),
 		  ?message(60, server, client, 'WTF?',
 			   "sent by a server to a client when it refuses some operation because it's not meaningful. After a WTF? the client may retry."),
 		  ?message(60, server, client, 'ROTFL',
 			   "sent by a server to a client when it refuses some operation and disconnects the client for being silly."),
 		  ?message(60, server, client, 'PWNED',
 "sent by a server to a client when it cannot perform some operation due to being pretty much exhausted."),

		  cpe_trace:disable()
	      end)]}.




