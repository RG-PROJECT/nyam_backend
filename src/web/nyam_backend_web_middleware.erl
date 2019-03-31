-module(nyam_backend_web_middleware).
-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
  #{
    method := Method,
    path := Path,
    qs := Qs,
    peer := {IP, _}
  } = Req,
  logger:notice("~p - ~p ~p ~p ~p", [inet:ntoa(IP), calendar:local_time(), binary_to_list(Method), binary_to_list(Path), binary_to_list(Qs)]),
  {ok, Req, Env}.
