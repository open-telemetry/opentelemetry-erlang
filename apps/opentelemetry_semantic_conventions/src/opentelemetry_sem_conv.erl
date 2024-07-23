-module(opentelemetry_sem_conv).

-export([stability_opt_in/0]).

-export_type([stability_option/0]).

-type stability_option() :: binary().

-spec stability_opt_in() -> [stability_option()].
stability_opt_in() ->
  OptInsList = string:split(os:getenv("OTEL_SEMCONV_STABILITY_OPT_IN", ""), ","),
  HttpStability = http_stability(OptInsList),
  OptIns = [HttpStability],
  lists:filter(fun(Opt) -> (Opt =/= undefined) end, OptIns).

http_stability(OptIns) ->
  Dup = lists:member("http/dup", OptIns),
  Http = lists:member("http", OptIns),
  case {Dup, Http} of
    {true, _} ->
      <<"http/dup">>;
    {_, true} ->
      <<"http">>;
    _ ->
      undefined
  end.
