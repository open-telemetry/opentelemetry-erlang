-module(opentelemetry_sem_conv).

-export([stability_opt_in/0]).

-export_type([
  http_stability/0,
  stability_opt_ins/0]).

-type http_stability() :: http | http_dup | default.
-type stability_opt_ins() :: #{http => http_stability()}.


-spec stability_opt_in() -> stability_opt_ins().
stability_opt_in() ->
  OptInsList = string:split(os:getenv("OTEL_SEMCONV_STABILITY_OPT_IN", ""), ","),
  HttpStability = http_stability(OptInsList),
  #{http => HttpStability}.

http_stability(OptIns) ->
  Dup = lists:member("http/dup", OptIns),
  Http = lists:member("http", OptIns),
  case {Dup, Http} of
    {true, _} ->
      http_dup;
    {_, true} ->
      http;
    _ ->
      default
  end.
