-module(otel_aggregation).

-export([default_mapping/0,
         instrument_temporality/1]).

%% -type t() :: drop | sum | last_value | histogram.
-type t() :: otel_aggregation_drop:t() | otel_aggregation_sum:t() |
             otel_aggregation_last_value:t() | otel_aggregation_histogram_explicit:t().

-export_type([t/0]).

-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_metrics.hrl").

-spec default_mapping() -> #{otel_instrument:kind() => module()}.
default_mapping() ->
    #{?KIND_COUNTER => otel_aggregation_sum,
      ?KIND_OBSERVABLE_COUNTER => otel_aggregation_sum,
      ?KIND_HISTOGRAM => otel_aggregation_histogram_explicit,
      ?KIND_OBSERVABLE_GAUGE => otel_aggregation_last_value,
      ?KIND_UPDOWN_COUNTER => otel_aggregation_sum,
      ?KIND_OBSERVABLE_UPDOWNCOUNTER => otel_aggregation_sum}.

instrument_temporality(#instrument{kind=?KIND_COUNTER}) ->
    ?AGGREGATION_TEMPORALITY_DELTA;
instrument_temporality(#instrument{kind=?KIND_OBSERVABLE_COUNTER}) ->
    ?AGGREGATION_TEMPORALITY_CUMULATIVE;
instrument_temporality(#instrument{kind=?KIND_UPDOWN_COUNTER}) ->
    ?AGGREGATION_TEMPORALITY_DELTA;
instrument_temporality(#instrument{kind=?KIND_OBSERVABLE_UPDOWNCOUNTER}) ->
    ?AGGREGATION_TEMPORALITY_CUMULATIVE;
instrument_temporality(#instrument{kind=?KIND_HISTOGRAM}) ->
    ?AGGREGATION_TEMPORALITY_UNSPECIFIED;
instrument_temporality(#instrument{kind=?KIND_OBSERVABLE_GAUGE}) ->
    ?AGGREGATION_TEMPORALITY_UNSPECIFIED.
