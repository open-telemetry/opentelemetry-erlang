-ifndef(MATCH_SPEC_TYPES_DEFINED).
-include_lib("opentelemetry_api_experimental/include/match_spec.hrl").
-endif.

-define(DEFAULT_METER_PROVIDER, otel_meter_provider_default).

-record(meter,
        {
         module                  :: module() | '_',
         instrumentation_scope   :: opentelemetry:instrumentation_scope() | undefined,
         provider                :: atom() | '_',
         instruments_tab         :: ets:table() | '_',
         view_aggregations_tab   :: ets:table() | '_',
         metrics_tab             :: ets:table() | '_'
        }).

-record(measurement,
        {
         instrument :: otel_instrument:t(),
         value :: number(),
         attributes :: opentelemetry:attributes_map()
        }).

-record(drop_aggregation, {}).

-record(sum_aggregation,
        {
         %% TODO: attributes should be a tuple of just the values, sorted by attribute name
         key :: match_spec(otel_aggregation:key()) |  otel_aggregation:key_match_spec() | undefined | {element, 2, '$_'},
         start_time_unix_nano :: match_spec(integer()) | undefined,
         last_start_time_unix_nano :: match_spec(integer()) | undefined,
         checkpoint :: match_spec(number()) | undefined | {'+', '$2', '$3'} | {'+', '$3', '$4'},
         previous_checkpoint :: match_spec(number()) | undefined | {'+', '$5', '$6'},
         int_value :: match_spec(number()) | undefined | {'+', '$3', {const, number()}},
         float_value :: match_spec(number()) | undefined | {'+', '$4', {const, number()}}
        }).

-record(last_value_aggregation,
        {
         %% TODO: attributes should be a tuple of just the values, sorted by attribute name
         key :: match_spec(otel_aggregation:key()) | otel_aggregation:key_match_spec() | undefined,
         checkpoint :: match_spec(number()) | undefined,
         value :: match_spec(number()) | undefined,
         start_time_unix_nano :: match_spec(integer()) | undefined,
         last_start_time_unix_nano :: match_spec(integer()) | undefined
        }).


-record(explicit_histogram_checkpoint,
        {
         bucket_counts :: match_spec(counters:counters_ref()) | undefined,
         min :: match_spec(number()) | undefined,
         max :: match_spec(number()) | undefined,
         sum :: match_spec(number()) | undefined,
         start_time_unix_nano :: match_spec(number()) | undefined
        }).

-record(explicit_histogram_aggregation,
        {
         %% TODO: attributes should be a tuple of just the values, sorted by attribute name
         key :: match_spec(otel_aggregation:key()) | otel_aggregation:key_match_spec() | undefined,
         start_time_unix_nano :: integer() | {const, eqwalizer:dynamic()} | '$9' | '$2' | undefined,
         %% instrument_temporality :: otel_aggregation:temporality(),
         %% default: [0.0, 5.0, 10.0, 25.0, 50.0, 75.0, 100.0, 250.0, 500.0, 1000.0]
         explicit_bucket_boundaries :: match_spec([float()]) | undefined,
         record_min_max :: match_spec(boolean()) | undefined,
         checkpoint :: match_spec(#explicit_histogram_checkpoint{}) | {#explicit_histogram_checkpoint{}} | undefined,
         bucket_counts :: counters:counters_ref() | match_spec(undefined),
         min :: infinity | match_spec(number()) | undefined,
         max :: match_spec(number()) | undefined,
         sum :: match_spec(number()) | undefined
        }).

-record(datapoint,
        {
         attributes :: opentelemetry:attributes_map(),
         start_time_unix_nano :: integer(),
         time_unix_nano :: integer(),
         value :: number(),
         exemplars :: list() | undefined,
         flags :: integer() %% uint32
        }).

-record(sum,
        {
         datapoints :: [#datapoint{}],
         aggregation_temporality :: otel_instrument:temporality(),
         is_monotonic :: boolean()
        }).

-record(gauge,
        {
         datapoints :: [#datapoint{}]
        }).

-record(histogram_datapoint,
        {
         attributes :: opentelemetry:attributes_map(),
         start_time_unix_nano :: match_spec(integer()) | {const, eqwalizer:dynamic()}  | undefined,
         time_unix_nano :: integer(),
         count :: number(),
         sum :: float() | match_spec(integer()) | undefined,
         bucket_counts :: list(),
         explicit_bounds :: match_spec([float()]) | undefined,
         exemplars :: list(),
         flags :: integer(),
         min ::  infinity | match_spec(integer()) | undefined,
         max :: match_spec(integer()) | undefined
        }).

-record(histogram,
       {
        datapoints :: [#histogram_datapoint{}],
        aggregation_temporality :: otel_instrument:temporality()
       }).

-record(metric,
        {
         name :: otel_view:name(),
         scope :: opentelemetry:instrumentation_scope(),
         description :: otel_instrument:description(),
         unit :: otel_instrument:unit(),
         data :: #sum{} | #gauge{} | #histogram{}
        }).
