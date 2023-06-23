-define(DEFAULT_METER_PROVIDER, otel_meter_provider_default).

-type match_var() :: '_' | '$1' | '$2' | '$3' | '$4' | '$5' | '$6' | '$7' | '$8' | '$9'.
-type match_expr(A) :: undefined | match_var() | {const, A}.
-type match_spec(A) :: match_expr(A).

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
         key :: otel_aggregation:key() | match_spec(otel_aggregation:key()) | {element, 2, '$_'},
         start_time_unix_nano :: integer() | match_spec(integer()),
         last_start_time_unix_nano :: integer() | match_spec(integer()),
         checkpoint :: number() | match_spec(number()) | {'+', '$2', '$3'} | {'+', '$3', '$4'},
         previous_checkpoint :: number() | match_spec(number()) | {'+', '$5', '$6'},
         int_value :: number() | match_spec(number()) | {'+', '$3', {const, number()}},
         float_value :: number() | match_spec(number()) | {'+', '$4', {const, number()}}
        }).

-record(last_value_aggregation,
        {
         %% TODO: attributes should be a tuple of just the values, sorted by attribute name
         key :: otel_aggregation:key() | match_spec(otel_aggregation:key()),
         checkpoint :: number() | match_spec(number()),
         value :: number() | match_spec(number()),
         start_time_unix_nano :: integer() | match_spec(integer()),
         last_start_time_unix_nano :: integer() | match_spec(integer())
        }).


-record(explicit_histogram_checkpoint,
        {
         bucket_counts :: counters:counters_ref() | match_spec(counters:counters_ref()),
         min :: number() | match_spec(number()),
         max :: number() | match_spec(number()),
         sum :: number() | match_spec(number()),
         start_time_unix_nano :: integer() | match_spec(number())
        }).

-record(explicit_histogram_aggregation,
        {
         %% TODO: attributes should be a tuple of just the values, sorted by attribute name
         key :: otel_aggregation:key() | match_spec(otel_aggregation:key()),
         start_time_unix_nano :: integer() | {const, eqwalizer:dynamic()} | '$9' | '$2' | undefined,
         %% instrument_temporality :: otel_aggregation:temporality(),
         %% default: [0.0, 5.0, 10.0, 25.0, 50.0, 75.0, 100.0, 250.0, 500.0, 1000.0]
         boundaries :: [float()] | match_spec([float()]),
         record_min_max :: boolean() | match_spec(boolean()),
         checkpoint :: #explicit_histogram_checkpoint{} | match_spec(#explicit_histogram_checkpoint{}) | {#explicit_histogram_checkpoint{}},
         bucket_counts :: counters:counters_ref() | match_spec(undefined),
         min :: number() | infinity | match_spec(number()),
         max :: number() | match_spec(number()),
         sum :: number() | match_spec(number())
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
         start_time_unix_nano :: integer() | match_spec(integer()) | {const, eqwalizer:dynamic()},
         time_unix_nano :: integer(),
         count :: number(),
         sum :: float() | match_spec(integer()),
         bucket_counts :: list(),
         explicit_bounds :: [float()] | match_spec([float()]),
         exemplars :: list(),
         flags :: integer(),
         min :: integer() | infinity | match_spec(integer()),
         max :: integer() | match_spec(integer())
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
