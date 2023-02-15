-define(DEFAULT_METER_PROVIDER, otel_meter_provider_default).

-define(AGGREGATION_TEMPORALITY_DELTA, aggregation_temporality_delta).
-define(AGGREGATION_TEMPORALITY_CUMULATIVE, aggregation_temporality_cumulative).
-define(AGGREGATION_TEMPORALITY_UNSPECIFIED, aggregation_temporality_unspecified).

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
         key :: {term(),  opentelemetry:attributes_map(), reference()} | '$1' | {element, 2, '$_'},
         start_time_unix_nano :: integer() | '_' | '$1' | {const, integer()},
         last_start_time_unix_nano :: integer() | '$4',
         checkpoint :: number() | undefined | '_' | '$2' | '$3',
         int_value :: number() | undefined | '$3' | {'+', '$3', {const, number()}},
         float_value :: number() | undefined | '$4' | {'+', '$4', {const, number()}}
        }).

-record(last_value_aggregation,
        {
         %% TODO: attributes should be a tuple of just the values, sorted by attribute name
         key :: {term(),  opentelemetry:attributes_map(), reference()} | '$1',
         checkpoint :: number() | undefined | '_' | '$2',
         value :: number() | undefined | '$2',
         start_time_unix_nano :: integer() | '_' | '$3' | {const, integer()},
         last_start_time_unix_nano :: integer() | '$4'
        }).


-record(explicit_histogram_checkpoint,
        {
         bucket_counts :: counters:counters_ref() | undefined | '$5',
         min :: number() | '$6',
         max :: number() | '$7',
         sum :: number() | '$8',
         start_time_unix_nano :: integer() | '$9'
        }).

-record(explicit_histogram_aggregation,
        {
         %% TODO: attributes should be a tuple of just the values, sorted by attribute name
         key :: {term(),  opentelemetry:attributes_map(), reference()} | '$1',
         start_time_unix_nano :: integer() | '$2',
         %% instrument_temporality :: otel_aggregation:temporality(),
         %% default: [0.0, 5.0, 10.0, 25.0, 50.0, 75.0, 100.0, 250.0, 500.0, 1000.0]
         boundaries :: [float()] | '$3',
         record_min_max :: boolean() | '$4',
         checkpoint :: #explicit_histogram_checkpoint{} | undefined | '_' | {#explicit_histogram_checkpoint{}},
         bucket_counts :: counters:counters_ref() | undefined | '$5',
         min :: number() | infinity | '$6',
         max :: number() | '$7',
         sum :: number() | '$8'
        }).

-record(datapoint,
        {
         attributes :: opentelemetry:attributes_map(),
         start_time_unix_nano :: integer() | undefined,
         time_unix_nano :: integer(),
         value :: number(),
         exemplars :: list() | undefined,
         flags :: integer() %% uint32
        }).

-record(sum,
        {
         datapoints :: [#datapoint{}],
         aggregation_temporality :: otel_aggregation:temporality(),
         is_monotonic :: boolean()
        }).

-record(gauge,
        {
         datapoints :: [#datapoint{}]
        }).

-record(histogram_datapoint,
        {
         attributes :: opentelemetry:attributes_map(),
         start_time_unix_nano :: integer() | undefined,
         time_unix_nano :: integer(),
         count :: number(),
         sum :: float(),
         bucket_counts :: list(),
         explicit_bounds :: [float()],
         exemplars :: list(),
         flags :: integer(),
         min :: integer() | undefined,
         max :: integer() | undefined
        }).

-record(histogram,
       {
        datapoints :: [#histogram_datapoint{}],
        aggregation_temporality :: otel_aggregation:temporality()
       }).

-record(metric,
        {
         name :: otel_view:name(),
         scope :: opentelemetry:instrumentation_scope(),
         description :: otel_instrument:description(),
         unit :: otel_instrument:unit(),
         data :: #sum{} | #gauge{} | #histogram{}
        }).
