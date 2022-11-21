-define(DEFAULT_METER_PROVIDER, otel_meter_provider_default).

-define(AGGREGATION_TEMPORALITY_DELTA, aggregation_temporality_delta).
-define(AGGREGATION_TEMPORALITY_CUMULATIVE, aggregation_temporality_cumulative).
-define(AGGREGATION_TEMPORALITY_UNSPECIFIED, aggregation_temporality_unspecified).

-record(meter, {module                  :: module() | '_',
                instrumentation_scope   :: opentelemetry:instrumentation_scope() | undefined,
                provider                :: atom() | '_',
                instruments_table       :: ets:tid() | '_',
                view_aggregations_table :: ets:tid() | '_',
                metrics_table           :: ets:tid() | '_'}).

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
         key :: {term(),  opentelemetry:attributes_map(), pid()} | '$1',
         start_time_unix_nano :: integer() | '_' | '$2' | {const, integer()},
         checkpoint :: number() | undefined | '_' | '$2' | '$3',
         value :: number() | undefined | '$2' | '$3'
        }).

-record(last_value_aggregation,
        {
         %% TODO: attributes should be a tuple of just the values, sorted by attribute name
         key :: {term(),  opentelemetry:attributes_map(), pid()} | '$1',
         checkpoint :: number() | undefined | '_' | '$2',
         value :: number() | undefined | '$2'
        }).


-record(explicit_histogram_checkpoint,
        {
         bucket_counts :: tuple() | '$5',
         min :: number() | '$6',
         max :: number() | '$7',
         sum :: number() | '$8'
        }).

-record(explicit_histogram_aggregation,
        {
         %% TODO: attributes should be a tuple of just the values, sorted by attribute name
         key :: {term(),  opentelemetry:attributes_map(), pid()} | '$1',
         start_time_unix_nano :: integer() | '$2',
         %% instrument_temporality :: otel_aggregation:temporality(),
         %% default: [0.0, 5.0, 10.0, 25.0, 50.0, 75.0, 100.0, 250.0, 500.0, 1000.0]
         boundaries :: [float()] | '$3',
         record_min_max :: boolean() | '$4',
         checkpoint :: #explicit_histogram_checkpoint{} | undefined | '_' | {#explicit_histogram_checkpoint{}},
         zeroed_counts :: tuple(),
         bucket_counts :: tuple() | '$5',
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
         bucket_counts :: tuple(),
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
