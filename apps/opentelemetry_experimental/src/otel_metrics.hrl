-define(DEFAULT_METER_PROVIDER, otel_meter_provider_default).

-define(AGGREGATION_TEMPORALITY_DELTA, aggregation_temporality_delta).
-define(AGGREGATION_TEMPORALITY_CUMULATIVE, aggregation_temporality_cumulative).
-define(AGGREGATION_TEMPORALITY_UNSPECIFIED, aggregation_temporality_unspecified).

-record(meter, {module                  :: module(),
                instrumentation_library :: otel_tracer_server:instrumentation_library() | undefined,
                provider                :: atom()}).

%% The name, version and language of this OpenTelemetry library
-record(telemetry_library, {name     :: unicode:unicode_binary() | undefined,
                            language :: unicode:unicode_binary() | undefined,
                            version  :: unicode:unicode_binary() | undefined}).

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
         key :: {term(),  opentelemetry:attributes_map()},
         start_time_unix_nano :: integer(),
         value :: number() | undefined
        }).

-record(last_value_aggregation,
        {
         %% TODO: attributes should be a tuple of just the values, sorted by attribute name
         key :: {term(),  opentelemetry:attributes_map()},
         value :: number() | undefined
        }).

-record(explicit_histogram_aggregation,
        {
         %% TODO: attributes should be a tuple of just the values, sorted by attribute name
         key :: {term(),  opentelemetry:attributes_map()},
         start_time_unix_nano :: integer(),
         %% instrument_temporality :: otel_aggregation:temporality(),
         boundaries :: list(),
         %% default: {0.0, 5.0, 10.0, 25.0, 50.0, 75.0, 100.0, 250.0, 500.0, 1000.0}
         bucket_counts :: tuple(),
         record_min_max :: boolean(),
         min :: number() | neg_infinity | infinity,
         max :: number() | neg_infinity | infinity,
         sum :: number()
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
         count :: integer(),
         sum :: float(),
         bucket_counts :: integer(),
         explicit_bounds :: tuple(), %% tuple of floats
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
         description :: otel_instrument:description(),
         unit :: otel_instrument:unit(),
         data :: #sum{} | #gauge{} | #histogram{}
        }).
