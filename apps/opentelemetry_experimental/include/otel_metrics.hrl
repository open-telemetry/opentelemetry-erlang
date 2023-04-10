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
         key :: {term(),  opentelemetry:attributes_map(), reference()} | '$1' | {element, 2, '$_'},
         start_time_unix_nano :: integer() | '_' | '$1' | {const, integer()} | '$4' | '$2',
         last_start_time_unix_nano :: integer() | undefined | '$5' | '$4' | '_' | '$2',
         checkpoint :: number() | undefined | '_' | '$2' | '$3' | '$5' | {'+', '$2', '$3'} | {'+', '$3', '$4'},
         previous_checkpoint :: number() | undefined | '_' | '$5',
         int_value :: number() | undefined | '$3' | {'+', '$3', {const, number()}} | '$2',
         float_value :: number() | undefined | '$4' | {'+', '$4', {const, number()}} | '$3'
        }).

-record(last_value_aggregation,
        {
         %% TODO: attributes should be a tuple of just the values, sorted by attribute name
         key :: {atom(),  opentelemetry:attributes_map(), reference()} | '$1',
         checkpoint :: number() | undefined | '_' | '$2',
         value :: number() | undefined | '$2',
         start_time_unix_nano :: integer() | '_' | '$3' | {const, integer()},
         last_start_time_unix_nano :: integer() | undefined | '$4' | '_' | '$3'
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
         start_time_unix_nano :: integer() | '$2' | '$9' | {const, eqwalizer:dynamic()},
         %% instrument_temporality :: otel_aggregation:temporality(),
         %% default: [0.0, 5.0, 10.0, 25.0, 50.0, 75.0, 100.0, 250.0, 500.0, 1000.0]
         boundaries :: [float()] | '$3' | '$2',
         record_min_max :: boolean() | '$4' | '$3',
         checkpoint :: #explicit_histogram_checkpoint{} | undefined | '_' | {#explicit_histogram_checkpoint{}},
         bucket_counts :: counters:counters_ref() | undefined | '$5' | {const, undefined},
         min :: number() | infinity | '$6',
         max :: number() | '$7',
         sum :: number() | '$8'
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
         start_time_unix_nano :: integer() | undefined | '$2' | '$9' | {const, eqwalizer:dynamic()},
         time_unix_nano :: integer(),
         count :: number(),
         sum :: float() | '$8',
         bucket_counts :: list(),
         explicit_bounds :: [float()] | '$3' | '$2',
         exemplars :: list(),
         flags :: integer(),
         min :: integer() | undefined | 'infinity' | '$6',
         max :: integer() | undefined | '$7'
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
