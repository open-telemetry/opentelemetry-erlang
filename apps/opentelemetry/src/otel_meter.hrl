-record(meter, {module                  :: module(),
                instrumentation_library :: otel_tracer_server:instrumentation_library() | undefined,
                telemetry_library       :: otel_tracer_server:telemetry_library() | undefined,
                resource                :: otel_resource:t() | undefined}).
-type meter() :: #meter{}.

-record(instrument, {name         :: otel_meter:name(),
                     description  :: otel_meter:description(),
                     kind         :: otel_meter:instrument_kind(),
                     number_kind  :: otel_meter:number_kind(),
                     monotonic    :: boolean(),
                     synchronous  :: boolean(),
                     unit         :: otel_meter:unit()}).
-type instrument() :: #instrument{}.

-record(descriptor, {name :: otel_meter:name(),
                     kind :: otel_meter:instrument_kind(),
                     number_kind :: otel_meter:number_kind(),
                     config :: otel_meter:instrument_config()}).

-record(config, {%% Description describes the instrument in human-readable terms.
                 description :: otel_meter:description(),
                 %% Unit describes the measurement unit for a instrument.
                 unit :: otel_meter:unit()}).

-type aggregator() :: module().

-record(active_instrument, {key :: {otel_meter:name(), otel_meter:labels()},
                            instrument :: instrument(),
                            aggregator :: aggregator(),
                            checkpoint :: number() | undefined,
                            current :: number() | {number(), integer()} | term()}).
-type active_instrument() :: #active_instrument{}.

-record(bound_instrument, {key :: {otel_meter:name(), otel_meter:labels()},
                           number_kind :: otel_meter:number_kind(),
                           aggregator :: aggregator()}).
-type bound_instrument() :: #bound_instrument{} | unknown_instrument.

-record(observer, {name       :: otel_meter:name(),
                   instrument :: otel_observer:observer_instrument(),
                   callback   :: otel_observer:callback()}).
