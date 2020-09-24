-record(meter, {module                  :: module(),
                instrumentation_library :: ot_tracer_server:instrumentation_library() | undefined,
                telemetry_library       :: ot_tracer_server:telemetry_library() | undefined,
                resource                :: ot_resource:t() | undefined}).
-type meter() :: #meter{}.

-record(instrument, {name         :: ot_meter:name(),
                     description  :: ot_meter:description(),
                     kind         :: ot_meter:instrument_kind(),
                     number_kind  :: ot_meter:number_kind(),
                     monotonic    :: boolean(),
                     synchronous  :: boolean(),
                     unit         :: ot_meter:unit()}).
-type instrument() :: #instrument{}.

-record(descriptor, {name :: ot_meter:name(),
                     kind :: ot_meter:instrument_kind(),
                     number_kind :: ot_meter:number_kind(),
                     config :: ot_meter:instrument_config()}).

-record(config, {%% Description describes the instrument in human-readable terms.
                 description :: ot_meter:description(),
                 %% Unit describes the measurement unit for a instrument.
                 unit :: ot_meter:unit()}).

-type aggregator() :: module().

-record(active_instrument, {key :: {ot_meter:name(), ot_meter:labels()},
                            instrument :: ot_meter:instrument(),
                            aggregator :: aggregator(),
                            checkpoint :: number() | undefined,
                            current :: number() | {number(), integer()} | term()}).
-type active_instrument() :: #active_instrument{}.

-record(bound_instrument, {key :: {ot_meter:name(), ot_meter:labels()},
                           number_kind :: ot_meter:number_kind(),
                           aggregator :: aggregator()}).
-type bound_instrument() :: #bound_instrument{} | unknown_instrument.

-record(observer, {name       :: ot_meter:name(),
                   instrument :: ot_observer:observer_instrument(),
                   callback   :: ot_observer:callback()}).
