-record(meter, {module                  :: module(),
                instrumentation_library :: ot_tracer_server:instrumentation_library() | undefined,
                telemetry_library       :: ot_tracer_server:telemetry_library() | undefined,
                resource                :: ot_resource:t() | undefined}).
-type meter() :: #meter{}.

-record(instrument, {name         :: ot_meter:name(),
                     description  :: ot_meter:description(),
                     kind         :: ot_meter:metric_kind(),
                     input_type   :: ot_meter:input_type(),
                     mode         :: ot_meter:instrument_mode(),
                     numeric_type :: atom(),
                     label_keys   :: [atom()],
                     unit         :: ot_meter:unit()}).
-type instrument() :: #instrument{}.

-type aggregator() :: module().

-record(active_instrument, {key :: {ot_meter:name(), ot_meter:label_set()},
                            instrument :: ot_meter:instrument(),
                            aggregator :: aggregator(),
                            checkpoint :: number() | undefined,
                            current :: number() | {number(), integer()} | term()}).
-type active_instrument() :: #active_instrument{}.

-record(bound_instrument, {key :: {ot_meter:name(), ot_meter:label_set()},
                           input_type   :: ot_meter:input_type(),
                           aggregator :: aggregator()}).
-type bound_instrument() :: #bound_instrument{} | unknown_instrument.

-record(observer, {name       :: ot_meter:name(),
                   instrument :: ot_observer:observer_instrument(),
                   callback   :: ot_observer:callback()}).
