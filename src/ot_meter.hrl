-record(meter, {module :: module(),
                library_resource :: term() | undefined,
                resource :: term() | undefined}).
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

-record(active_instrument, {key :: {ot_meter:name(), ot_meter:label_set()},
                            instrument :: ot_meter:instrument(),
                            aggregator :: module(),
                            checkpoint :: number() | undefined,
                            value :: number() | {number(), integer()} | term()}).
-type active_instrument() :: #active_instrument{}.

-define(ACTIVE_TAB, active_instrument_updates).

-record(observer, {name :: ot_meter:name(),
                   observer_result :: ot_observer:observer_result(),
                   callback :: ot_observer:callback()}).
