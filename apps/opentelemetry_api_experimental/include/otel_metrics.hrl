-record(instrument, {module        :: module() | '_',
                     meter         :: otel_meter:t() | '_',
                     name          :: otel_instrument:name() | '_',
                     description   :: otel_instrument:description() | undefined | '_',
                     kind          :: otel_instrument:kind() | '_',
                     unit          :: otel_instrument:unit() | undefined | '_',
                     callback      :: otel_instrument:callback() | undefined | '_',
                     callback_args :: term() | undefined | '_'}).

-define(KIND_COUNTER, counter).
-define(KIND_OBSERVABLE_COUNTER, observable_counter).
-define(KIND_HISTOGRAM, histogram).
-define(KIND_OBSERVABLE_GAUGE, observable_gauge).
-define(KIND_UPDOWN_COUNTER, updown_counter).
-define(KIND_OBSERVABLE_UPDOWNCOUNTER, observable_updowncounter).
