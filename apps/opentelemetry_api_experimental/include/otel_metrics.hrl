-record(instrument, {module        :: module(),
                     meter         :: otel_meter:t(),
                     name          :: otel_instrument:name(),
                     description   :: otel_instrument:description(),
                     kind          :: otel_instrument:kind(),
                     unit          :: otel_instrument:unit() | undefined,
                     callback      :: otel_instrument:callback() | undefined,
                     callback_args :: term() | undefined}).

-define(KIND_COUNTER, counter).
-define(KIND_OBSERVABLE_COUNTER, observable_counter).
-define(KIND_HISTOGRAM, histogram).
-define(KIND_OBSERVABLE_GAUGE, observable_gauge).
-define(KIND_UPDOWN_COUNTER, updown_counter).
-define(KIND_OBSERVABLE_UPDOWNCOUNTER, observable_updowncounter).
