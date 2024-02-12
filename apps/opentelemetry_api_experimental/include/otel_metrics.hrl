-include_lib("match_spec.hrl").

-record(instrument, {module          :: module(),
                     meter           :: otel_meter:t(),
                     name            :: otel_instrument:name(),
                     description     :: otel_instrument:description() | undefined,
                     kind            :: otel_instrument:kind(),
                     unit            :: otel_instrument:unit() | undefined,
                     temporality     :: otel_instrument:temporality(),
                     callback        :: otel_instrument:callback() | undefined,
                     callback_args   :: otel_instrument:callback_args() | undefined,
                     advisory_params :: otel_instrument:advisory_params() | undefined}).

-define(TEMPORALITY_DELTA, temporality_delta).
-define(TEMPORALITY_CUMULATIVE, temporality_cumulative).

-define(KIND_COUNTER, counter).
-define(KIND_OBSERVABLE_COUNTER, observable_counter).
-define(KIND_HISTOGRAM, histogram).
-define(KIND_OBSERVABLE_GAUGE, observable_gauge).
-define(KIND_UPDOWN_COUNTER, updown_counter).
-define(KIND_OBSERVABLE_UPDOWNCOUNTER, observable_updowncounter).
