-include_lib("match_spec.hrl").

-record(instrument, {module          :: match_spec(module()),
                     meter           :: match_spec(otel_meter:t()),
                     name            :: match_spec(otel_instrument:name()),
                     description     :: match_spec(otel_instrument:description()) | undefined,
                     kind            :: match_spec(otel_instrument:kind()),
                     unit            :: match_spec(otel_instrument:unit()) | undefined,
                     temporality     :: match_spec(otel_instrument:temporality()),
                     callback        :: match_spec(otel_instrument:callback()) | undefined,
                     callback_args   :: match_spec(otel_instrument:callback_args()) | undefined,
                     advisory_params :: match_spec(otel_instrument:advisory_params() | undefined)}).

-define(TEMPORALITY_DELTA, temporality_delta).
-define(TEMPORALITY_CUMULATIVE, temporality_cumulative).

-define(KIND_COUNTER, counter).
-define(KIND_OBSERVABLE_COUNTER, observable_counter).
-define(KIND_HISTOGRAM, histogram).
-define(KIND_OBSERVABLE_GAUGE, observable_gauge).
-define(KIND_UPDOWN_COUNTER, updown_counter).
-define(KIND_OBSERVABLE_UPDOWNCOUNTER, observable_updowncounter).
