-define(GLOBAL_METER_PROVIDER_NAME, global).
-define(GLOBAL_METER_PROVIDER_REG_NAME, otel_meter_provider_global).

%% macros for metrics
%% Meters for applications are automatically created on boot

-define(current_meter, opentelemetry_experimental:get_meter(
                         opentelemetry:get_application_scope(?MODULE))).

-define(create_counter(Name, Opts),
        otel_meter:create_counter(?current_meter, Name, Opts)).

-define(create_observable_counter(Name, Callback, CallbackArgs, Opts),
        otel_meter:create_observable_counter(?current_meter, Name, Callback, CallbackArgs, Opts)).

-define(create_histogram(Name, Opts),
        otel_meter:create_histogram(?current_meter, Name, Opts)).

-define(create_observable_gauge(Name, Callback, CallbackArgs, Opts),
        otel_meter:create_observable_gauge(?current_meter, Name, Callback, CallbackArgs, Opts)).

-define(create_updown_counter(Name, Opts),
        otel_meter:create_updown_counter(?current_meter, Name, Opts)).

-define(create_observable_updowncounter(Name, Callback, CallbackArgs, Opts),
        otel_meter:create_observable_updowncounter(?current_meter, Name, Callback, CallbackArgs, Opts)).

-define(counter_add(Name, Number),
        otel_counter:add(?current_meter, Name, Number)).

-define(counter_add(Name, Number, Attributes),
        otel_counter:add(?current_meter, Name, Number, Attributes)).

-define(updown_counter_add(Name, Number),
        otel_updown_counter:add(?current_meter, Name, Number)).

-define(updown_counter_add(Name, Number, Attributes),
        otel_updown_counter:add(?current_meter, Name, Number, Attributes)).

-define(histogram_record(Name, Number),
        otel_histogram:record(?current_meter, Name, Number)).

-define(histogram_record(Name, Number, Attributes),
        otel_histogram:record(?current_meter, Name, Number, Attributes)).

-define(lookup_instrument(Name),
        otel_meter:lookup_instrument(?current_meter, Name)).
