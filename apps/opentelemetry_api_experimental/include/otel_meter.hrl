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

-define(create_observable_counter(Name, Opts),
        otel_meter:create_observable_counter(?current_meter, Name, Opts)).

-define(create_histogram(Name, Opts),
        otel_meter:create_histogram(?current_meter, Name, Opts)).

-define(create_observable_gauge(Name, Callback, CallbackArgs, Opts),
        otel_meter:create_observable_gauge(?current_meter, Name, Callback, CallbackArgs, Opts)).

-define(create_observable_gauge(Name, Opts),
        otel_meter:create_observable_gauge(?current_meter, Name, Opts)).

-define(create_updown_counter(Name, Opts),
        otel_meter:create_updown_counter(?current_meter, Name, Opts)).

-define(create_observable_updowncounter(Name, Callback, CallbackArgs, Opts),
        otel_meter:create_observable_updowncounter(?current_meter, Name, Callback, CallbackArgs, Opts)).

-define(create_observable_updowncounter(Name, Opts),
        otel_meter:create_observable_updowncounter(?current_meter, Name, Opts)).

%% To support only using an atom name of an instrument the macro must support looking
%% up the meter to use to resolve the instrument name. But if an Instrument record is
%% given then the meter lookup can be skipped.

-define(counter_add(Name, Number, Attributes),
        otel_counter:add(otel_ctx:get_current(), ?current_meter, Name, Number, Attributes)).

-define(updown_counter_add(Name, Number, Attributes),
        otel_updown_counter:add(otel_ctx:get_current(), ?current_meter, Name, Number, Attributes)).

-define(histogram_record(Name, Number, Attributes),
        otel_histogram:record(otel_ctx:get_current(), ?current_meter, Name, Number, Attributes)).

-define(register_callback(Instruments, Callback, CallbackArgs),
        otel_meter:register_callback(?current_meter, Instruments, Callback, CallbackArgs)).
