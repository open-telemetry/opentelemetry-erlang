-define(GLOBAL_METER_PROVIDER_NAME, global).
-define(GLOBAL_METER_PROVIDER_REG_NAME, otel_meter_provider_global).

%% macros for metrics
%% Meters for applications are automatically created on boot

-define(current_meter, opentelemetry_experimental:get_meter(
                         opentelemetry:get_application_scope(?MODULE))).

-define(create_counter(Name, Opts),
        otel_meter:create_counter(?current_meter, Name, Opts)).

-define(register_instrument(Alias, Instrument),
        otel_instrument:register_alias(Alias, Instrument)).

-define(unregister_instrument(Alias),
        otel_instrument:unregister_alias(Alias)).

-define(register_counter(Alias, Name, Opts),
        ?register_instrument(Alias, ?create_counter(Name, Opts))).

-define(create_observable_counter(Name, Callback, CallbackArgs, Opts),
        otel_meter:create_observable_counter(?current_meter, Name, Callback, CallbackArgs, Opts)).

-define(create_observable_counter(Name, Opts),
        otel_meter:create_observable_counter(?current_meter, Name, Opts)).

-define(create_histogram(Name, Opts),
        otel_meter:create_histogram(?current_meter, Name, Opts)).

-define(register_histogram(Alias, Name, Opts),
        ?register_instrument(Alias, ?create_histogram(Name, Opts))).

-define(create_observable_gauge(Name, Callback, CallbackArgs, Opts),
        otel_meter:create_observable_gauge(?current_meter, Name, Callback, CallbackArgs, Opts)).

-define(create_observable_gauge(Name, Opts),
        otel_meter:create_observable_gauge(?current_meter, Name, Opts)).

-define(create_updown_counter(Name, Opts),
        otel_meter:create_updown_counter(?current_meter, Name, Opts)).

-define(register_updown_counter(Alias, Name, Opts),
        ?register_instrument(Alias, ?create_updown_counter(Name, Opts))).

-define(create_observable_updowncounter(Name, Callback, CallbackArgs, Opts),
        otel_meter:create_observable_updowncounter(?current_meter, Name, Callback, CallbackArgs, Opts)).

-define(create_observable_updowncounter(Name, Opts),
        otel_meter:create_observable_updowncounter(?current_meter, Name, Opts)).

%% A recording target is either the Instrument returned at creation time or a
%% node-local atom explicitly registered with one of the register_* macros.
%% The alias resolves directly to the Instrument, including its owning Meter.

-define(counter_add(Target, Number, Attributes),
        otel_counter:add(otel_ctx:get_current(), Target, Number, Attributes)).

-define(updown_counter_add(Target, Number, Attributes),
        otel_updown_counter:add(otel_ctx:get_current(), Target, Number, Attributes)).

-define(histogram_record(Target, Number, Attributes),
        otel_histogram:record(otel_ctx:get_current(), Target, Number, Attributes)).

-define(register_callback(Instruments, Callback, CallbackArgs),
        otel_meter:register_callback(?current_meter, Instruments, Callback, CallbackArgs)).
