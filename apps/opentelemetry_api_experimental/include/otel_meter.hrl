-define(GLOBAL_METER_PROVIDER_NAME, global).
-define(GLOBAL_METER_PROVIDER_REG_NAME, otel_meter_provider_global).

%% macros for metrics
%% Meters for applications are automatically created on boot

-define(current_meter, opentelemetry_experimental:get_meter(?MODULE)).

