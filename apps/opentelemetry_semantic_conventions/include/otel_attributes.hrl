

%% The name of the instrumentation scope - (`InstrumentationScope.Name` in OTLP).
-define(OTEL_SCOPE_NAME, 'otel.scope.name').


%% The version of the instrumentation scope - (`InstrumentationScope.Version` in OTLP).
-define(OTEL_SCOPE_VERSION, 'otel.scope.version').


%% Name of the code, either "OK" or "ERROR". MUST NOT be set if the status code is UNSET.

-define('otel_statuscode.ok', 'OK').

-define('otel_statuscode.error', 'ERROR').

-define(otel_statuscode(Custom), Custom).


%% Description of the Status if it has a value, otherwise not set.
-define(OTEL_STATUSDESCRIPTION, 'otel.status_description').
