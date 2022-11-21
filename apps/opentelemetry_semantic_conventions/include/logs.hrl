%% The schema url for telemetry resources
-define(LOGS_SCHEMA_URL, <<"https://opentelemetry.io/schemas/1.13.0">>).

%% The name identifies the event
-define(EVENT_NAME, 'event.name').

%% The domain identifies the context in which an event happened. An event name is unique only within a domain
%% An `event.name` is supposed to be unique only in the context of an
%% `event.domain`, so this allows for two events in different domains to
%% have same `event.name`, yet be unrelated events
-define(EVENT_DOMAIN, 'event.domain').
