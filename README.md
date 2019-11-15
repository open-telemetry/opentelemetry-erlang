# Erlang OpenTelemetry API

This is the API portion of [OpenTelemetry](https://opentelemetry.io/) for Erlang and Elixir applications.

This is a library, it does not start any processes, and should be the only OpenTelemetry dependency of Erlang/Elixir applications. 

The end user of your application can then choose to either include the [OpenTelemetry implementation](https://github.com/open-telemetry/opentelemetry-erlang) application. If the other application is not in the final release the OpenTelemetry instrumentation will all be noops. This means no processes started, no ETS tables created and nothing added to the process dictionary.

This separation is done so you should feel comfortable instrumenting your Erlang/Elixir application with OpenTelemetry and not worry that a complicated dependency is being forced on your users.


