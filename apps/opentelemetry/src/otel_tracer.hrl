%% This record is for the data of a named tracer.
%% The `tracer' contains the modules to use for span creation,
%% the sampler to use at creation time and the processors to
%% run `OnStart' and `OnEnd'.
%% See OpenTelemetry Tracer spec: https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/sdk.md#tracer-creation
-record(tracer,
        {
         module                  :: module(),
         on_start_processors     :: fun((otel_ctx:t(), opentelemetry:span()) -> opentelemetry:span()),
         on_end_processors       :: fun((opentelemetry:span()) -> boolean() | {error, term()}),
         sampler                 :: otel_sampler:t(),
         id_generator            :: otel_id_generator:t(),
         instrumentation_library :: otel_tracer_server:instrumentation_library() | undefined,
         telemetry_library       :: otel_tracer_server:telemetry_library() | undefined,
         resource                :: otel_resource:t() | undefined
        }).
-type tracer() :: #tracer{}.
