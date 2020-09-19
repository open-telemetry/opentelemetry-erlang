%% This record is for the data of a named tracer.
%% The `tracer' contains the modules to use for span creation,
%% the sampler to use at creation time and the processors to
%% run `OnStart' and `OnEnd'.
%% See OpenTelemetry Tracer spec: https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/sdk-tracing.md#tracer-creation
-record(tracer,
        {
         module                        :: module(),
         span_module                   :: module(),
         ctx_module                    :: module(),
         on_start_processors           :: fun((opentelemetry:span()) -> opentelemetry:span()),
         on_end_processors             :: fun((opentelemetry:span()) -> boolean() | {error, term()}),
         sampler                       :: ot_sampler:t(),
         instrumentation_library       :: ot_tracer_server:instrumentation_library() | undefined,
         telemetry_library             :: ot_tracer_server:telemetry_library() | undefined,
         resource                      :: ot_resource:t() | undefined
        }).
-type tracer() :: #tracer{}.

%% Tracer Context is a recursive record to store the currently
%% active Span Context and the previous Tracer Context so when
%% the currenly active Span is ended the previous Tracer Context
%% becomes active again.
-record(tracer_ctx,
        {
         %% the currently active span ctx
         active :: opentelemetry:span_ctx() | undefined,
         %% the tracer_ctx at the time the active span ctx
         %% was made the active span ctx
         previous :: #tracer_ctx{} | undefined
        }).
-type tracer_ctx() :: #tracer_ctx{}.
