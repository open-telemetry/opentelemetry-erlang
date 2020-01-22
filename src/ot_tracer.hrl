
-record(tracer, {module :: module(),
                 span_module :: module(),
                 ctx_module :: module(),
                 on_start_processors :: fun((opentelemetry:span()) -> opentelemetry:span()),
                 on_end_processors :: fun((opentelemetry:span()) -> boolean() | {error, term()}),
                 sampler :: ot_sampler:sampler(),
                 library_resource :: ot_tracer_server:library_resource() | undefined,
                 resource :: term() | undefined}).
-type tracer() :: #tracer{}.

-record(tracer_ctx, {active :: opentelemetry:span_ctx() | undefined,
                     parent :: #tracer_ctx{} | undefined}).
-type tracer_ctx() :: #tracer_ctx{}.
