
-record(tracer, {module :: module(),
                 span_module :: module(),
                 ctx_module :: module(),
                 on_start_processors :: fun((opentelemetry:span()) -> opentelemetry:span()),
                 on_end_processors :: fun((opentelemetry:span()) -> boolean() | {error, term()}),
                 sampler :: ot_sampler:t(),
                 library_resource :: ot_tracer_server:library_resource() | undefined,
                 resource :: ot_resource:t() | undefined}).
-type tracer() :: #tracer{}.

-record(tracer_ctx, {
                     %% the currently active span ctx
                     active :: opentelemetry:span_ctx() | undefined,
                     %% the tracer_ctx at the time the active span ctx
                     %% was made the active span ctx
                     previous :: #tracer_ctx{} | undefined
                    }).
-type tracer_ctx() :: #tracer_ctx{}.
