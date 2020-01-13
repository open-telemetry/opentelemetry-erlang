-define(SPAN_CTX, {ot_tracer_default, span_ctx}).
-define(CTX_IMPL_KEY, {ot_tracer_default, ctx}).

-define(ctx, (persistent_term:get(?CTX_IMPL_KEY))).

-record(tracer, {name :: unicode:unicode_binary() | undefined,
                 module :: module(),
                 span_module :: module(),
                 ctx_module :: module(),
                 on_start_processors :: fun((opentelemetry:span()) -> opentelemetry:span()),
                 on_end_processors :: fun((opentelemetry:span()) -> boolean() | {error, term()}),
                 sampler :: ot_sampler:sampler(),
                 resource :: term() | undefined}).

-record(tracer_ctx, {active :: opentelemetry:span_ctx() | undefined,
                     parent :: #tracer_ctx{} | undefined}).
