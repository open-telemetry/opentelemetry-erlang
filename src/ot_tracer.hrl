-define(SPAN_CTX, {ot_tracer_default, span_ctx}).
-define(CTX_IMPL_KEY, {ot_tracer_default, ctx}).
-define(SPAN_IMPL_KEY, {ot_tracer_default, span}).

-define(ctx, (persistent_term:get(?CTX_IMPL_KEY))).
-define(span, (persistent_term:get(?SPAN_IMPL_KEY))).

