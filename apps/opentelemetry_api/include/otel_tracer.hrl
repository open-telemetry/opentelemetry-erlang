%% macros for tracing
%% register a tracer for an application with opentelemetry:register_application_tracer(AppName)

-define(current_tracer, opentelemetry:get_tracer(?MODULE)).
-define(current_span_ctx, otel_tracer:current_span_ctx()).

-define(start_span(SpanName),
        otel_tracer:start_span(?current_tracer, SpanName, #{})).

-define(start_span(SpanName, StartOpts),
        otel_tracer:start_span(?current_tracer, SpanName, StartOpts)).

-define(with_span(SpanName, StartOpts, Fun),
        otel_tracer:with_span(?current_tracer, SpanName, StartOpts, Fun)).

-define(end_span(),
        otel_tracer:end_span(?current_tracer, ?current_span_ctx)).

-define(end_span(SpanCtx),
        otel_tracer:end_span(?current_tracer, SpanCtx)).

-define(set_current_span(SpanCtx),
        otel_tracer:set_current_span(SpanCtx)).

-define(is_recording(),
        otel_span:is_recording(?current_tracer, ?current_span_ctx)).

-define(set_attribute(Key, Value),
        otel_tracer:set_attribute(?current_tracer, ?current_span_ctx, Key, Value)).

-define(set_attributes(Attributes),
        otel_tracer:set_attributes(?current_tracer, ?current_span_ctx, Attributes)).

-define(add_event(Event),
        otel_tracer:add_event(?current_tracer, ?current_span_ctx, Event)).

-define(add_events(Events),
        otel_tracer:add_events(?current_tracer, ?current_span_ctx, Events)).

-define(add_links(Links),
        otel_tracer:add_links(?current_tracer, ?current_span_ctx, Links)).

-define(set_status(Status),
        otel_tracer:set_status(?current_tracer, ?current_span_ctx, Status)).

-define(update_name(Name),
        otel_tracer:update_name(?current_tracer, ?current_span_ctx, Name)).
