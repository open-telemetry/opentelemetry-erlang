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

-define(set_current_span(SpanCtx),
        otel_tracer:set_current_span(SpanCtx)).

%% use tracer call to ensure the current span is overwritten with
%% an ended (`is_recording' set to `false') is put in the context
-define(end_span(),
        otel_tracer:end_span()).

-define(end_span(SpanCtx),
        otel_span:end_span(SpanCtx)).

-define(is_recording(),
        otel_span:is_recording(?current_span_ctx)).

-define(set_attribute(Key, Value),
        otel_span:set_attribute(?current_span_ctx, Key, Value)).

-define(set_attributes(Attributes),
        otel_span:set_attributes(?current_span_ctx, Attributes)).

-define(add_event(Name, Attributes),
        otel_span:add_event(?current_span_ctx, Name, Attributes)).

-define(add_events(Events),
        otel_span:add_events(?current_span_ctx, Events)).

-define(add_links(Links),
        otel_span:add_links(?current_span_ctx, Links)).

-define(set_status(Status),
        otel_span:set_status(?current_span_ctx, Status)).

-define(update_name(Name),
        otel_span:update_name(?current_span_ctx, Name)).
