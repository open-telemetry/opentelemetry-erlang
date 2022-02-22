%% macros for tracing
%% tracers for applications are automatically created on boot

-define(current_tracer, opentelemetry:get_application_tracer(?MODULE)).
-define(current_span_ctx, otel_tracer:current_span_ctx()).

-define(start_span(SpanName),
        otel_tracer:start_span(?current_tracer, SpanName, #{})).

-define(start_span(SpanName, StartOpts),
        otel_tracer:start_span(?current_tracer, SpanName, StartOpts)).

-define(with_span(SpanName, Fun),
        otel_tracer:with_span(?current_tracer, SpanName, #{}, Fun)).

-define(with_span(SpanName, StartOpts, Fun),
        otel_tracer:with_span(?current_tracer, SpanName, StartOpts, Fun)).

-define(set_current_span(SpanCtx),
        otel_tracer:set_current_span(SpanCtx)).

%% updates the current context with the updated span context that
%% has `is_recording' set to `false'
-define(end_span(),
        ?end_span(undefined)).

-define(end_span(Timestamp),
        otel_tracer:set_current_span(otel_span:end_span(?current_span_ctx, Timestamp))).

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

-define(set_status(Code, Message),
        otel_span:set_status(?current_span_ctx, Code, Message)).

-define(set_status(StatusOrCode),
        otel_span:set_status(?current_span_ctx, StatusOrCode)).

-define(update_name(Name),
        otel_span:update_name(?current_span_ctx, Name)).
