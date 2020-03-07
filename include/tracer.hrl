%% macros for tracing
%% register a tracer for an application with opentelemetry:register_application_tracer(AppName)

-define(current_tracer, opentelemetry:get_tracer(?MODULE)).
-define(current_span_ctx, ot_tracer:current_span_ctx(?current_tracer)).

-define(start_span(SpanName),
        ot_tracer:start_span(?current_tracer, SpanName, #{})).

-define(start_span(SpanName, StartOpts),
        ot_tracer:start_span(?current_tracer, SpanName, StartOpts)).

-define(set_span(SpanCtx),
        ot_tracer:set_span(?current_tracer, SpanCtx)).

-define(with_span(SpanName, StartOpts, Fun),
        ot_tracer:with_span(?current_tracer, SpanName, StartOpts, Fun)).

-define(end_span(),
        ot_tracer:end_span(?current_tracer)).

-define(current_span_ctx(),
        ot_tracer:current_span_ctx(?current_tracer)).

-define(is_recording(),
        ot_span:is_recording(?current_tracer, ?current_span_ctx)).

-define(set_attribute(Key, Value),
        ot_span:set_attribute(?current_tracer, ?current_span_ctx, Key, Value)).

-define(set_attributes(Attributes),
        ot_span:set_attributes(?current_tracer, ?current_span_ctx, Attributes)).

-define(add_event(Event),
        ot_span:add_event(?current_tracer, ?current_span_ctx, Event)).

-define(add_events(Events),
        ot_span:add_events(?current_tracer, ?current_span_ctx, Events)).

-define(add_links(Links),
        ot_span:add_links(?current_tracer, ?current_span_ctx, Links)).

-define(set_status(Status),
        ot_span:set_status(?current_tracer, ?current_span_ctx, Status)).

-define(update_name(Name),
        ot_span:update_name(?current_tracer, ?current_span_ctx, Name)).
