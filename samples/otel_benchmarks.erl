-module(otel_benchmarks).

-export([run/0]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% doesn't currently show anything useful
run() ->
    PDictCtxFun = fun() ->
                          ?start_span(a_span),
                          ?end_span()
                  end,

    CtxFun = fun() ->
                     Tracer = opentelemetry:get_application_tracer(?MODULE),
                     SpanCtx = otel_tracer:start_span(Tracer, a_span, #{}),
                     otel_span:end_span(SpanCtx)
             end,

    benchee:run(#{<<"pdict_ctx">> => PDictCtxFun,
                  <<"ctx">> => CtxFun}),
    ok.
