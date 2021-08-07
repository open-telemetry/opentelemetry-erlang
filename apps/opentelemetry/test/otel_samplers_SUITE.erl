-module(otel_samplers_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_sampler.hrl").

all() ->
    [trace_id_ratio_based, parent_based, get_description, custom_sampler_module].

init_per_suite(Config) ->
    application:load(opentelemetry),
    %% set application environment variables
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(opentelemetry).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

get_description(_Config) ->
    Probability = 0.5,
    Sampler = otel_sampler:new({otel_sampler_trace_id_ratio_based, Probability}),

    ?assertEqual(<<"TraceIdRatioBased{0.500000}">>, otel_sampler:description(Sampler)),

    ParentBasedSampler = otel_sampler:new(
        {otel_sampler_parent_based, #{
            root => {otel_sampler_trace_id_ratio_based, Probability}
        }}
    ),
    ?assertEqual(
        <<"ParentBased{root:TraceIdRatioBased{0.500000},remoteParentSampled:AlwaysOnSampler,remoteParentNotSampled:AlwaysOffSampler,localParentSampled:AlwaysOnSampler,localParentNotSampled:AlwaysOffSampler}">>,
        otel_sampler:description(ParentBasedSampler)
    ),

    ok.

trace_id_ratio_based(_Config) ->
    SpanName = <<"span-prob-sampled">>,
    Probability = 0.5,
    DoSample = 120647249294066572380176333851662846319,
    DoNotSample = 53020601517903903921384168845238205400,

    Ctx = otel_ctx:new(),

    %% sampler that runs on all spans
    {Sampler, _, Opts} = otel_sampler:new({otel_sampler_trace_id_ratio_based, Probability}),

    %% checks the trace id is under the upper bound
    ?assertMatch(
        {?RECORD_AND_SAMPLE, [], []},
        Sampler:should_sample(
            otel_tracer:set_current_span(Ctx, undefined),
            DoSample,
            [],
            SpanName,
            undefined,
            [],
            Opts
        )
    ),

    %% checks the trace id is is over the upper bound
    ?assertMatch(
        {?DROP, [], []},
        Sampler:should_sample(
            otel_tracer:set_current_span(Ctx, undefined),
            DoNotSample,
            [],
            SpanName,
            undefined,
            [],
            Opts
        )
    ),

    %% ignores the parent span context trace flags
    ?assertMatch(
        {?DROP, [], []},
        Sampler:should_sample(
            otel_tracer:set_current_span(Ctx, #span_ctx{
                trace_flags = 1,
                is_remote = true
            }),
            DoNotSample,
            [],
            SpanName,
            undefined,
            [],
            Opts
        )
    ),

    %% ignores the parent span context trace flags
    ?assertMatch(
        {?RECORD_AND_SAMPLE, [], []},
        Sampler:should_sample(
            otel_tracer:set_current_span(Ctx, #span_ctx{
                trace_flags = 0,
                is_remote = false
            }),
            DoSample,
            [],
            SpanName,
            undefined,
            [],
            Opts
        )
    ),

    %% trace id is under the upper bound
    ?assertMatch(
        {?RECORD_AND_SAMPLE, [], []},
        Sampler:should_sample(
            otel_tracer:set_current_span(Ctx, #span_ctx{
                trace_flags = 0,
                is_remote = true
            }),
            DoSample,
            [],
            SpanName,
            undefined,
            [],
            Opts
        )
    ),

    ok.

parent_based(_Config) ->
    SpanName = <<"span-prob-sampled">>,
    Probability = 0.5,
    DoSample = 120647249294066572380176333851662846319,
    DoNotSample = 53020601517903903921384168845238205400,

    Ctx = otel_ctx:new(),

    {Sampler, _, Opts} = otel_sampler:new(
        {otel_sampler_parent_based, #{root => {otel_sampler_trace_id_ratio_based, Probability}}}
    ),

    %% with no parent it will run the probability sampler
    ?assertMatch(
        {?RECORD_AND_SAMPLE, [], []},
        Sampler:should_sample(
            otel_tracer:set_current_span(Ctx, undefined),
            DoSample,
            [],
            SpanName,
            undefined,
            [],
            Opts
        )
    ),
    ?assertMatch(
        {?DROP, [], []},
        Sampler:should_sample(
            otel_tracer:set_current_span(Ctx, undefined),
            DoNotSample,
            [],
            SpanName,
            undefined,
            [],
            Opts
        )
    ),

    %% with parent it will use the parents value
    ?assertMatch(
        {?RECORD_AND_SAMPLE, [], []},
        Sampler:should_sample(
            otel_tracer:set_current_span(Ctx, #span_ctx{
                trace_flags = 1,
                is_remote = true
            }),
            DoNotSample,
            [],
            SpanName,
            undefined,
            [],
            Opts
        )
    ),
    ?assertMatch(
        {?DROP, [], []},
        Sampler:should_sample(
            otel_tracer:set_current_span(Ctx, #span_ctx{
                trace_flags = 0,
                is_remote = true
            }),
            DoNotSample,
            [],
            SpanName,
            undefined,
            [],
            Opts
        )
    ),

    %% with no root sampler in setup opts the default sampler always_on is used
    {DefaultParentOrElse, _, Opts1} = otel_sampler:new({otel_sampler_parent_based, #{}}),

    ?assertMatch(
        {?RECORD_AND_SAMPLE, [], []},
        DefaultParentOrElse:should_sample(
            otel_tracer:set_current_span(Ctx, undefined),
            DoSample,
            [],
            SpanName,
            undefined,
            [],
            Opts1
        )
    ),
    ?assertMatch(
        {?RECORD_AND_SAMPLE, [], []},
        DefaultParentOrElse:should_sample(
            otel_tracer:set_current_span(Ctx, undefined),
            DoNotSample,
            [],
            SpanName,
            undefined,
            [],
            Opts1
        )
    ),

    ?assertMatch(
        {?RECORD_AND_SAMPLE, [], []},
        DefaultParentOrElse:should_sample(
            otel_tracer:set_current_span(Ctx, #span_ctx{trace_flags = 1}),
            DoNotSample,
            [],
            SpanName,
            undefined,
            [],
            Opts1
        )
    ),
    ?assertMatch(
        {?DROP, [], []},
        DefaultParentOrElse:should_sample(
            otel_tracer:set_current_span(Ctx, #span_ctx{
                trace_flags = 0,
                is_remote = true
            }),
            DoNotSample,
            [],
            SpanName,
            undefined,
            [],
            Opts1
        )
    ),

    ok.

custom_sampler_module(_Config) ->
    SpanName = <<"span-name">>,
    {Sampler, _, Opts} = otel_sampler:new({static_sampler, #{SpanName => ?DROP}}),
    ?assertMatch(
        {?DROP, [], []},
        Sampler:should_sample(
            otel_ctx:new(),
            opentelemetry:generate_trace_id(),
            [],
            SpanName,
            undefined,
            [],
            Opts
        )
    ),
    ok.

should_sample(_Config) ->
    Sampler = otel_sampler:new({otel_sampler_always_on, #{}}),
    ?assertMatch(
        {?RECORD_AND_SAMPLE, [], []},
        otel_samplers:should_sample(
            Sampler,
            otel_ctx:new(),
            opentelemetry:generate_trace_id(),
            [],
            <<"span-name">>,
            undefined,
            []
        )
    ),
    ok.
