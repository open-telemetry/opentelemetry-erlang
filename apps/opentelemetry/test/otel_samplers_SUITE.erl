-module(otel_samplers_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_sampler.hrl").

all() ->
    [trace_id_ratio_based, parent_based, get_description].

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
    Sampler = otel_sampler:setup(trace_id_ratio_based, Probability),

    ?assertEqual(<<"TraceIdRatioBased{0.500000}">>, otel_sampler:get_description(Sampler)),

    ok.

trace_id_ratio_based(_Config) ->
    SpanName = <<"span-prob-sampled">>,
    Probability = 0.5,
    DoSample = 120647249294066572380176333851662846319,
    DoNotSample =  53020601517903903921384168845238205400,

    %% sampler that runs on all spans
    {Sampler, _, Opts} = otel_sampler:setup(trace_id_ratio_based, Probability),

    %% checks the trace id is under the upper bound
    ?assertMatch({?RECORD_AND_SAMPLED, [], []},
                 Sampler(DoSample, undefined, [], SpanName, undefined, [], Opts)),

    %% checks the trace id is is over the upper bound
    ?assertMatch({?NOT_RECORD, [], []},
                 Sampler(DoNotSample, undefined, [], SpanName, undefined, [], Opts)),

    %% ignores the parent span context trace flags
    ?assertMatch({?NOT_RECORD, [], []},
                 Sampler(DoNotSample, #span_ctx{trace_flags=1,
                                                is_remote=true},
                         [], SpanName, undefined, [], Opts)),

    %% ignores the parent span context trace flags
    ?assertMatch({?RECORD_AND_SAMPLED, [], []},
                 Sampler(DoSample, #span_ctx{trace_flags=0,
                                             is_remote=false},
                         [], SpanName, undefined, [], Opts)),

    %% trace id is under the upper bound
    ?assertMatch({?RECORD_AND_SAMPLED, [], []},
                 Sampler(DoSample, #span_ctx{trace_flags=0,
                                             is_remote=true},
                         [], SpanName, undefined, [], Opts)),

    ok.

parent_based(_Config) ->
    SpanName = <<"span-prob-sampled">>,
    Probability = 0.5,
    DoSample = 120647249294066572380176333851662846319,
    DoNotSample =  53020601517903903921384168845238205400,

    {Sampler, _, Opts} = otel_sampler:setup(parent_based,
                                            #{root => {trace_id_ratio_based, Probability}}),

    %% with no parent it will run the probability sampler
    ?assertMatch({?RECORD_AND_SAMPLED, [], []},
                 Sampler(DoSample, undefined, [], SpanName, undefined, [], Opts)),
    ?assertMatch({?NOT_RECORD, [], []},
                 Sampler(DoNotSample, undefined, [], SpanName, undefined, [], Opts)),

    %% with parent it will use the parents value
    ?assertMatch({?RECORD_AND_SAMPLED, [], []},
                 Sampler(DoNotSample, #span_ctx{trace_flags=1,
                                                is_remote=true},
                         [], SpanName, undefined, [], Opts)),
    ?assertMatch({?NOT_RECORD, [], []},
                 Sampler(DoNotSample, #span_ctx{trace_flags=0,
                                                is_remote=true},
                         [], SpanName, undefined, [], Opts)),

    %% with no root sampler in setup opts the default sampler always_on is used
    {DefaultParentOrElse, _, Opts1} = otel_sampler:setup(parent_based, #{}),

    ?assertMatch({?RECORD_AND_SAMPLED, [], []},
                 DefaultParentOrElse(DoSample, undefined, [], SpanName, undefined, [], Opts1)),
    ?assertMatch({?RECORD_AND_SAMPLED, [], []},
                 DefaultParentOrElse(DoNotSample, undefined, [], SpanName, undefined, [], Opts1)),

    ?assertMatch({?RECORD_AND_SAMPLED, [], []},
                 DefaultParentOrElse(DoNotSample, #span_ctx{trace_flags=1},
                                     [], SpanName, undefined, [], Opts1)),
    ?assertMatch({?NOT_RECORD, [], []},
                 DefaultParentOrElse(DoNotSample, #span_ctx{trace_flags=0,
                                                            is_remote=true},
                                     [], SpanName, undefined, [], Opts1)),

    ok.
