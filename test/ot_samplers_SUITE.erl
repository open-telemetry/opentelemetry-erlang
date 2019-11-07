-module(ot_samplers_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_sampler.hrl").

all() ->
    [probability_sampler].

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

probability_sampler(_Config) ->
    SpanName = <<"span-prob-sampled">>,
    Probability = 0.5,
    DoSample = 120647249294066572380176333851662846319,
    DoNotSample =  53020601517903903921384168845238205400,

    %% sampler that runs on all spans
    Sampler = ot_sampler:setup(probability, #{probability => Probability,
                                              only_root_spans => false}),

    %% checks the trace id is is under the upper bound
    ?assertMatch({?RECORD_AND_PROPAGATE, []},
            Sampler(DoSample, 0, undefined, undefined, [], SpanName, undefined, [])),

    %% checks the trace id is is over the upper bound
    ?assertMatch({?NOT_RECORD, []},
            Sampler(DoNotSample, 0, undefined, undefined, [], SpanName, undefined, [])),

    %% uses the value from the parent span context
    ?assertMatch({?RECORD_AND_PROPAGATE, []},
                 Sampler(DoNotSample, 0, #span_ctx{trace_flags=1,
                                                   is_remote=true},
                         ?NOT_RECORD, [], SpanName, undefined, [])),

    %% since parent is not remote it uses the value from the parent span context
    ?assertMatch({?NOT_RECORD, []},
                 Sampler(DoSample, 0, #span_ctx{trace_flags=0,
                                                is_remote=false},
                         ?NOT_RECORD, [], SpanName, undefined, [])),

    %% since parent is remote it checks the trace id and it is under the upper bound
    ?assertMatch({?RECORD_AND_PROPAGATE, []},
                 Sampler(DoSample, 0, #span_ctx{trace_flags=0,
                                                is_remote=true},
                         undefined, [], SpanName, undefined, [])),

    %% relies on the sampler hint
    ?assertMatch({?RECORD_AND_PROPAGATE, []},
                 Sampler(DoNotSample, 0, #span_ctx{trace_flags=0},
                         ?RECORD_AND_PROPAGATE, [], SpanName, undefined, [])),

    Sampler1 = ot_sampler:setup(probability, #{probability => Probability,
                                               only_root_spans => false,
                                               ignore_hints => [],
                                               ignore_parent_flag => false}),

    %% relies on the sampler hint
    ?assertMatch({?RECORD, []},
                 Sampler1(DoNotSample, 0, #span_ctx{trace_flags=0, is_remote=true},
                         ?RECORD, [], SpanName, undefined, [])),

    %% a sampler that does not ignore RECORD like it does by default but does ignore the parent
    SamplerWithAllHints = ot_sampler:setup(probability, #{probability => Probability,
                                                          ignore_hints => [],
                                                          ignore_parent_flag => true}),

    %% relies on the sampler hint
    ?assertMatch({?RECORD, []},
                 SamplerWithAllHints(DoNotSample, 0, undefined, ?RECORD, [], SpanName, undefined, [])),

    %% relies on the sampler hint
    ?assertMatch({?NOT_RECORD, []},
                 SamplerWithAllHints(DoNotSample, 0, undefined, ?NOT_RECORD, [], SpanName, undefined, [])),

    %% since parents are ignored this relies on the sampler hint
    ?assertMatch({?NOT_RECORD, []},
                 SamplerWithAllHints(DoNotSample, 0, #span_ctx{trace_flags=1},
                                     ?NOT_RECORD, [], SpanName, undefined, [])),

    Sampler2 = ot_sampler:setup(probability, #{probability => Probability,
                                               ignore_parent_flag => false}),

    %% parent not ignored but is 0 and sampler hint RECORD is ignored by default
    ?assertMatch({?NOT_RECORD, []},
                 Sampler2(DoNotSample, 0, #span_ctx{trace_flags=0, is_remote=true},
                          ?RECORD, [], SpanName, undefined, [])),

    ok.
