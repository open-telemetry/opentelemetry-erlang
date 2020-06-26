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
    {Sampler, Opts} = ot_sampler:setup(probability, #{probability => Probability,
                                                      only_root_spans => false}),

    %% checks the trace id is is under the upper bound
    ?assertMatch({?RECORD_AND_PROPAGATE, []},
            Sampler(DoSample, undefined, [], SpanName, undefined, [], Opts)),

    %% checks the trace id is is over the upper bound
    ?assertMatch({?NOT_RECORD, []},
            Sampler(DoNotSample, undefined, [], SpanName, undefined, [], Opts)),

    %% uses the value from the parent span context
    ?assertMatch({?RECORD_AND_PROPAGATE, []},
                 Sampler(DoNotSample, #span_ctx{trace_flags=1,
                                                   is_remote=true},
                         [], SpanName, undefined, [], Opts)),

    %% since parent is not remote it uses the value from the parent span context
    ?assertMatch({?NOT_RECORD, []},
                 Sampler(DoSample, #span_ctx{trace_flags=0,
                                                is_remote=false},
                         [], SpanName, undefined, [], Opts)),

    %% since parent is remote it checks the trace id and it is under the upper bound
    ?assertMatch({?RECORD_AND_PROPAGATE, []},
                 Sampler(DoSample, #span_ctx{trace_flags=0,
                                                is_remote=true},
                         [], SpanName, undefined, [], Opts)),

    {Sampler1, Opts1} = ot_sampler:setup(probability, #{probability => Probability,
                                                        only_root_spans => false,
                                                        ignore_parent_flag => false}),

    ?assertMatch({?RECORD_AND_PROPAGATE, []},
                 Sampler1(DoSample, #span_ctx{trace_flags=0, is_remote=true},
                          [], SpanName, undefined, [], Opts1)),

    {Sampler2, Opts3} = ot_sampler:setup(probability, #{probability => Probability,
                                               ignore_parent_flag => false}),

    %% parent not ignored but is 0 and sampler hint RECORD is ignored by default
    ?assertMatch({?NOT_RECORD, []},
                 Sampler2(DoNotSample, #span_ctx{trace_flags=0, is_remote=true},
                          [], SpanName, undefined, [], Opts3)),

    ok.
