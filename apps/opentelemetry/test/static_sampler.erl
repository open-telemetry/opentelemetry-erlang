-module(static_sampler).

-behavior(otel_sampler).

-export([description/1, setup/1, should_sample/7]).

-include("otel_sampler.hrl").

%% sampler returns the value from the Opts map based on the SpanName or `NOT_RECORD'
setup(Opts) -> Opts.

description(_) -> <<"StaticSampler">>.

should_sample(_, _, _, SpanName, _, _, Config) -> {maps:get(SpanName, Config, ?DROP), [], []}.
