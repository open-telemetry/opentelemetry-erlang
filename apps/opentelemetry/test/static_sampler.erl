-module(static_sampler).

-export([setup/1]).

-include("otel_sampler.hrl").

%% sampler returns the value from the Opts map based on the SpanName or `NOT_RECORD'
setup(Opts) ->
    otel_sampler:new(
        fun(_, _, _, SpanName, _, _, Opts1) -> {maps:get(SpanName, Opts1, ?DROP), [], []} end,
        <<"StaticSampler">>,
        Opts
    ).
