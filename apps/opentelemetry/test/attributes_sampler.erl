%% sampler returns `DROP' if a span attribute matches the configured attribute map
%% otherwise `RECORD_AND_SAMPLE'
-module(attributes_sampler).

-behavior(otel_sampler).

-export([description/1,
         setup/1,
         should_sample/7]).

-include("otel_sampler.hrl").

setup(Attributes) when is_map(Attributes) ->
    Attributes;
setup(_) ->
    #{}.

description(_) ->
    <<"AttributesSampler">>.

-spec should_sample(Ctx, TraceId, Links, SpanName, SpanKind, Attributes, Config) -> Result
              when Ctx :: otel_ctx:t(),
                   TraceId :: opentelemetry:trace_id(),
                   Links :: otel_links:t(),
                   SpanName :: opentelemetry:span_name(),
                   SpanKind :: nopentelemetry:span_kind(),
                   Attributes :: opentelemetry:attributes_map(),
                   Config :: otel_sampler:sampler_config(),
                   Result :: otel_sampler:sampling_result().
should_sample(_Ctx, _TraceId, _Links, _SpanName, _SpanKind, Attributes, ConfigAttributes) ->
    case has_match(Attributes, ConfigAttributes) of
        true ->
            {?DROP, [], []};
        _ ->
            {?RECORD_AND_SAMPLE, [], []}
    end.

has_match(A, B) ->
    {Min, Max} = if
                     map_size(A) < map_size(B) ->
                         {A, B};
                     true ->
                         {B, A}
                 end,
    I = maps:iterator(Min),
    has_match_(maps:next(I), Max).

has_match_(none, _) ->
    false;
has_match_({K, V, _I}, B) when map_get(K, B) =:= V ->
    true;
has_match_({_K, _V, I}, B) ->
    has_match_(maps:next(I), B).
