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
    case maps:intersect(Attributes, ConfigAttributes) of
        Map when map_size(Map) > 0 ->
            {?DROP, [], []};
        _ ->
            {?RECORD_AND_SAMPLE, [], []}
    end.
