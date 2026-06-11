-module(otel_aggregation).

-export([maybe_init_aggregate/6,
         default_mapping/0,
         default_temporality_mapping/0]).

-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_view.hrl").

%% -type t() :: drop | sum | last_value | histogram.
-type t() :: otel_aggregation_drop:t() | otel_aggregation_sum:t() |
             otel_aggregation_last_value:t() | otel_aggregation_histogram_explicit:t().

-type key() :: {atom(), opentelemetry:attributes_map(), reference(), number()}.

-type options() :: map().

-export_type([t/0,
              key/0,
              options/0]).

%% Returns the aggregation's record as it is seen and updated by
%% the aggregation module in the metrics table.
-callback init(Stream, Attributes) -> Aggregation when
      Stream :: #stream{},
      Attributes :: opentelemetry:attributes_map(),
      Aggregation :: t().

-callback aggregate(Ctx, Table, ExemplarTable, Stream, Value, Attributes, DroppedAttributes) -> boolean() when
      Ctx :: otel_ctx:t(),
      Table :: ets:table(),
      ExemplarTable :: ets:table(),
      Stream :: #stream{},
      Value :: number(),
      Attributes :: opentelemetry:attributes_map(),
      DroppedAttributes :: opentelemetry:attributes_map().

-callback collect(Table, ExemplarTable, Stream, Generation) -> tuple() when
      Table :: ets:table(),
      ExemplarTable :: ets:table(),
      Stream :: #stream{},
      Generation :: integer().

maybe_init_aggregate(Ctx, MetricsTab, ExemplarsTab, Stream=#stream{aggregation_module=AggregationModule,
                                                                   attribute_keys=AttributeKeys},
                     Value, Attributes) ->
    {FilteredAttributes, DroppedAttributes} = filter_attributes(AttributeKeys, Attributes),
    AttributesKey = term_to_binary(FilteredAttributes),
    case AggregationModule:aggregate(Ctx, MetricsTab, ExemplarsTab, Stream, Value, AttributesKey, DroppedAttributes) of
        true ->
            true;
        false ->
            %% entry doesn't exist (Exists=false); apply the cardinality limit
            %% before creating a new series. If the limit is reached the
            %% measurement is redirected into the single overflow series.
            maybe_init_new_series(Ctx, MetricsTab, ExemplarsTab, Stream, Value,
                                  FilteredAttributes, AttributesKey, DroppedAttributes)
    end.

maybe_init_new_series(Ctx, MetricsTab, ExemplarsTab,
                      Stream=#stream{aggregation_module=AggregationModule,
                                     cardinality_limit=Limit},
                      Value, FilteredAttributes, AttributesKey, DroppedAttributes) ->
    CurrentCount = series_count(MetricsTab, Stream),
    case otel_metric_cardinality:limit_attributes(FilteredAttributes, CurrentCount, false, limit(Limit)) of
        FilteredAttributes ->
            %% under the limit: create the requested series and record into it
            init_and_aggregate(Ctx, MetricsTab, ExemplarsTab, Stream, Value,
                               AttributesKey, DroppedAttributes);
        OverflowAttributes ->
            %% over the limit: fold into the single overflow series. It may
            %% already exist (and just needs updating) or need to be created
            %% once and then reused.
            OverflowKey = term_to_binary(OverflowAttributes),
            case AggregationModule:aggregate(Ctx, MetricsTab, ExemplarsTab, Stream, Value, OverflowKey, DroppedAttributes) of
                true ->
                    true;
                false ->
                    init_and_aggregate(Ctx, MetricsTab, ExemplarsTab, Stream, Value,
                                       OverflowKey, DroppedAttributes)
            end
    end.

init_and_aggregate(Ctx, MetricsTab, ExemplarsTab,
                   Stream=#stream{aggregation_module=AggregationModule}, Value,
                   AttributesKey, DroppedAttributes) ->
    %% entry doesn't exist, create it and rerun the aggregate function
    Metric = AggregationModule:init(Stream, AttributesKey),
    %% don't overwrite a possible concurrent measurement doing the same
    _ = ets:insert_new(MetricsTab, Metric),
    AggregationModule:aggregate(Ctx, MetricsTab, ExemplarsTab, Stream, Value, AttributesKey, DroppedAttributes).

%% a limit of `undefined' (e.g. on a stream built before the field existed)
%% falls back to the default limit
limit(undefined) ->
    otel_metric_cardinality:default_limit();
limit(Limit) ->
    Limit.

%% Count the distinct series currently recorded for this stream (same metric
%% name, reader, and generation). This runs only on the cold path when a new
%% series would otherwise be created.
series_count(MetricsTab, #stream{name=Name,
                                 reader=ReaderId,
                                 forget=Forget}) ->
    Generation = case Forget of
                     true ->
                         otel_metric_reader:checkpoint_generation(ReaderId);
                     _ ->
                         0
                 end,
    %% all aggregation records store the key `{Name, Attributes, ReaderId,
    %% Generation}' at element 2; match on the key shape regardless of the
    %% record type (which have differing arities).
    MS = [{'$1',
           [{'=:=', {element, 1, {element, 2, '$1'}}, {const, Name}},
            {'=:=', {element, 3, {element, 2, '$1'}}, {const, ReaderId}},
            {'=:=', {element, 4, {element, 2, '$1'}}, {const, Generation}}],
           [true]}],
    ets:select_count(MetricsTab, MS).

filter_attributes(undefined, Attributes) ->
    {Attributes, #{}};
filter_attributes(Keys, Attributes) ->
    split(Keys, Attributes).

-spec default_mapping() -> #{otel_instrument:kind() => module()}.
default_mapping() ->
    #{?KIND_COUNTER => otel_aggregation_sum,
      ?KIND_OBSERVABLE_COUNTER => otel_aggregation_sum,
      ?KIND_HISTOGRAM => otel_aggregation_histogram_explicit,
      ?KIND_OBSERVABLE_GAUGE => otel_aggregation_last_value,
      ?KIND_UPDOWN_COUNTER => otel_aggregation_sum,
      ?KIND_OBSERVABLE_UPDOWNCOUNTER => otel_aggregation_sum}.

%% by default the aggregators use the same temporality as is native to the instrument
-spec default_temporality_mapping() -> #{otel_instrument:kind() => otel_instrument:temporality()}.
default_temporality_mapping() ->
    lists:foldl(fun(Kind, Acc) ->
                        Acc#{Kind => otel_instrument:kind_temporality(Kind)}
                end, #{}, [?KIND_COUNTER,
                           ?KIND_OBSERVABLE_COUNTER,
                           ?KIND_HISTOGRAM,
                           ?KIND_OBSERVABLE_GAUGE,
                           ?KIND_UPDOWN_COUNTER,
                           ?KIND_OBSERVABLE_UPDOWNCOUNTER
                          ]).

split(Keys, Map) ->
    lists:foldl(fun(Key, {KeptAcc, DroppedAcc}) ->
                        case maps:take(Key, DroppedAcc) of
                            {Value, DroppedAcc1} ->
                                {KeptAcc#{Key => Value}, DroppedAcc1};
                            error ->
                                {KeptAcc, DroppedAcc}
                        end
                end, {#{}, Map}, Keys).
