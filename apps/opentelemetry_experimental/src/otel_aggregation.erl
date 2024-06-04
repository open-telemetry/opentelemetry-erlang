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
    case AggregationModule:aggregate(Ctx, MetricsTab, ExemplarsTab, Stream, Value, term_to_binary(FilteredAttributes), DroppedAttributes) of
        true ->
            true;
        false ->
            %% entry doesn't exist, create it and rerun the aggregate function
            Metric = AggregationModule:init(Stream, term_to_binary(FilteredAttributes)),
            %% don't overwrite a possible concurrent measurement doing the same
            _ = ets:insert_new(MetricsTab, Metric),
            AggregationModule:aggregate(Ctx, MetricsTab, ExemplarsTab, Stream, Value, term_to_binary(FilteredAttributes), DroppedAttributes)
    end.

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
