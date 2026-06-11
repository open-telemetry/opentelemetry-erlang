%% EUnit tests for the pure cardinality-limit decision helper.
%%
%% These mirror the "meat" of opentelemetry-go's
%% sdk/metric/internal/aggregate/limit_test.go (TestLimiterAttributes) and the
%% cardinality-limit semantics shared by opentelemetry-java
%% (MetricStorage.DEFAULT_MAX_CARDINALITY / CARDINALITY_OVERFLOW).
%%
%% Contract under test (designed for this repo's ETS-keyed aggregation, where
%% the caller supplies the current distinct-series count and an existence flag
%% rather than passing a whole map like Go does):
%%
%%   otel_metric_cardinality:limit_attributes(Attributes, CurrentCount, Exists, Limit)
%%       -> Attributes | OverflowAttributes
%%
%%     * Limit =< 0          -> unlimited; always return Attributes (Go semantics)
%%     * Exists =:= true     -> series already counted; return Attributes
%%     * not Exists and CurrentCount >= Limit - 1 -> return OverflowAttributes
%%     * otherwise           -> return Attributes
%%
%%   otel_metric_cardinality:overflow_attributes() -> #{<<"otel.metric.overflow">> => true}
%%   otel_metric_cardinality:default_limit()       -> 2000
%%
%% The `Limit - 1` reservation matches Go's `len(measurements) >= aggLimit-1`:
%% with limit N you get up to N-1 real series plus 1 overflow series (N total).
-module(otel_metric_cardinality_tests).

-include_lib("eunit/include/eunit.hrl").

alice() -> #{<<"user">> => <<"alice">>}.
bob()   -> #{<<"user">> => <<"bob">>}.

overflow() -> #{<<"otel.metric.overflow">> => true}.

limit(Attrs, Count, Exists, Limit) ->
    otel_metric_cardinality:limit_attributes(Attrs, Count, Exists, Limit).

%% --- Go limit_test.go parity -------------------------------------------------
%% Go's measurement map always holds exactly one entry (alice), so across these
%% cases CurrentCount = 1 and Exists = (Attrs == alice).

%% t.Run("NoLimit")
no_limit_test() ->
    ?assertEqual(alice(), limit(alice(), 1, true, 0)),
    ?assertEqual(bob(),   limit(bob(),   1, false, 0)).

%% t.Run("NotAtLimit/Exists")
not_at_limit_exists_test() ->
    ?assertEqual(alice(), limit(alice(), 1, true, 3)).

%% t.Run("NotAtLimit/DoesNotExist")
not_at_limit_new_test() ->
    ?assertEqual(bob(), limit(bob(), 1, false, 3)).

%% t.Run("AtLimit/Exists")
at_limit_exists_test() ->
    ?assertEqual(alice(), limit(alice(), 1, true, 2)).

%% t.Run("AtLimit/DoesNotExist")
at_limit_new_test() ->
    ?assertEqual(overflow(), limit(bob(), 1, false, 2)).

%% --- contract extras ---------------------------------------------------------

%% With limit N only N-1 distinct real series are allowed; the Nth distinct set
%% overflows. (One slot reserved for the overflow series itself.)
reserves_one_slot_for_overflow_test() ->
    %% limit 3 => 2 real series allowed
    ?assertEqual(bob(),      limit(bob(), 1, false, 3)),   %% count 1 < 2 -> allowed
    ?assertEqual(overflow(), limit(bob(), 2, false, 3)).   %% count 2 >= 2 -> overflow

%% Once an attribute set is already present it is always allowed through,
%% regardless of how far over the limit the count is. This is what lets the
%% single overflow series keep accumulating spillover measurements.
existing_series_always_allowed_test() ->
    ?assertEqual(overflow(), limit(overflow(), 50, true, 3)),
    ?assertEqual(alice(),    limit(alice(),   50, true, 3)).

%% A negative limit is treated the same as 0 (unlimited), per Go semantics.
negative_limit_is_unlimited_test() ->
    ?assertEqual(bob(), limit(bob(), 999999, false, -1)).

%% limit of 1 means zero real series: the very first distinct set overflows.
limit_one_overflows_immediately_test() ->
    ?assertEqual(overflow(), limit(alice(), 0, false, 1)).

default_limit_is_2000_test() ->
    ?assertEqual(2000, otel_metric_cardinality:default_limit()).

overflow_marker_attribute_test() ->
    ?assertEqual(#{<<"otel.metric.overflow">> => true},
                 otel_metric_cardinality:overflow_attributes()).
