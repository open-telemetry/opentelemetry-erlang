%% Try for 1 seconds
-define(UNTIL(X), (fun Until(I) when I =:= 10 ->
                           ct:fail(timeout);
                       Until(I) ->
                           case X of
                               true ->
                                   ok;
                               false ->
                                   timer:sleep(100),
                                   Until(I+1)
                           end
                   end)(0)).

%% try for 1 second and also return the result of Y
-define(UNTIL_NOT_EQUAL(X, Y), (fun Until(I) when I =:= 10 ->
                                        ct:fail(timeout);
                                    Until(I) ->
                                        R = Y,
                                        case X =/= R of
                                            true ->
                                                R;
                                            false ->
                                                timer:sleep(100),
                                                Until(I+1)
                                        end
                                end)(0)).

-define(assertListsMatch(List1, List2), ?assertEqual(lists:sort(List1), lists:sort(List2))).

%% a macro for asserting the important parts of a span ctx are equal
%% parts we keep in the record like is_recorded is not propagated and
%% thus should not be part of a comparison to check propagated ctx
-define(assertSpanCtxsEqual(SpanCtx1, SpanCtx2), begin
                                                    #span_ctx{trace_id=TraceId1,
                                                              span_id=SpanId1,
                                                              trace_flags=TraceFlags1} = SpanCtx1,
                                                    #span_ctx{trace_id=TraceId2,
                                                              span_id=SpanId2,
                                                              trace_flags=TraceFlags2} = SpanCtx2,
                                                    ?assertEqual({TraceId1, SpanId1, TraceFlags1},
                                                                 {TraceId2, SpanId2, TraceFlags2})
                                                end).
