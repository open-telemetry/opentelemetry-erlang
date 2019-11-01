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

