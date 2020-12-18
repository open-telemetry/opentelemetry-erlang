-module(otel_resource_detector_test).

-export([get_resource/1]).

get_resource(sleep) ->
    timer:sleep(5000),
    otel_resource:create([{<<"some-test-resource">>, <<"some-test-value">>}]);
get_resource(error) ->
    erlang:error(some_failure).
