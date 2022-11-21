-module(opentelemetry_semantic_conventions).

-export([]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("trace.hrl").

quick_test() ->
    ?assertEqual('aws.lambda.invoked_arn', ?AWS_LAMBDA_INVOKED_ARN).
-endif.
