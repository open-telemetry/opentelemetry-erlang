%%%------------------------------------------------------------------------
%% Copyright 2019, OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc
%% @end
%%%-------------------------------------------------------------------------
-module(ot_benchmarks).

-export([run/0]).

-include_lib("opentelemetry_api/include/tracer.hrl").

run() ->
    Iterations = 10,
    PDictCtxFun = fun() ->
                      [?start_span(<<"span-", (integer_to_binary(X))/binary>>)
                       || X <- lists:seq(1, Iterations)]
                  end,
    benchee:run(#{<<"pdict_ctx">> => PDictCtxFun}),
    ok.
