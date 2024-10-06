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
%%%-----------------------------------------------------------------------
-module(otel_exporter_traces).

-export([init/1,
         export/3,
         shutdown/1]).

%% Do any initialization of the exporter here and return configuration
%% that will be passed along with a list of spans to the `export' function.
-callback init(term()) -> {ok, term()} | ignore.

%% This function is called when the configured interval expires with any
%% spans that have been collected so far and the configuration returned in `init'.
%% Do whatever needs to be done to export each span here, the caller will block
%% until it returns.
-callback export(ets:tab(), otel_resource:t(), term()) -> ok |
                                                          success |
                                                          failed_not_retryable |
                                                          failed_retryable.
-callback shutdown(term()) -> ok.

init(Opts) ->
    otel_exporter:init(Opts).

export({ExporterModule, Config}, SpansTid, Resource) ->
    ExporterModule:export(SpansTid, Resource, Config).

shutdown(Exporter) ->
    otel_exporter:shutdown(Exporter).
