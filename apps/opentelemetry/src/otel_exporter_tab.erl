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
%% @doc A test reporter that keeps finished spans in an ETS table.
%% @end
%%%-----------------------------------------------------------------------
-module(otel_exporter_tab).

-behaviour(otel_exporter).

-export([init/1,
         export/3,
         shutdown/1]).

init(Tid) ->
    {ok, Tid}.

export(SpansTid, _Resource, Tid) ->
    ets:foldl(fun(Span, _Acc) ->
                      ets:insert(Tid, Span)
              end, [], SpansTid),
    ok.

shutdown(_) ->
    ok.
