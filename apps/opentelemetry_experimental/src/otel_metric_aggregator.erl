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
%%
%% @end
%%%-------------------------------------------------------------------------
-module(otel_metric_aggregator).

-include("otel_meter.hrl").

-callback update(ets:tab(), otel_meter:name() | {otel_meter:name(), otel_meter:labels()}, otel_meter:number_kind(), number()) -> boolean().

-callback checkpoint(ets:tab(), otel_meter:name() | {otel_meter:name(), otel_meter:labels()}) -> boolean().

-callback merge(term(), term()) -> term().

%% TODO: rename to `new'
-callback initial_value(otel_meter:number_kind()) -> term().
