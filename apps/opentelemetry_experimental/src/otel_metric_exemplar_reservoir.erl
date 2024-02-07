%%%------------------------------------------------------------------------
%% Copyright 2024, OpenTelemetry Authors
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
-module(otel_metric_exemplar_reservoir).

-export([new/2,
         offer/6,
         collect/3]).

-record(exemplar_resevoir, {module :: module(),
                            state :: term()}).

new(Module, Config) ->
    State = Module:new(Config),
    #exemplar_resevoir{module=Module,
                       state=State}.

offer(Ctx, #exemplar_resevoir{module=Module,
                              state=State}, ExemplarTab, Key, Value, DroppedAttributes) ->
    Module:offer(Ctx, ExemplarTab, Key, Value, DroppedAttributes, State).

collect(#exemplar_resevoir{module=Module,
                           state=State}, ExemplarTab, Key) ->
    Module:collect(ExemplarTab, Key, State).
