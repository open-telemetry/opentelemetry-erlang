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
-module(custom_propagator).

-behaviour(otel_propagator_text_map).

%% functions for interacting with the custom context key/value
-export([add_to_context/1,
         context_content/0]).

-export([fields/1,
         inject/4,
         extract/5]).

-define(SOMETHING_CTX_KEY, ?MODULE).
-define(SOMETHING_TEXT_ID, <<"something-header-id">>).

fields(_) ->
    [?SOMETHING_TEXT_ID].

add_to_context(Something) ->
    otel_ctx:set_value(?SOMETHING_CTX_KEY, Something).

context_content() ->
    otel_ctx:get_value(?SOMETHING_CTX_KEY).

inject(Ctx, Carrier, CarrierSet, _) ->
    case otel_ctx:get_value(Ctx, ?SOMETHING_CTX_KEY, undefined) of
        undefined ->
            Carrier;
        Value ->
            CarrierSet(?SOMETHING_TEXT_ID, Value, Carrier)
    end.

extract(Ctx, Carrier, _CarrierKeysFun, CarrierGet, _) ->
    Value = CarrierGet(?SOMETHING_TEXT_ID, Carrier),
    otel_ctx:set_value(Ctx, ?SOMETHING_CTX_KEY, Value).

