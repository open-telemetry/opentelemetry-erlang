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
-module(ot_correlations).

-export([ctx_key/0,
         set/3,
         get_http_extractor/0,
         get_http_injector/0]).

-type key() :: string().
-type value() :: string().
-type hop_limit() :: no_propagation | unlimited_propagation.

-export_type([key/0,
              value/0]).

-define(CORRELATIONS_KEY, '$__ot_correlations_ctx_key').

ctx_key() ->
    ?CORRELATIONS_KEY.

-spec set(key(), value(), hop_limit()) -> ok.
set(Key, Value, HopLimit) ->
    ot_ctx:set_value(?CORRELATIONS_KEY, Key, {Value, HopLimit}).

-spec get_http_extractor() -> ot_propagation:extractor().
get_http_extractor() ->
    ok.

-spec get_http_injector() -> ot_propagation:injector().
get_http_injector() ->
    ok.
