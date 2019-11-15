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
-module(ot_baggage).

-export([ctx_key/0,
         set/2,
         get/1,
         remove/1,
         clear/0,
         get_http_extractor/0,
         get_http_injector/0]).

-type key() :: string().
-type value() :: string().

-export_type([key/0,
              value/0]).

-define(BAGGAGE_KEY, '$__ot_baggage_ctx_key').

ctx_key() ->
    ?BAGGAGE_KEY.

-spec set(key(), value()) -> ok.
set(Key, Value) ->
    ot_ctx:set_value(?BAGGAGE_KEY, Key, Value).

-spec get(key()) -> value().
get(Key)->
    ot_ctx:get_value(?BAGGAGE_KEY, Key).

-spec remove(key()) -> ok.
remove(Key) ->
    ot_ctx:remove(?BAGGAGE_KEY, Key).

-spec clear() -> ok.
clear() ->
    ot_ctx:clear(?BAGGAGE_KEY).

-spec get_http_extractor() -> ot_propagation:extractor().
get_http_extractor() ->
    ok.

-spec get_http_injector() -> ot_propagation:injector().
get_http_injector() ->
    ok.
