%%%------------------------------------------------------------------------
%% Copyright 2020, OpenTelemetry Authors
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
-module(otel_propagator).

-export([text_map_inject/1,
         text_map_extract/1]).

-callback inject(term()) -> ot_propagator:carrier().
-callback extract(ot_propagator:carrier(), term()) -> term().

-type text_map() :: [{binary(), binary()}].

%% TODO add binary carrier when it is included in the otel spec
-type carrier() :: text_map().

%% T is a carrier()
-type extractor(T) :: {fun((T, term(), fun()) -> ok), term()}.
-type injector(T) :: {fun((T, term(), fun()) -> T), term()}.

-type text_map_injector() :: injector(text_map()).
-type text_map_extractor() :: extractor(text_map()).

-export_type([carrier/0,
              extractor/1,
              injector/1,
              text_map_injector/0,
              text_map_extractor/0,
              text_map/0]).

text_map_inject(TextMap) ->
    Injectors = opentelemetry:get_text_map_injectors(),
    run_injectors(TextMap, Injectors).

text_map_extract(TextMap) ->
    Extractors = opentelemetry:get_text_map_extractors(),
    run_extractors(TextMap, Extractors).

run_extractors(TextMap, Extractors) ->
    lists:foldl(fun({Extract, {Key, FromText}}, ok) ->
                        Extract(TextMap, Key, FromText),
                        ok;
                   (_, ok) ->
                        ok
                end, ok, Extractors).

run_injectors(TextMap, Injectors) ->
    lists:foldl(fun({Inject, {Key, ToText}}, TextMapAcc) ->
                        Inject(TextMapAcc, Key, ToText);
                   (_, TextMapAcc) ->
                        TextMapAcc
                end, TextMap, Injectors).
