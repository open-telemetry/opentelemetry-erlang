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
-module(ot_propagation).

-export([http_inject/1,
         http_extract/1]).

-type extractor(T) :: {fun((T, {fun(), term()}) -> ok), term()} |
                      {fun((T, ot_ctx:key(), {fun(), term()}) -> ok), term()}.
-type injector(T) :: {fun((T, {fun(), term()}) -> T), term()} |
                     {fun((T, ot_ctx:key(), {fun(), term()}) -> T), term()}.

-type http_headers() :: [{binary(), binary()}].

-type http_injector() :: injector(http_headers()).
-type http_extractor() :: extractor(http_headers()).

-export_type([extractor/1,
              injector/1,
              http_injector/0,
              http_extractor/0,
              http_headers/0]).

http_inject(Headers) ->
    Injectors = opentelemetry:get_http_injector(),
    run_injectors(Headers, Injectors).

http_extract(Headers) ->
    Extractors = opentelemetry:get_http_extractor(),
    run_extractors(Headers, Extractors).

run_extractors(Headers, Extractors) ->
    lists:foldl(fun({Extract, {Key, FromText}}, ok) ->
                        Extract(Headers, Key, FromText),
                        ok;
                   ({Extract, FromText}, ok) ->
                        Extract(Headers, FromText),
                        ok;
                   (_, ok) ->
                        ok
                end, ok, Extractors).

run_injectors(Headers, Injectors) ->
    lists:foldl(fun({Inject, {Key, ToText}}, HeadersAcc) ->
                        Inject(HeadersAcc, Key, ToText);
                   ({Inject, ToText}, HeadersAcc) ->
                        Inject(HeadersAcc, ToText);
                   (_, HeadersAcc) ->
                        HeadersAcc
                end, Headers, Injectors).
