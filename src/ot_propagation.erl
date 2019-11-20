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

-type data() :: term().

-type extractor() :: fun((data()) -> ok).
-type injector() :: fun((data()) -> data()).
-type http_headers() :: [{iodata(), iodata()}].

-type http_injector() :: fun((http_headers()) -> http_headers()).
-type http_extractor() :: fun((http_headers()) -> ok).

-export_type([extractor/0,
              injector/0,
              http_injector/0,
              http_extractor/0,
              http_headers/0]).

http_inject(Headers) ->
    Fun = opentelemetry:get_http_injector(),
    Fun(Headers).

http_extract(Headers) ->
    Fun = opentelemetry:get_http_extractor(),
    Fun(Headers).
