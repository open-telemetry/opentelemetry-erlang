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

-export([]).

-type extractor() :: fun().
-type injector() :: fun().
-type http_headers() :: [{string(), string()}].

-callback http_inject(ot_ctx:ctx()) -> http_headers().
-callback http_extract(ot_ctx:ctx(), http_headers()) -> ot_ctx:ctx().

-callback set_http_injector(injector()) -> ok.
-callback get_http_injector() -> injector().

-callback set_http_extractor(extractor()) -> ok.
-callback get_http_extractor() -> extractor().

-export_type([extractor/0,
              injector/0,
              http_headers/0]).
