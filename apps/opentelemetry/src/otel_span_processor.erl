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
%% @doc Behaviour each Span Processor must implement.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_span_processor).

-export([]).

-type processor_config() :: term().

-export_type([processor_config/0]).

-callback on_start(otel_ctx:t(), opentelemetry:span(), processor_config()) -> opentelemetry:span().
-callback on_end(opentelemetry:span(), processor_config()) -> true |
                                                              dropped |
                                                              {error, invalid_span} |
                                                              {error, no_export_buffer}.
-callback force_flush(processor_config()) -> ok |
                                             {error, term()}.
