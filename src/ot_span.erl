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
%% Span behaviour.
%% @end
%%%-------------------------------------------------------------------------
-module(ot_span).

-type start_opts() :: #{parent => undefined | opentelemetry:span() | opentelemetry:span_ctx(),
                        sampler => module(),
                        links => opentelemetry:links(),
                        is_recorded => boolean(),
                        kind => opentelemetry:span_kind()}.

-export_type([start_opts/0]).

-callback start_span(opentelemetry:span_name(), start_opts()) -> opentelemetry:span_ctx().
-callback finish_span(opentelemetry:span_ctx()) -> ok.
-callback get_ctx(opentelemetry:span()) -> opentelemetry:span_ctx().
-callback is_recording_events(opentelemetry:span_ctx()) -> boolean().
-callback set_attributes(opentelemetry:span_ctx(), opentelemetry:attributes()) -> ok.
-callback add_events(opentelemetry:span_ctx(), opentelemetry:time_events()) -> ok.
-callback add_links(opentelemetry:span_ctx(), opentelemetry:links()) -> ok.
-callback set_status(opentelemetry:span_ctx(), opentelemetry:status()) -> ok.
-callback update_name(opentelemetry:span_ctx(), opentelemetry:span_name()) -> ok.

