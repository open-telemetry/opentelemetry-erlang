%%%------------------------------------------------------------------------
%% Copyright 2022, OpenTelemetry Authors
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
%% @private
%%%-------------------------------------------------------------------------
-module(otel_tracer_server_sup).

-behaviour(supervisor).

-export([start_link/3]).

-export([init/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

start_link(Name, Resource, Opts) ->
    supervisor:start_link(?MODULE, [Name, Resource, Opts]).

init([Name, Resource, Opts]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    %% the name of the span tracer provider is used to distinguish span processor
    %% supervisors and the tracer provider itself.
    %% in the case of the global provider the name is `global'
    SpanProcessorSupRegName = list_to_atom(lists:concat([otel_span_processor_sup, "_", Name])),
    TracerServeRegName = list_to_atom(lists:concat([otel_tracer_provider, "_", Name])),

    SpanProcessorSup = #{id => otel_span_processor_sup,
                         start => {otel_span_processor_sup, start_link, [SpanProcessorSupRegName]},
                         restart => permanent,
                         shutdown => 5000,
                         type => supervisor,
                         modules => [otel_span_processor_sup, otel_span_processor]},

    TracerServer = #{id => otel_tracer_server,
                     start => {otel_tracer_server, start_link, [Name,
                                                                TracerServeRegName,
                                                                SpanProcessorSupRegName,
                                                                Resource,
                                                                Opts]},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [otel_tracer_provider, otel_tracer_server]},

    ChildSpecs = [SpanProcessorSup, TracerServer],

    {ok, {SupFlags, ChildSpecs}}.
