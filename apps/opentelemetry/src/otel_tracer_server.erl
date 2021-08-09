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
-module(otel_tracer_server).

-behaviour(otel_tracer_provider).

-export([init/1,
         register_tracer/4,
         register_tracer/3,
         resource/1,
         code_change/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_tracer.hrl").
-include("otel_span.hrl").

-type telemetry_library() :: #telemetry_library{}.
-type instrumentation_library() :: #instrumentation_library{}.
-export_type([telemetry_library/0,
              instrumentation_library/0]).

-record(state,
        {
         %% the tracer configuration shared between all named
         %% tracers created by this provider
         shared_tracer :: tracer(),
         processors :: [module()],
         sampler :: otel_sampler:t(),
         resource :: otel_resource:t(),

         %% list of tracer names to return noop tracers for
         deny_list :: [atom() | {atom(), string()}],

         %% data about this SDK implementation itself
         %% the name, language (erlang) and version
         telemetry_library :: telemetry_library()
        }).

init(Opts) ->
    Resource = otel_resource_detector:get_resource(),

    SamplerSpec = proplists:get_value(
        sampler, Opts, {parent_based, #{root => always_on}}
    ),
    Sampler = otel_sampler:new(SamplerSpec),
    Processors = proplists:get_value(processors, Opts, []),
    DenyList = proplists:get_value(deny_list, Opts, []),

    {ok, LibraryVsn} = application:get_key(opentelemetry, vsn),
    LibraryName = <<"opentelemetry">>,
    LibraryLanguage = <<"erlang">>,
    TelemetryLibrary = #telemetry_library{name=LibraryName,
                                          language=LibraryLanguage,
                                          version=list_to_binary(LibraryVsn)},

    Tracer = #tracer{module=otel_tracer_default,
                     sampler=Sampler,
                     on_start_processors=on_start(Processors),
                     on_end_processors=on_end(Processors),
                     resource=Resource,
                     telemetry_library=TelemetryLibrary},
    opentelemetry:set_default_tracer({otel_tracer_default, Tracer}),

    {ok, #state{shared_tracer=Tracer,
                resource=Resource,
                sampler=Sampler,
                telemetry_library=TelemetryLibrary,
                deny_list=DenyList,
                processors=Processors}}.

resource(#state{resource=Resource}) ->
    Resource.

register_tracer(Name, Vsn, Modules, #state{shared_tracer=Tracer,
                                           deny_list=DenyList}) ->
    %% TODO: support semver constraints in denylist
    case proplists:is_defined(Name, DenyList) of
        true ->
            opentelemetry:set_tracer(Name, {otel_tracer_noop, []});
        false ->
            InstrumentationLibrary = otel_utils:instrumentation_library(Name, Vsn),
            TracerTuple = {Tracer#tracer.module,
                           Tracer#tracer{instrumentation_library=InstrumentationLibrary}},
            [opentelemetry:set_tracer(M, TracerTuple) || M <- Modules]
    end.

register_tracer(Name, Vsn, #state{shared_tracer=Tracer,
                                  deny_list=DenyList}) ->
    %% TODO: support semver constraints in denylist
    case proplists:is_defined(Name, DenyList) of
        true ->
            opentelemetry:set_tracer(Name, {otel_tracer_noop, []});
        false ->
            InstrumentationLibrary = otel_utils:instrumentation_library(Name, Vsn),
            TracerTuple = {Tracer#tracer.module,
                           Tracer#tracer{instrumentation_library=InstrumentationLibrary}},
            opentelemetry:set_tracer(Name, TracerTuple)
    end.

%% the application vsn has likely changed during a code change
%% so update the shared tracer data here
code_change(State=#state{shared_tracer=Tracer=#tracer{telemetry_library=TelemetryLibrary}}) ->
    {ok, LibraryVsn} = application:get_key(opentelemetry, vsn),

    NewTelemetryLibrary = TelemetryLibrary#telemetry_library{version=list_to_binary(LibraryVsn)},
    NewTracer = Tracer#tracer{telemetry_library=NewTelemetryLibrary},

    {ok, State#state{shared_tracer=NewTracer}}.

%%

on_start(Processors) ->
    fun(Ctx, Span) ->
            lists:foldl(fun({P, Config}, Acc) ->
                                P:on_start(Ctx, Acc, Config)
                        end, Span, Processors)
    end.

on_end(Processors) ->
    fun(Span) ->
            lists:foldl(fun({P, Config}, Bool) ->
                                Bool andalso P:on_end(Span, Config)
                        end, true, Processors)
    end.
