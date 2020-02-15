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
-module(ot_tracer_server).

-behaviour(ot_tracer_provider).

-export([init/1,
         register_tracer/4,
         register_tracer/3]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_tracer.hrl").
-include("ot_span.hrl").

-type library_resource() :: #library_resource{}.
-export_type([library_resource/0]).

-record(state, {tracer :: tracer(),
                processors :: [module()],
                deny_list :: [atom() | {atom(), string()}],
                sampler :: ot_sampler:sampler()}).

init(Opts) ->
    {Sampler, SamplerOpts} = proplists:get_value(sampler, Opts, {always_on, #{}}),
    SamplerFun = ot_sampler:setup(Sampler, SamplerOpts),
    Processors = proplists:get_value(processors, Opts, []),
    DenyList = proplists:get_value(deny_list, Opts, []),

    Tracer = #tracer{module=ot_tracer_default,
                     sampler=SamplerFun,
                     on_start_processors=on_start(Processors),
                     on_end_processors=on_end(Processors),
                     span_module=ot_span_ets,
                     ctx_module=ot_ctx_pdict},
    opentelemetry:set_default_tracer({ot_tracer_default, Tracer}),

    {ok, #state{tracer=Tracer,
                sampler=SamplerFun,
                deny_list=DenyList,
                processors=Processors}}.

register_tracer(Name, Vsn, Modules, #state{tracer=Tracer,
                                           deny_list=DenyList}) ->
    %% TODO: support semver constraints in denylist
    case proplists:is_defined(Name, DenyList) of
        true ->
            opentelemetry:set_tracer(Name, {ot_tracer_noop, []});
        false ->
            LibraryResource = #library_resource{name=Name,
                                                version=Vsn},
            [opentelemetry:set_tracer(M, {Tracer#tracer.module,
                                          Tracer#tracer{library_resource=LibraryResource}})
            || M <- Modules]
    end.

register_tracer(Name, Vsn, #state{tracer=Tracer,
                                  deny_list=DenyList}) ->
    %% TODO: support semver constraints in denylist
    case proplists:is_defined(Name, DenyList) of
        true ->
            opentelemetry:set_tracer(Name, {ot_tracer_noop, []});
        false ->
            LibraryResource = #library_resource{name=Name,
                                                version=Vsn},
            opentelemetry:set_tracer(Name, {Tracer#tracer.module,
                                            Tracer#tracer{library_resource=LibraryResource}})
    end.


%%

on_start(Processors) ->
    fun(Span) -> [P:on_start(Span, Config) || {P, Config} <- Processors] end.

on_end(Processors) ->
    fun(Span) -> [P:on_end(Span, Config) || {P, Config} <- Processors] end.
