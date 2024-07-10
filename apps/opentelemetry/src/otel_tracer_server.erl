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
%% @doc This module is the SDK's implementation of the TracerProvider. The
%% calls to the server are done from the API module `otel_tracer_provider'.
%% This `gen_server' is started as part of the SDK's supervision tree and
%% registers itself as the default TracerProvider by using the name
%% `otel_tracer_provider' as its name.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_tracer_server).

-behaviour(gen_server).

-export([start_link/5]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         code_change/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_tracer.hrl").
-include("otel_span.hrl").

-type instrumentation_scope() :: #instrumentation_scope{}.
-export_type([instrumentation_scope/0]).

-record(state,
        {
         %% the tracer configuration shared between all named
         %% tracers created by this provider
         shared_tracer :: tracer() | undefined,
         id_generator :: module(),
         processors :: [module()],
         sampler :: otel_sampler:t(),
         resource :: otel_resource:t(),

         span_processor_sup :: atom(),

         %% list of tracer names to return noop tracers for
         deny_list :: [atom() | {atom(), string()}]
        }).

-spec start_link(atom(), atom(), atom(), otel_resource:t(), otel_configuration:t()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Name, RegName, SpanProcessorSupRegName, Resource, Config) ->
    gen_server:start_link({local, RegName}, ?MODULE, [Name, SpanProcessorSupRegName, Resource, Config], []).

init([Name, SpanProcessorSup, Resource, #{id_generator := IdGeneratorModule,
                                          sampler := SamplerSpec,
                                          processors := Processors,
                                          deny_list := DenyList}]) ->
    Sampler = otel_sampler:new(SamplerSpec),

    Processors1 = init_processors(SpanProcessorSup, Processors),

    Tracer = #tracer{module=otel_tracer_default,
                     sampler=Sampler,
                     on_start_processors=on_start(Processors1),
                     on_end_processors=on_end(Processors1),
                     id_generator=IdGeneratorModule},

    %% TODO: don't do this if its already set?
    opentelemetry:set_default_tracer(Name, {otel_tracer_default, Tracer}),

    {ok, #state{id_generator=IdGeneratorModule,
                resource=Resource,
                sampler=Sampler,
                deny_list=DenyList,
                shared_tracer=Tracer,
                span_processor_sup=SpanProcessorSup,
                processors=Processors1}}.

handle_call(resource, _From, State=#state{resource=Resource}) ->
    {reply, Resource, State};
handle_call({get_tracer, _Name, _Vsn, _SchemaUrl}, _From, State=#state{shared_tracer=undefined}) ->
    {reply, {otel_tracer_noop, []}, State};
handle_call({get_tracer, Name, Vsn, SchemaUrl}, _From, State=#state{shared_tracer=Tracer,
                                                                    deny_list=DenyList})
  when Tracer =/= undefined ->
    %% TODO: support semver constraints in denylist
    case proplists:is_defined(Name, DenyList) of
        true ->
            {reply, {otel_tracer_noop, []}, State};
        false ->
            InstrumentationScope = opentelemetry:instrumentation_scope(Name, Vsn, SchemaUrl),
            TracerTuple = {Tracer#tracer.module,
                           Tracer#tracer{instrumentation_scope=InstrumentationScope}},
            {reply, TracerTuple, State}
    end;
handle_call({get_tracer, InstrumentationScope}, _From, State=#state{shared_tracer=Tracer,
                                                                    deny_list=_DenyList})
  when Tracer =/= undefined ->
    {reply, {Tracer#tracer.module,
             Tracer#tracer{instrumentation_scope=InstrumentationScope}}, State};
handle_call(force_flush, _From, State=#state{processors=Processors}) ->
    Reply = lists:foldl(fun(Processor, Result) ->
                                case force_flush_(Processor) of
                                    ok ->
                                        Result;
                                    {error, Reason} ->
                                        update_force_flush_error(Reason, Result)
                                end
                        end, ok, Processors),
    {reply, Reply, State}.

handle_cast(_, State) ->
    {noreply, State}.

code_change(State) ->
    {ok, State}.
%%

-spec update_force_flush_error(term(), ok | {error, list()}) -> {error, list()}.
update_force_flush_error(Reason, ok) ->
    {error, [Reason]};
update_force_flush_error(Reason, {error, List}) ->
    {error, [Reason | List]}.

%% TODO: remove after a period of deprecation for processor `set_exporter' functions
%% This is a hack that makes it so there is a known name for the processor if there
%% is only one, like was once guaranteed. This allows functions like `set_exporter'
%% to work. But if there is more than one processor defined or it isn't one of the
%% builtin processors we will not do this hack of adding a name to the config.
init_processors(SpanProcessorSup, [{P, Config}]) when P =:= otel_batch_processor ;
                                                      P =:= otel_simple_processor ->
    case init_processor(SpanProcessorSup, P, maps:merge(#{name => global}, Config)) of
        {true, {_, _}=Processor} ->
            [Processor];
        _ ->
            []
    end;
init_processors(SpanProcessorSup, Processors) ->
    init_processors_(SpanProcessorSup, Processors).

init_processors_(_SpanProcessorSup, []) ->
    [];
init_processors_(SpanProcessorSup, [{P, Config} | Rest]) ->
    case init_processor(SpanProcessorSup, P, Config) of
        {true, {_, _}=Processor} ->
            [Processor | init_processors_(SpanProcessorSup, Rest)];
        _ ->
            init_processors_(SpanProcessorSup, Rest)
    end.

init_processor(SpanProcessorSup, ProcessorModule, Config) ->
    %% start_link is an optional callback for processors
    case lists:member({start_link, 1}, ProcessorModule:module_info(exports)) of
        true ->
            try supervisor:start_child(SpanProcessorSup,
                                       [ProcessorModule,
                                        %% use a unique reference to distinguish multiple processors of the same type while
                                        %% still having a single name, instead of a possibly changing pid, to
                                        %% communicate with the processor
                                        maps:merge(#{name => erlang:ref_to_list(erlang:make_ref())},
                                                   Config)])
            of
                {ok, _Pid, Config1} ->
                    {true, {ProcessorModule, Config1}};
                {error, Reason} ->
                    ?LOG_INFO("Dropping span processor ~p because `processor_init/1` failed ~p", [ProcessorModule, Reason]),
                    false
            catch
                C:T:S ->
                    ?LOG_DEBUG("Dropping span processor ~p because `processor_init/1` threw an exception ~p:~p:~p", [ProcessorModule, C, T, S]),
                    ?LOG_INFO("Dropping span processor ~p because `processor_init/1` threw an exception ~p:~p", [ProcessorModule, C, T]),
                    false
            end;
        false ->
            {true, {ProcessorModule, Config}}
    end.

on_start(Processors) ->
    fun(Ctx, Span) ->
            lists:foldl(fun({P, Config}, Acc) ->
                                P:on_start(Ctx, Acc, Config)
                        end, Span, Processors)
    end.

on_end(Processors) ->
    fun(Span) ->
            lists:foldl(fun({P, Config}, Bool) ->
                                Bool andalso P:on_end(Span, Config) =:= true
                        end, true, Processors)
    end.

force_flush_({Processor, Config}) ->
    try
        Processor:force_flush(Config)
    catch
        C:T ->
            {error, {Processor, {C, T}}}
    end.
