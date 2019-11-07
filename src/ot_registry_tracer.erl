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
-module(ot_registry_tracer).

-behaviour(gen_server).

-export([start_link/1,
         get/0,
         get/1,
         get_sampler/0,
         on_start/1,
         on_end/1,
         add_span_processor/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

-include("ot_tracer.hrl").

-record(state, {processors :: [module()],
                sampler :: ot_sampler:sampler()}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

get() ->
    ot_tracer_default.

get(_Name) ->
    ot_tracer_default.

get_sampler() ->
    persistent_term:get({?MODULE, sampler}).

on_start(Span) ->
    call_processors(on_start, Span).

on_end(Span) ->
    call_processors(on_end, Span).

call_processors(F, Span) ->
    Processors = persistent_term:get({?MODULE, processors}, []),
    [P:F(Span, Config) || {P, Config} <- Processors].

add_span_processor(SpanProcessor) ->
    gen_server:call(?MODULE, {add_span_processor, SpanProcessor}).

init(Opts) ->
    process_flag(trap_exit, true),

    {Sampler, SamplerOpts} = proplists:get_value(sampler, Opts, {always_on, #{}}),
    SamplerFun = ot_sampler:setup(Sampler, SamplerOpts),
    persistent_term:put({?MODULE, sampler}, SamplerFun),

    opentelemetry:set_default_tracer_registry(?MODULE),

    Processors = proplists:get_value(processors, Opts, []),
    persistent_term:put({?MODULE, processors}, Processors),

    %% DO NOT LIKE
    persistent_term:put(?CTX_IMPL_KEY, ot_ctx_pdict),
    persistent_term:put(?SPAN_IMPL_KEY, ot_span_ets),

    {ok, #state{sampler=SamplerFun,
                processors=Processors}}.

handle_call({add_span_processor, SpanProcessor}, _From, State=#state{processors=Processors}) ->
    %% do something with these
    {reply, ok, State#state{processors=Processors++[SpanProcessor]}};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
