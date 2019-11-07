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

-behaviour(gen_server).

-export([start_link/1,
         add_span_processor/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

-include("ot_tracer.hrl").

-record(state, {processors :: [module()],
                sampler :: ot_sampler:sampler()}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

add_span_processor(SpanProcessor) ->
    gen_server:call(?MODULE, {add_span_processor, SpanProcessor}).

init(Opts) ->
    {Sampler, SamplerOpts} = proplists:get_value(sampler, Opts, {always_on, #{}}),
    SamplerFun = ot_sampler:setup(Sampler, SamplerOpts),
    Processors = proplists:get_value(processors, Opts, []),

    Tracer = #tracer{module=ot_tracer_default,
                     sampler=SamplerFun,
                     processors=Processors,
                     span_module=ot_span_ets,
                     ctx_module=ot_ctx_pdict},
    opentelemetry:set_default_tracer({ot_tracer_default, Tracer}),

    %% TODO: remove this and use ctx module from tracer state everywhere
    persistent_term:put(?CTX_IMPL_KEY, ot_ctx_pdict),

    {ok, #state{sampler=SamplerFun,
                processors=Processors}}.

handle_call({add_span_processor, SpanProcessor}, _From, State=#state{processors=Processors}) ->
    %% do something with these
    {reply, ok, State#state{processors=Processors++[SpanProcessor]}};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
