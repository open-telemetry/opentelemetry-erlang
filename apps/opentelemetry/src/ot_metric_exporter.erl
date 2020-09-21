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
%%
%% @end
%%%-------------------------------------------------------------------------
-module(ot_metric_exporter).

-behaviour(gen_server).

-export([start_link/1,
         export/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-record(state, {exporter :: {module(), term()}}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

export(Records) ->
    {Module, Args} = gen_server:call(?MODULE, exporter),
    erlang:apply(Module, export, [Records | Args]).

init(Opts) ->
    Exporter = proplists:get_value(metric_exporter, Opts, {ot_metric_exporter_stdout, []}),
    {ok, #state{exporter=Exporter}}.

handle_call(exporter, _From, State=#state{exporter=Exporter}) ->
    {reply, Exporter, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
