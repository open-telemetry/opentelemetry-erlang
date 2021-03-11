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
%% @doc Supervisor for processes belonging to the Metric SDK.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_metric_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]).

init([Opts]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    MeterServer = #{id => otel_meter_server,
                    start => {otel_meter_provider, start_link, [otel_meter_server, Opts]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [otel_meter_provider, otel_meter_server]},

    Meter = #{id => otel_meter,
              start => {otel_meter_default, start_link, [Opts]},
              restart => permanent,
              shutdown => 5000,
              type => worker,
              modules => [otel_meter]},

    MetricAccumulator = #{id => otel_metric_accumulator,
                          start => {otel_metric_accumulator, start_link, [Opts]},
                          restart => permanent,
                          shutdown => 5000,
                          type => worker,
                          modules => [otel_metric_accumulator]},

    MetricIntegrator = #{id => otel_metric_integrator,
                         start => {otel_metric_integrator, start_link, [Opts]},
                         restart => permanent,
                         shutdown => 5000,
                         type => worker,
                         modules => [otel_metric_integrator]},

    MetricExporter = #{id => otel_metric_exporter,
                       start => {otel_metric_exporter, start_link, [Opts]},
                       restart => permanent,
                       shutdown => 5000,
                       type => worker,
                       modules => [otel_metric_exporter]},

    %% TODO: remove this, it should not be enabled by default
    %% ControllerOpts = proplists:get_value(controller, Opts, #{}),
    %% Controller = #{id => otel_metric_controller,
    %%                start => {otel_metric_controller_push, start_link, [ControllerOpts]},
    %%                restart => permanent,
    %%                shutdown => 5000,
    %%                type => worker,
    %%                modules => [otel_metric_controller_push]},

    ChildSpecs = [MeterServer, Meter, MetricExporter, MetricIntegrator, MetricAccumulator],
    {ok, {SupFlags, ChildSpecs}}.
