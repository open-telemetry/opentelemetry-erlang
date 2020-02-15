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
-module(ot_metric_sup).

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

    MeterServer = #{id => ot_meter_server,
                    start => {ot_meter_provider, start_link, [ot_meter_server, Opts]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [ot_meter_provider, ot_meter_server]},

    Meter = #{id => ot_meter,
              start => {ot_meter_default, start_link, [Opts]},
              restart => permanent,
              shutdown => 5000,
              type => worker,
              modules => [ot_meter]},

    MetricAccumulator = #{id => ot_metric_accumulator,
                          start => {ot_metric_accumulator, start_link, [Opts]},
                          restart => permanent,
                          shutdown => 5000,
                          type => worker,
                          modules => [ot_metric_accumulator]},

    MetricIntegrator = #{id => ot_metric_integrator,
                         start => {ot_metric_integrator, start_link, [Opts]},
                         restart => permanent,
                         shutdown => 5000,
                         type => worker,
                         modules => [ot_metric_integrator]},

    MetricExporter = #{id => ot_metric_exporter,
                       start => {ot_metric_exporter, start_link, [Opts]},
                       restart => permanent,
                       shutdown => 5000,
                       type => worker,
                       modules => [ot_metric_exporter]},

    %% TODO: remove this, it should not be enabled by default
    %% ControllerOpts = proplists:get_value(controller, Opts, #{}),
    %% Controller = #{id => ot_metric_controller,
    %%                start => {ot_metric_controller_push, start_link, [ControllerOpts]},
    %%                restart => permanent,
    %%                shutdown => 5000,
    %%                type => worker,
    %%                modules => [ot_metric_controller_push]},

    ChildSpecs = [MeterServer, Meter, MetricExporter, MetricIntegrator, MetricAccumulator],
    {ok, {SupFlags, ChildSpecs}}.
