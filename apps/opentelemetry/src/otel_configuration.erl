%%%------------------------------------------------------------------------
%% Copyright 2021, OpenTelemetry Authors
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
%% Resolves normalized configuration overrides into the concrete runtime
%% configuration consumed by the SDK.
%% @private
%%%-------------------------------------------------------------------------
-module(otel_configuration).

-export([resolve/1,
         defaults/0,
         span_processor_defaults/1,
         store_runtime/1,
         runtime/0,
         clear_runtime/0,
         merge_with_os/1,
         merge_list_with_environment/3,
         report_cb/1]).

-type resolved() :: #{configuration_source := legacy | declarative,
                      sdk_disabled := boolean(),
                      traces_enabled := boolean(),
                      metrics_enabled := boolean(),
                      log_level := atom(),
                      register_loaded_applications := boolean() | undefined,
                      create_application_tracers := boolean(),
                      id_generator := module(),
                      deny_list := [atom()],

                      resource_detectors := [module()],
                      resource_detector_timeout := integer(),
                      resource := otel_resource:t(),
                      bsp_scheduled_delay_ms := integer() | undefined,
                      bsp_exporting_timeout_ms := integer() | undefined,
                      bsp_max_queue_size := integer() | undefined,
                      ssp_exporting_timeout_ms := integer() | undefined,
                      text_map_propagators := [atom()],
                      traces_exporter := {atom(), term()} | none | undefined,
                      metrics_exporter := {atom(), term()} | none | undefined,
                      views := list(), %% TODO: type should be `[otel_meter_server:view_config]'
                                       %% when Metrics are moved out of the experimental app
                      readers := [#{module := module(), config := map()}],
                      exemplars_enabled := boolean(),
                      exemplar_filter := always_on | always_off | trace_based,
                      metric_producers := [{module(), term()}],

                      processors := list(),
                      sampler := {atom(), term()},
                      sweeper := #{interval => integer() | infinity,
                                   strategy => atom() | fun(),
                                   span_ttl => integer() | infinity,
                                   storage_size => integer() | infinity},
                      attribute_count_limit := integer(),
                      attribute_value_length_limit := integer() | infinity,
                      event_count_limit := integer(),
                      link_count_limit := integer(),
                      attribute_per_event_limit := integer(),
                      attribute_per_link_limit := integer()}.

-type t() :: resolved().
-type overrides() :: map().

-export_type([t/0,
              resolved/0,
              overrides/0]).

-spec resolve(overrides()) -> resolved().
resolve(Overrides) ->
    maps:merge(defaults(), Overrides).

-spec defaults() -> resolved().
defaults() ->
    #{configuration_source => legacy,
      sdk_disabled => false,
      traces_enabled => true,
      metrics_enabled => true,
      log_level => info,
      register_loaded_applications => undefined,
      create_application_tracers => true,
      id_generator => otel_id_generator,
      deny_list => [],
      resource_detectors => [otel_resource_env_var,
                             otel_resource_app_env],
      resource_detector_timeout => 5000,
      resource => undefined,
      bsp_scheduled_delay_ms => undefined,
      bsp_exporting_timeout_ms => undefined,
      bsp_max_queue_size => undefined,
      ssp_exporting_timeout_ms => undefined,
      text_map_propagators => [trace_context, baggage],
      traces_exporter => {opentelemetry_exporter, #{}},
      metrics_exporter => {opentelemetry_exporter, #{}},
      views => [],
      readers => [],
      exemplars_enabled => false,
      exemplar_filter => trace_based,
      metric_producers => [],
      processors => [{otel_batch_processor,
                      span_processor_defaults(otel_batch_processor)}],
      sampler => {parent_based, #{root => always_on}},
      sweeper => #{interval => timer:minutes(10),
                   strategy => drop,
                   span_ttl => timer:minutes(30),
                   storage_size => infinity},
      attribute_count_limit => 128,
      attribute_value_length_limit => infinity,
      event_count_limit => 128,
      link_count_limit => 128,
      attribute_per_event_limit => 128,
      attribute_per_link_limit => 128}.

%% The stable and experimental SDK applications start separately, but a
%% declarative document configures both. Store the immutable resolved value so
%% the experimental application consumes the exact same configuration.
-spec store_runtime(resolved()) -> ok.
store_runtime(Configuration) ->
    persistent_term:put({?MODULE, runtime}, Configuration).

-spec runtime() -> resolved() | undefined.
runtime() ->
    persistent_term:get({?MODULE, runtime}, undefined).

-spec clear_runtime() -> ok.
clear_runtime() ->
    _ = persistent_term:erase({?MODULE, runtime}),
    ok.

-spec span_processor_defaults(otel_batch_processor | otel_simple_processor) -> map().
span_processor_defaults(otel_batch_processor) ->
    #{scheduled_delay_ms => 5000,
      exporting_timeout_ms => 30000,
      max_queue_size => 2048,
      exporter => {opentelemetry_exporter, #{}}};
span_processor_defaults(otel_simple_processor) ->
    #{exporting_timeout_ms => 30000,
      exporter => {opentelemetry_exporter, #{}}}.

%% Compatibility entry points for callers of the previous private API.
-spec merge_with_os(otel_configuration_legacy:app_env()) -> resolved().
merge_with_os(AppEnv) ->
    otel_configuration_legacy:resolve(AppEnv).

-spec merge_list_with_environment([{string(), atom(), atom()}],
                                  otel_configuration_legacy:app_env(),
                                  map()) -> map().
merge_list_with_environment(ConfigMappings, AppEnv, ConfigMap) ->
    otel_configuration_legacy:merge_list_with_environment(ConfigMappings, AppEnv, ConfigMap).

report_cb(Report) ->
    otel_configuration_legacy:report_cb(Report).
