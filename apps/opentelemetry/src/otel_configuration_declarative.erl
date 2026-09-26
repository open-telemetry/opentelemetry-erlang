%%%------------------------------------------------------------------------
%% Copyright 2026, OpenTelemetry Authors
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
%% Entry point for preprocessed OpenTelemetry declarative configuration.
%% Conversion from YAML, environment substitution, and JSON Schema validation
%% deliberately happen before configuration reaches this module.
%% @private
%%%------------------------------------------------------------------------
-module(otel_configuration_declarative).

-export([parse/1,
         resolve/1]).

-type error_reason() :: otel_configuration_model:error_reason()
                      | otel_configuration_sdk:error_reason().

-export_type([error_reason/0]).

-spec parse(map()) ->
          {ok, otel_configuration_model:t()} |
          {error, otel_configuration_model:error_reason()}.
parse(Configuration) ->
    otel_configuration_model:from_map(Configuration).

%% Kept as a convenient parse-and-create entry point for callers and tests.
-spec resolve(map()) ->
          {ok, otel_configuration_sdk:configuration()} | {error, error_reason()}.
resolve(Configuration) ->
    case parse(Configuration) of
        {ok, Model} -> otel_configuration_sdk:create(Model);
        {error, _}=Error -> Error
    end.
