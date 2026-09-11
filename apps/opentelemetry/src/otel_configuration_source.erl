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
%% Selects one configuration source at SDK startup. Declarative configuration
%% files are expected to have already undergone YAML conversion, environment
%% substitution, and JSON Schema validation outside the SDK.
%% @private
%%%------------------------------------------------------------------------
-module(otel_configuration_source).

-export([resolve/1,
         parse_file/1,
         parse_binary/1]).

-include_lib("kernel/include/logger.hrl").

-type parse_error() :: {configuration_decode_error, term()}
                     | {configuration_root_error, term()}.
-type error_reason() :: parse_error()
                      | {configuration_file_error, file:filename_all(), term()}
                      | {declarative_configuration_error,
                         file:filename_all(),
                         otel_configuration_declarative:error_reason()}.

-export_type([error_reason/0]).

-spec resolve(otel_configuration_legacy:app_env()) ->
          {ok, otel_configuration:resolved()} | {error, error_reason()}.
resolve(AppEnv) ->
    case select(AppEnv) of
        legacy ->
            {ok, otel_configuration_legacy:resolve(AppEnv)};
        {declarative, File} ->
            resolve_declarative(File)
    end.

-spec parse_file(file:filename_all()) -> {ok, map()} | {error, error_reason()}.
parse_file(File) ->
    try file:read_file(File) of
        {ok, Binary} ->
            case parse_binary(Binary) of
                {ok, _}=Result -> Result;
                {error, Reason} ->
                    {error, {configuration_file_error, File, Reason}}
            end;
        {error, Reason} ->
            {error, {configuration_file_error, File, {read_error, Reason}}}
    catch
        error:Reason ->
            {error, {configuration_file_error, File, {read_error, Reason}}}
    end.

-spec parse_binary(binary()) -> {ok, map()} | {error, error_reason()}.
parse_binary(Binary) when is_binary(Binary) ->
    case code:ensure_loaded(json) of
        {module, json} ->
            try erlang:apply(json, decode, [Binary]) of
                Configuration when is_map(Configuration) ->
                    {ok, Configuration};
                Value ->
                    {error, {configuration_root_error, Value}}
            catch
                error:Reason ->
                    {error, {configuration_decode_error, Reason}}
            end;
        {error, Reason} ->
            {error, {configuration_decode_error,
                     {json_module_unavailable, Reason}}}
    end;
parse_binary(Value) ->
    {error, {configuration_decode_error, {not_a_binary, Value}}}.

select(AppEnv) ->
    case os:getenv("OTEL_CONFIG_FILE") of
        Unset when Unset =:= false; Unset =:= "" ->
            select_legacy_file_variable(AppEnv);
        File ->
            {declarative, File}
    end.

select_legacy_file_variable(AppEnv) ->
    case os:getenv("OTEL_EXPERIMENTAL_CONFIG_FILE") of
        Unset when Unset =:= false; Unset =:= "" ->
            case proplists:get_value(config_file, AppEnv) of
                undefined -> legacy;
                ConfigFile -> {declarative, ConfigFile}
            end;
        File ->
            ?LOG_WARNING("OTEL_EXPERIMENTAL_CONFIG_FILE is deprecated; use "
                         "OTEL_CONFIG_FILE instead", []),
            {declarative, File}
    end.

resolve_declarative(File) ->
    case parse_file(File) of
        {ok, Configuration} ->
            case otel_configuration_declarative:resolve(Configuration) of
                {ok, Resolved} ->
                    {ok, Resolved};
                {error, Reason} ->
                    {error, {declarative_configuration_error, File, Reason}}
            end;
        {error, _}=Error ->
            Error
    end.
