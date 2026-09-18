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
%% In-memory representation of an OpenTelemetry declarative configuration.
%% The shape follows the configuration schema and deliberately preserves the
%% difference between an absent property and a property whose value is null.
%% JSON keys and extension component properties remain binaries. Application
%% configuration may use atoms for known keys and component names.
%% @private
%%%------------------------------------------------------------------------
-module(otel_configuration_model).

-export([from_map/1,
         from_application_env/1,
         root/1,
         source/1,
         file_format/1]).

-type source() :: declarative | application_env.
-opaque t() :: #{source := source(),
                 file_format := binary(),
                 root := map()}.
-type error_reason() :: {invalid_configuration, [atom()], term()}
                      | {unsupported_file_format, term()}.

-export_type([t/0,
              error_reason/0]).

-spec from_map(map()) -> {ok, t()} | {error, error_reason()}.
from_map(Configuration) when is_map(Configuration) ->
    case find(file_format, Configuration) of
        {ok, Version} ->
            case supported_file_format(Version) of
                true ->
                    {ok, #{source => declarative,
                           file_format => version_binary(Version),
                           root => Configuration}};
                false -> {error, {unsupported_file_format, Version}}
            end;
        error ->
            {error, {invalid_configuration, [file_format], missing}}
    end;
from_map(Configuration) ->
    {error, {invalid_configuration, [], Configuration}}.

%% Application environment configuration describes the same settings using
%% idiomatic Erlang values. Since it is not itself a versioned file, use the
%% configuration version implemented by this SDK when file_format is omitted.
-spec from_application_env([{term(), term()}]) ->
          {ok, t()} | {error, error_reason()}.
from_application_env(AppEnv) when is_list(AppEnv) ->
    case first_legacy_key(AppEnv) of
        none ->
            Configuration0 = maps:from_list(AppEnv),
            Configuration = case find(file_format, Configuration0) of
                                {ok, _} -> Configuration0;
                                error -> Configuration0#{file_format => <<"1.1">>}
                            end,
            from_application_map(Configuration);
        Key ->
            {error, {invalid_configuration, [Key], legacy_configuration_not_supported}}
    end;
from_application_env(AppEnv) ->
    {error, {invalid_configuration, [], AppEnv}}.

-spec root(t()) -> map().
root(Configuration) ->
    maps:get(root, Configuration).

-spec source(t()) -> source().
source(Configuration) ->
    maps:get(source, Configuration).

-spec file_format(t()) -> binary().
file_format(Configuration) ->
    maps:get(file_format, Configuration).

from_application_map(Configuration) ->
    case from_map(Configuration) of
        {ok, Model} -> {ok, Model#{source := application_env}};
        {error, _}=Error -> Error
    end.

first_legacy_key(AppEnv) ->
    first_present(legacy_keys(), AppEnv).

first_present([], _AppEnv) -> none;
first_present([Key | Rest], AppEnv) ->
    case proplists:is_defined(Key, AppEnv) of
        true -> Key;
        false -> first_present(Rest, AppEnv)
    end.

legacy_keys() ->
    [config_file,
     sdk_disabled,
     register_loaded_applications,
     create_application_tracers,
     id_generator,
     deny_list,
     resource_detectors,
     resource_detector_timeout,
     bsp_scheduled_delay_ms,
     bsp_exporting_timeout_ms,
     bsp_max_queue_size,
     ssp_exporting_timeout_ms,
     text_map_propagators,
     traces_exporter,
     processors,
     span_processor,
     sampler,
     sweeper,
     attribute_count_limit,
     attribute_value_length_limit,
     event_count_limit,
     link_count_limit,
     attribute_per_event_limit,
     attribute_per_link_limit].

find(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, _}=Found -> Found;
        error -> maps:find(atom_to_binary(Key, utf8), Map)
    end.

supported_file_format(Version) ->
    case to_binary(Version) of
        {ok, Binary} ->
            re:run(Binary, <<"^1\\.[0-9]+(?:\\.[0-9]+)?(?:[-+].*)?$">>,
                   [{capture, none}]) =:= match;
        error ->
            false
    end.

version_binary(Version) ->
    {ok, Binary} = to_binary(Version),
    Binary.

to_binary(Value) when is_binary(Value) -> {ok, Value};
to_binary(Value) when is_atom(Value) -> {ok, atom_to_binary(Value, utf8)};
to_binary(Value) when is_list(Value) ->
    try unicode:characters_to_binary(Value) of
        Binary when is_binary(Binary) -> {ok, Binary};
        _ -> error
    catch
        _:_ -> error
    end;
to_binary(_) -> error.
