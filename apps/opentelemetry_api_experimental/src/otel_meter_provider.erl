%%%------------------------------------------------------------------------
%% Copyright 2020, OpenTelemetry Authors
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
%% @doc This module defines the API for a MeterProvider. A MeterProvider
%% stores Meter configuration and is how Meters are accessed. An
%% implementation must be a `gen_server' that handles the API's calls. The
%% SDK should register a MeterProvider with the name `otel_meter_provider'
%% which is used as the default global Provider.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_meter_provider).

-export([get_meter/3,
         get_meter/4,
         resource/0,
         resource/1,
         force_flush/0,
         force_flush/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("otel_meter.hrl").

-type meter() :: term().

-spec get_meter(Name, Vsn, SchemaUrl) -> Meter when
      Name :: atom(),
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Meter:: {module(), meter()}.
get_meter(Name, Vsn, SchemaUrl) ->
    get_meter(?GLOBAL_METER_PROVIDER_NAME, Name, Vsn, SchemaUrl).

-spec get_meter(ServerRef, Name, Vsn, SchemaUrl) -> Meter when
      ServerRef :: atom() | pid(),
      Name :: atom(),
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Meter:: {module(), meter()}.
get_meter(ServerRef, Name, Vsn, SchemaUrl) ->
    try
        gen_server:call(maybe_to_reg_name(ServerRef), {get_meter, Name, Vsn, SchemaUrl})
    catch exit:{noproc, _} ->
            %% ignore get_meter because no SDK has been included and started
            {otel_meter_noop, []}
    end.

-spec resource() -> term() | undefined.
resource() ->
    resource(?GLOBAL_METER_PROVIDER_NAME).

-spec resource(atom() | pid()) -> term() | undefined.
resource(ServerRef) ->
    try
        gen_server:call(maybe_to_reg_name(ServerRef), resource)
    catch exit:{noproc, _} ->
            %% ignore because no SDK has been included and started
            undefined
    end.

-spec force_flush() -> ok | {error, term()} | timeout.
force_flush() ->
    force_flush(?GLOBAL_METER_PROVIDER_NAME).

-spec force_flush(atom() | pid()) -> ok | {error, term()} | timeout.
force_flush(ServerRef) ->
    try
        gen_server:call(maybe_to_reg_name(ServerRef), force_flush)
    catch exit:{noproc, _} ->
            %% ignore because likely no SDK has been included and started
            ok
    end.

%%

maybe_to_reg_name(Name) when is_atom(Name) ->
    ?REG_NAME(Name);
maybe_to_reg_name(Pid) when is_pid(Pid) ->
    Pid;
maybe_to_reg_name(Other) ->
    Other.
