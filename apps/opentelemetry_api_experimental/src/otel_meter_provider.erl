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
         resource/0,
         resource/1,
         force_flush/0,
         force_flush/1]).

-spec get_meter(Name, Vsn, SchemaUrl) -> Meter when
      Name :: atom(),
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Meter:: opentelemetry:meter().
get_meter(Name, Vsn, SchemaUrl) ->
    get_meter(?MODULE, Name, Vsn, SchemaUrl).

-spec get_meter(ServerRef, Name, Vsn, SchemaUrl) -> Meter when
      ServerRef :: atom() | pid(),
      Name :: atom(),
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Meter:: opentelemetry:meter().
get_meter(ServerRef, Name, Vsn, SchemaUrl) ->
    try
        gen_server:call(ServerRef, {get_meter, Name, Vsn, SchemaUrl})
    catch exit:{noproc, _} ->
            %% ignore get_meter because no SDK has been included and started
            false
    end.

-spec resource() -> term() | undefined.
resource() ->
    resource(?MODULE).

-spec resource(atom() | pid()) -> term() | undefined.
resource(ServerRef) ->
    try
        gen_server:call(ServerRef, resource)
    catch exit:{noproc, _} ->
            %% ignore because no SDK has been included and started
            undefined
    end.

-spec force_flush() -> ok | {error, term()} | timeout.
force_flush() ->
    force_flush(?MODULE).

-spec force_flush(atom() | pid()) -> ok | {error, term()} | timeout.
force_flush(ServerRef) ->
    try
        gen_server:call(ServerRef, force_flush)
    catch exit:{noproc, _} ->
            %% ignore because likely no SDK has been included and started
            ok
    end.
