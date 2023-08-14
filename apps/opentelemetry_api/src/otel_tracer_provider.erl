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
%% @doc This module defines the API for a TracerProvider. A TracerProvider
%% stores Tracer configuration and is how Tracers are accessed. An
%% implementation must be a `gen_server' that handles the API's calls. The
%% SDK should register a TracerProvider with the name `otel_tracer_provider'
%% which is used as the default global Provider.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_tracer_provider).

-export([start/2,
         get_tracer/3,
         get_tracer/4,
         resource/0,
         resource/1,
         force_flush/0,
         force_flush/1]).

-include("opentelemetry.hrl").

%% uncomment when OTP-23 becomes the minimum required version
%% -deprecated({start, 2, "start the TracerProvider through the SDK"}).

%% @deprecated Start the TracerProvider through the SDK
start(Name, Config) ->
    otel_tracer_provider_sup:start(Name, Config).

-spec get_tracer(Name, Vsn, SchemaUrl) -> Tracer when
      Name :: atom(),
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Tracer:: opentelemetry:tracer().
get_tracer(Name, Vsn, SchemaUrl) ->
    get_tracer(?GLOBAL_TRACER_PROVIDER_NAME, Name, Vsn, SchemaUrl).

-spec get_tracer(ServerRef, Name, Vsn, SchemaUrl) -> Tracer when
      ServerRef :: atom() | pid() | string(),
      Name :: atom(),
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Tracer:: opentelemetry:tracer().
get_tracer(ServerRef, Name, Vsn, SchemaUrl) ->
    try
        %% update atom name to the registered name
        Server = maybe_to_reg_name(ServerRef),
        gen_server:call(Server, {get_tracer, Name, Vsn, SchemaUrl})
    catch exit:{noproc, _} ->
            %% ignore get_tracer because no SDK has been included and started
            {otel_tracer_noop, []}
    end.

-spec resource() -> term() | undefined.
resource() ->
    resource(?GLOBAL_TRACER_PROVIDER_NAME).

-spec resource(atom() | pid() | string()) -> term() | undefined.
resource(ServerRef) ->
    try
        Server = maybe_to_reg_name(ServerRef),
        gen_server:call(Server, resource)
    catch exit:{noproc, _} ->
            %% ignore because no SDK has been included and started
            undefined
    end.

-spec force_flush() -> ok | {error, term()} | timeout.
force_flush() ->
    force_flush(?GLOBAL_TRACER_PROVIDER_NAME).

-spec force_flush(atom() | pid()) -> ok | {error, term()} | timeout.
force_flush(ServerRef) ->
    try
        Server = maybe_to_reg_name(ServerRef),
        gen_server:call(Server, force_flush)
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
