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
%% @end
%%%-------------------------------------------------------------------------
-module(opentelemetry_experimental).

-export([start_meter_provider/2,
         set_meter/2,
         set_meter/4,
         set_meter/5,
         set_default_meter/1,
         set_default_meter/2,
         get_meter/0,
         get_meter/1]).

-include_lib("kernel/include/logger.hrl").
-include("otel_meter.hrl").

-export_type([meter/0]).

-type meter() :: {module(), term()}.

-define(METER_KEY(Name), {?MODULE, meter, Name}).
-define(METER_KEY(MeterProvider, Name), {?MODULE, MeterProvider, meter, Name}).
-define(DEFAULT_METER_KEY(MeterProvider), ?METER_KEY(MeterProvider, '$__default_meter')).

-spec start_meter_provider(atom(), map()) -> {ok, pid() | undefined} | {error, term()}.
start_meter_provider(Name, Config) ->
    otel_meter_provider:start(Name, Config).

-spec set_default_meter(meter()) -> boolean().
set_default_meter(Meter) ->
    set_default_meter(?GLOBAL_METER_PROVIDER_NAME, Meter).

-spec set_default_meter(atom(), meter()) -> boolean().
set_default_meter(MeterProvider, Meter) ->
    opentelemetry:verify_and_set_term(Meter, ?DEFAULT_METER_KEY(MeterProvider), otel_meter).

-spec get_meter() -> meter().
get_meter() ->
    get_meter_(?GLOBAL_METER_PROVIDER_NAME).

-spec get_meter_(atom()) -> meter().
get_meter_(MeterProvider) ->
    persistent_term:get(?DEFAULT_METER_KEY(MeterProvider), {otel_meter_noop, []}).

-spec get_meter(Name) -> Meter when
      Name :: atom() | {atom(), Vsn, SchemaUrl},
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Meter:: meter().
get_meter('$__default_meter') ->
    get_meter();
get_meter({Name, Vsn, SchemaUrl}) ->
    get_meter(Name, Vsn, SchemaUrl);
get_meter(Name) ->
    get_meter(Name, undefined, undefined).

-spec get_meter(Name, Vsn, SchemaUrl) -> Meter when
      Name :: atom(),
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Meter:: meter().
get_meter(Name, Vsn, SchemaUrl) ->
    get_meter(?GLOBAL_METER_PROVIDER_NAME, Name, Vsn, SchemaUrl).

-spec get_meter(MeterProvider, Name, Vsn, SchemaUrl) -> Meter when
      MeterProvider :: atom() | pid(),
      Name :: atom(),
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Meter:: meter().
get_meter(MeterProvider, Name, Vsn, SchemaUrl) ->
    %% check cache and then use provider to get the meter if it isn't cached yet
    case persistent_term:get(?METER_KEY(MeterProvider, {Name, Vsn, SchemaUrl}), undefined) of
        undefined ->
            VsnBin = opentelemetry:vsn_to_binary(Vsn),
            Meter = otel_meter_provider:get_meter(MeterProvider, Name, VsnBin, SchemaUrl),

            %% cache the meter
            _ = set_meter(Name, Vsn, SchemaUrl, Meter),

            Meter;
        Meter ->
            Meter
    end.

-spec set_meter(atom(), meter()) -> boolean().
set_meter(Name, Meter) ->
    set_meter(Name, <<>>, undefined, Meter).

-spec set_meter(Name, Vsn, SchemaUrl, Meter) -> boolean() when
      Name :: atom(),
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Meter:: meter().
set_meter(Name, Vsn, SchemaUrl, Meter) ->
    set_meter(?GLOBAL_METER_PROVIDER_NAME, Name, Vsn, SchemaUrl, Meter).

-spec set_meter(MeterProvider, Name, Vsn, SchemaUrl, Meter) -> boolean() when
      MeterProvider :: atom(),
      Name :: atom(),
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Meter:: meter().
set_meter(MeterProvider, Name, Vsn, SchemaUrl, Meter) ->
    opentelemetry:verify_and_set_term(Meter, ?METER_KEY(MeterProvider, {Name, Vsn, SchemaUrl}), otel_meter).
