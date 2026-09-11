%%%-------------------------------------------------------------------
%% @doc opentelemetry_experimental public API
%% @end
%%%-------------------------------------------------------------------

-module(opentelemetry_experimental_app).

-behaviour(application).

-export([start/2,
         stop/1]).

-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

start(_StartType, _StartArgs) ->
    Config = configuration(),

    SupResult = opentelemetry_experimental_sup:start_link(Config),
    case Config of
        #{sdk_disabled := true} ->
          %% skip the rest if the SDK is disabled
          SupResult;
        #{metrics_enabled := false} ->
            SupResult;
        _ ->
            Resource = otel_resource_detector:get_resource(),
            {ok, _} = otel_meter_provider_sup:start(?GLOBAL_METER_PROVIDER_NAME, Resource, Config),

            SupResult
    end.

stop(_State) ->
    ok.

%% internal functions

configuration() ->
    case otel_configuration:runtime() of
        #{configuration_source := declarative}=Configuration ->
            Configuration;
        _ ->
            %% Preserve the existing application and OS environment behavior
            %% when the stable SDK selected the legacy source.
            otel_configuration_legacy:resolve(
              application:get_all_env(opentelemetry_experimental))
    end.
