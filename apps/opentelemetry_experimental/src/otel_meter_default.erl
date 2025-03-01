%%%------------------------------------------------------------------------
%% Copyright 2022, OpenTelemetry Authors
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
-module(otel_meter_default).

-behaviour(otel_meter).

-export([create_instrument/4,
         create_instrument/6,
         lookup_instrument/2,
         register_callback/4,
         scope/1]).

-export([record/5]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_metrics.hrl").

-define(INSTRUMENT_NAME_REGEX, "^[A-Za-z]+[A-Za-z0-9/_.\-]{0,254}$").

-spec create_instrument(otel_meter:t(), otel_instrument:name(), otel_instrument:kind(), otel_instrument:opts()) -> otel_instrument:t().
create_instrument(Meter, Name, Kind, Opts) ->
    validate_name(Name),
    ValidatedOpts = validate_opts(Name, Kind, Opts),
    Instrument=#instrument{meter={_, #meter{provider=Provider}}} =
        otel_instrument:new(?MODULE, Meter, Kind, Name, ValidatedOpts),
    _ = otel_meter_server:add_instrument(Provider, Instrument),
    Instrument.

lookup_instrument({_, Meter=#meter{instruments_tab=InstrumentsTab}}, Name) ->
    otel_metrics_tables:lookup_instrument(InstrumentsTab, Meter, Name).

-spec create_instrument(otel_meter:t(), otel_instrument:name(), otel_instrument:kind(), otel_instrument:callback(), otel_instrument:callback_args(), otel_instrument:opts()) -> otel_instrument:t().
create_instrument(Meter, Name, Kind, Callback, CallbackArgs, Opts) ->
    validate_name(Name),
    ValidatedOpts = validate_opts(Name, Kind, Opts),
    Instrument=#instrument{meter={_, #meter{provider=Provider}}} =
        otel_instrument:new(?MODULE, Meter, Kind, Name, Callback, CallbackArgs, ValidatedOpts),
    _ = otel_meter_server:add_instrument(Provider, Instrument),
    Instrument.

register_callback({_, #meter{provider=Provider}}, Instruments, Callback, CallbackArgs) ->
    otel_meter_server:register_callback(Provider, Instruments, Callback, CallbackArgs);
register_callback(_, _, _, _) ->
    ok.

-spec scope({module(), #meter{}} | #meter{}) -> opentelemetry:instrumentation_scope().
scope({_, #meter{instrumentation_scope=Scope}}) ->
    Scope;
scope(#meter{instrumentation_scope=Scope}) ->
    Scope.

validate_name(Name) when is_atom(Name) ->
    NameString = atom_to_list(Name),
    case re:run(NameString, ?INSTRUMENT_NAME_REGEX, [{capture, none}]) of
        match ->
            ok;
        nomatch ->
            ?LOG_ERROR("Invalid instrument name, should be an atom matching '~s', but got '~s'", [?INSTRUMENT_NAME_REGEX, NameString]),
            ok
    end;
validate_name(Name) ->
    ?LOG_ERROR("Invalid instrument name, should be an atom matching '~s', but got ~p", [?INSTRUMENT_NAME_REGEX, Name]),
    ok.

validate_opts(Name, Kind, #{advisory_params := AdvisoryParams} = Opts) ->
    % switch to maps:filtermap when we support only 24 onwards
    ValidatedAdvisoryParams = maps:from_list(lists:filtermap(fun({Key, Value}) -> validate_advisory_param(Name, Kind, Key, Value) end,  maps:to_list(AdvisoryParams))),
    maps:put(advisory_params, ValidatedAdvisoryParams, Opts);
validate_opts(_Name, _Kind, Opts) ->
    Opts.

validate_advisory_param(Name, ?KIND_HISTOGRAM, explicit_bucket_boundaries, Value) ->
    validate_explicit_bucket_boundaries(Name, Value);
validate_advisory_param(Name, _Kind, explicit_bucket_boundaries, _Value) ->
    ?LOG_WARNING("[instrument '~s'] 'explicit_bucket_boundaries' advisory parameter is allowed only for histograms, ignoring", [Name]),
    false;
validate_advisory_param(Name, _Kind, Opt, _Value) ->
    ?LOG_WARNING("[instrument '~s'] '~s' advisory parameter is not supported, ignoring", [Name, Opt]),
    false.

%% empty list denotes a single bucket histogram that can be used for things like summaries
validate_explicit_bucket_boundaries(_Name, []) ->
    {true, {explicit_bucket_boundaries, []}};
validate_explicit_bucket_boundaries(Name, [_ | _] = Value) ->
    case lists:all(fun is_number/1, Value) and (lists:sort(Value) == Value) of
        true ->
            {true, {explicit_bucket_boundaries, Value}};
        false ->
            ?LOG_WARNING("[instrument '~s'] 'explicit_bucket_boundaries' advisory parameter should be a not empty ordered list of numbers, got ~p", [Name, Value]),
            false
    end;
validate_explicit_bucket_boundaries(Name, Value) ->
    ?LOG_WARNING("[instrument '~s'] 'explicit_bucket_boundaries' advisory parameter should be a not empty ordered list of numbers, got ~p", [Name, Value]),
    false.

%%

record(Ctx, Meter, Name, Number, Attributes) ->
    otel_meter_server:record(Ctx, Meter, Name, Number, Attributes).
