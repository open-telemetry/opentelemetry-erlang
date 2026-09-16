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
%% @doc All measurements are associated with an instrument.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_instrument).

-export([new/5,
         new/7,
         register_alias/2,
         lookup_alias/1,
         unregister_alias/1,
         record/4,
         id/1,
         identity/1,
         normalized_name/1,
         is_monotonic/1,
         temporality/1,
         kind_temporality/1]).

-include("otel_metrics.hrl").

-type name() :: atom().
-type alias() :: atom().
-type description() :: unicode:unicode_binary().
-type kind() :: ?KIND_COUNTER | ?KIND_OBSERVABLE_COUNTER | ?KIND_HISTOGRAM |
                ?KIND_OBSERVABLE_GAUGE | ?KIND_UPDOWN_COUNTER | ?KIND_OBSERVABLE_UPDOWNCOUNTER.
-type unit() :: atom(). %% latin1, maximum length of 63 characters
-type observation() :: {number(), opentelemetry:attributes_map()}.
-type named_observations() :: {t() | alias(), [observation()]}.
-type callback_args() :: term().
-type callback_result() :: [observation()] |
                           [named_observations()].
-type callback() :: fun((callback_args()) -> callback_result()).

-type temporality() :: ?TEMPORALITY_DELTA | ?TEMPORALITY_CUMULATIVE.

-type advisory_params() :: #{explicit_bucket_boundaries => [number(), ...]}.

-type opts() :: #{description => description() | undefined,
                  unit => unit() | undefined,
                  advisory_params => advisory_params() | undefined}.

-type t() :: #instrument{}.

-export_type([t/0,
              name/0,
              alias/0,
              description/0,
              kind/0,
              unit/0,
              temporality/0,
              observation/0,
              named_observations/0,
              callback/0,
              callback_args/0,
              callback_result/0,
              advisory_params/0,
              opts/0]).

-spec new(module(), otel_meter:t(), kind(), name(), opts()) -> t().
new(Module, Meter, Kind, Name, Opts) ->
    Description = maps:get(description, Opts, undefined),
    Unit = maps:get(unit, Opts, undefined),
    AdvisoryParams = maps:get(advisory_params, Opts, undefined),
    #instrument{id              = make_ref(),
                module          = Module,
                meter           = Meter,
                name            = Name,
                description     = Description,
                temporality     = ?TEMPORALITY_DELTA,
                kind            = Kind,
                unit            = Unit,
                advisory_params = AdvisoryParams}.

-spec new(module(), otel_meter:t(), kind(), name(), callback(), callback_args(), opts()) -> t().
new(Module, Meter, Kind, Name, Callback, CallbackArgs, Opts) ->
    Description = maps:get(description, Opts, undefined),
    Unit = maps:get(unit, Opts, undefined),
    AdvisoryParams = maps:get(advisory_params, Opts, undefined),
    #instrument{id              = make_ref(),
                module          = Module,
                meter           = Meter,
                name            = Name,
                description     = Description,
                kind            = Kind,
                unit            = Unit,
                temporality     = ?TEMPORALITY_CUMULATIVE,
                callback        = Callback,
                callback_args   = CallbackArgs,
                advisory_params = AdvisoryParams}.

-spec register_alias(alias(), t()) -> {ok, t()} | {error, term()}.
register_alias(Alias, Instrument=#instrument{}) when is_atom(Alias) ->
    otel_instrument_registry:register(Alias, Instrument);
register_alias(Alias, _Instrument) ->
    {error, {invalid_instrument_alias, Alias}}.

-spec lookup_alias(alias()) -> {ok, t()} | error.
lookup_alias(Alias) when is_atom(Alias) ->
    otel_instrument_registry:lookup(Alias);
lookup_alias(_) ->
    error.

-spec unregister_alias(alias()) -> ok.
unregister_alias(Alias) when is_atom(Alias) ->
    otel_instrument_registry:unregister(Alias).

-spec record(otel_ctx:t(), t(), number(), opentelemetry:attributes_map()) -> ok | false.
record(Ctx, Instrument=#instrument{module=Module}, Number, Attributes) ->
    Module:record(Ctx, Instrument, Number, Attributes).

-spec id(t()) -> reference().
id(#instrument{id=Id}) ->
    Id.

-spec identity(t()) -> term().
identity(#instrument{meter=Meter,
                     name=Name,
                     kind=Kind,
                     unit=Unit,
                     description=Description}) ->
    {Meter,
     normalized_name(Name),
     Kind,
     normalize_optional(Unit),
     normalize_optional(Description)}.

-spec normalized_name(name() | binary()) -> binary().
normalized_name(Name) when is_atom(Name) ->
    normalized_name(atom_to_binary(Name, utf8));
normalized_name(Name) when is_binary(Name) ->
    string:lowercase(Name).

normalize_optional(undefined) ->
    <<>>;
normalize_optional(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
normalize_optional(Value) ->
    Value.

is_monotonic(#instrument{kind=?KIND_COUNTER}) ->
    true;
is_monotonic(#instrument{kind=?KIND_OBSERVABLE_COUNTER}) ->
    true;
is_monotonic(#instrument{kind=?KIND_HISTOGRAM}) ->
    true;
is_monotonic(_) ->
    false.

temporality(#instrument{kind=Kind}) ->
    kind_temporality(Kind).

kind_temporality(?KIND_COUNTER) ->
    ?TEMPORALITY_DELTA;
kind_temporality(?KIND_OBSERVABLE_COUNTER) ->
    ?TEMPORALITY_CUMULATIVE;
kind_temporality(?KIND_UPDOWN_COUNTER) ->
    ?TEMPORALITY_DELTA;
kind_temporality(?KIND_OBSERVABLE_UPDOWNCOUNTER) ->
    ?TEMPORALITY_CUMULATIVE;
kind_temporality(?KIND_HISTOGRAM) ->
    ?TEMPORALITY_DELTA;
kind_temporality(?KIND_OBSERVABLE_GAUGE) ->
    ?TEMPORALITY_CUMULATIVE.
