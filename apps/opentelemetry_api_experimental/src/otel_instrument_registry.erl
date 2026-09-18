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
%% @doc Node-local aliases for Metrics instruments.
%%
%% Aliases are an Erlang/Elixir convenience and are not part of OpenTelemetry
%% instrument identity. They are expected to be registered rarely and read on
%% the measurement hot path, so each alias is stored under its own
%% `persistent_term' key.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_instrument_registry).

-export([register/2,
         lookup/1,
         unregister/1]).

-include("otel_metrics.hrl").

-define(ALIAS_KEY(Alias), {?MODULE, Alias}).

%% global:trans has infinity retries so will always return the result of fun
-eqwalizer({nowarn_function, register/2}).

-spec register(otel_instrument:alias(), otel_instrument:t()) ->
          {ok, otel_instrument:t()} | {error, term()}.
register(Alias, Instrument=#instrument{}) when is_atom(Alias) ->
    global:trans({{?MODULE, Alias}, self()},
                 fun() -> register_(Alias, Instrument) end,
                 [node()]).

register_(Alias, Instrument) ->
    Key = ?ALIAS_KEY(Alias),
    case persistent_term:get(Key, undefined) of
        undefined ->
            persistent_term:put(Key, Instrument),
            {ok, Instrument};
        Existing ->
            case otel_instrument:identity(Existing) =:= otel_instrument:identity(Instrument) of
                true ->
                    %% Refresh the handle so an idempotent registration after a
                    %% provider restart replaces a stale implementation token.
                    persistent_term:put(Key, Instrument),
                    {ok, Instrument};
                false ->
                    {error, {alias_conflict, Alias, Existing, Instrument}}
            end
    end.

-spec lookup(otel_instrument:alias()) -> {ok, otel_instrument:t()} | error.
lookup(Alias) when is_atom(Alias) ->
    case persistent_term:get(?ALIAS_KEY(Alias), undefined) of
        undefined -> error;
        Instrument -> {ok, Instrument}
    end.

-spec unregister(otel_instrument:alias()) -> ok.
unregister(Alias) when is_atom(Alias) ->
    persistent_term:erase(?ALIAS_KEY(Alias)),
    ok.
