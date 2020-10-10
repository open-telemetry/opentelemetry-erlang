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
%% @doc Baggage is used to annotate telemetry, adding context and
%% information to metrics, traces, and logs. It is represented by a set
%% of name/value pairs describing user-defined properties.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_baggage).

-export([set/1,
         set/2,
         get_all/0,
         clear/0,
         get_text_map_propagators/0]).

-type key() :: string().
-type value() :: string().

-type t() :: #{key() => value()}.

-export_type([t/0,
              key/0,
              value/0]).

-define(BAGGAGE_KEY, '$__otel_baggage_ctx_key').
-define(BAGGAGE_HEADER, <<"baggage">>).

-spec set(#{key() => value()} | [{key(), value()}]) -> ok.
set(KeyValues) when is_list(KeyValues) ->
    set(maps:from_list(KeyValues));
set(KeyValues) when is_map(KeyValues) ->
    Baggage = otel_ctx:get_value(?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(?BAGGAGE_KEY, maps:merge(Baggage, KeyValues)).

-spec set(key(), value()) -> ok.
set(Key, Value) ->
    Baggage = otel_ctx:get_value(?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(?BAGGAGE_KEY, Baggage#{Key => Value}).

-spec get_all() -> t().
get_all() ->
    otel_ctx:get_value(?BAGGAGE_KEY, #{}).

-spec clear() -> ok.
clear() ->
    otel_ctx:set_value(?BAGGAGE_KEY, #{}).

-spec get_text_map_propagators() -> {otel_propagator:text_map_extractor(), otel_propagator:text_map_injector()}.
get_text_map_propagators() ->
    ToText = fun(Baggage) ->
                     case maps:fold(fun(Key, Value, Acc) ->
                                            [$,, [Key, "=", Value] | Acc]
                                    end, [], Baggage) of
                         [$, | List] ->
                             [{?BAGGAGE_HEADER, unicode:characters_to_list(List)}];
                         _ ->
                             []
                     end
             end,
    FromText = fun(Headers, CurrentBaggage) ->
                       case lookup(?BAGGAGE_HEADER, Headers) of
                           undefined ->
                               CurrentBaggage;
                           String ->
                               Pairs = string:lexemes(String, [$,]),
                               lists:foldl(fun(Pair, Acc) ->
                                                   [Key, Value] = string:split(Pair, "="),
                                                   Acc#{unicode:characters_to_list(Key) =>
                                                            unicode:characters_to_list(Value)}
                                           end, CurrentBaggage, Pairs)
                       end
               end,
    Inject = otel_ctx:text_map_injector(?BAGGAGE_KEY, ToText),
    Extract = otel_ctx:text_map_extractor(?BAGGAGE_KEY, FromText),
    {Extract, Inject}.

%% find a header in a list, ignoring case
lookup(_, []) ->
    undefined;
lookup(Header, [{H, Value} | Rest]) ->
    case string:equal(Header, H, true, none) of
        true ->
            Value;
        false ->
            lookup(Header, Rest)
    end.
