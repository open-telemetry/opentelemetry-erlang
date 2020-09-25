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
-module(ot_baggage).

-export([ctx_key/0,
         set/2,
         get_all/0,
         get_http_propagators/0]).

-type key() :: string().
-type value() :: string().

-type t() :: #{key() => value()}.

-export_type([t/0,
              key/0,
              value/0]).

-define(BAGGAGE_KEY, '$__ot_baggage_ctx_key').
-define(BAGGAGE_HEADER, <<"otcorrelations">>).

ctx_key() ->
    ?BAGGAGE_KEY.

-spec set(key(), value()) -> ok.
set(Key, Value) ->
    Baggage = ot_ctx:get_value(?BAGGAGE_KEY, #{}),
    ot_ctx:set_value(?BAGGAGE_KEY, Baggage#{Key => Value}).

-spec get_all() -> t().
get_all() ->
    ot_ctx:get_value(?BAGGAGE_KEY, #{}).

-spec get_http_propagators() -> {ot_propagation:http_extractor(), ot_propagation:http_injector()}.
get_http_propagators() ->
    ToText = fun(_Headers, Baggage) ->
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
    Inject = ot_ctx:http_injector(?BAGGAGE_KEY, ToText),
    Extract = ot_ctx:http_extractor(?BAGGAGE_KEY, FromText),
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
