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
         get/1,
         remove/1,
         clear/0,
         get_http_propagators/0,
         from_text/2,
         to_text/2]).

-type key() :: string().
-type value() :: string().

-export_type([key/0,
              value/0]).

-define(BAGGAGE_KEY, '$__ot_baggage_ctx_key').
-define(BAGGAGE_HEADER, <<"Baggage-Context">>).
ctx_key() ->
    ?BAGGAGE_KEY.

-spec set(key(), value()) -> ok.
set(Key, Value) ->
    ot_ctx:set_value(?BAGGAGE_KEY, Key, Value).

-spec get(key()) -> value().
get(Key)->
    ot_ctx:get_value(?BAGGAGE_KEY, Key).

-spec remove(key()) -> ok.
remove(Key) ->
    ot_ctx:remove(?BAGGAGE_KEY, Key).

-spec clear() -> ok.
clear() ->
    ot_ctx:clear(?BAGGAGE_KEY).

-spec get_http_propagators() -> {ot_propagation:http_extractor(), ot_propagation:http_injector()}.
get_http_propagators() ->
    ToText = fun ?MODULE:to_text/2,
    FromText = fun ?MODULE:from_text/2,
    Inject = ot_ctx:http_injector(?BAGGAGE_KEY, ToText),
    Extract = ot_ctx:http_extractor(?BAGGAGE_KEY, FromText),
    {Extract, Inject}.

to_text(_Headers, undefined) ->
    [];
to_text(_Headers, Baggage) ->
    case maps:fold(fun(Key, Value, Acc) ->
                           [$,, [Key, "=", Value] | Acc]
                   end, [], Baggage) of
        [$, | List] ->
            [{?BAGGAGE_HEADER, List}];
        _ ->
            []
    end.

from_text(Headers, CurrentBaggage) ->
    case lists:keyfind(?BAGGAGE_HEADER, 1, Headers) of
        {_, String} ->
            Pairs = string:lexemes(String, [","]),
            lists:foldl(fun(Pair, Acc) ->
                                [Key, Value] = string:split(Pair, "="),
                                Acc#{Key => {Value, unlimited_propagation}}
                        end, CurrentBaggage, Pairs);
        _ ->
            CurrentBaggage
    end.
