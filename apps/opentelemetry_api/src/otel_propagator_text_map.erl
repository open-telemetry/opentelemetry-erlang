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
%% @doc
%% @end
%%%-------------------------------------------------------------------------
-module(otel_propagator_text_map).

-behaviour(otel_propagator).

-export([inject/1,
         inject/2,
         inject/3,
         extract/1,
         extract/2,
         extract/4]).

-export([default_carrier_get/2,
         default_carrier_set/3,
         default_carrier_keys/1]).

-include_lib("kernel/include/logger.hrl").

-callback inject(otel_ctx:t(), otel_propagator:carrier(), carrier_set()) -> otel_propagator:carrier().
-callback extract(otel_ctx:t(), otel_propagator:carrier(), carrier_keys(), carrier_get()) -> term().
%% returns all the keys the propagator sets with `inject'
-callback fields() -> [field_key()].

-type field_key() :: unicode:latin1_binary().
-type field_value() :: unicode:latin1_binary().

%% return all matching keys from the carrier
%% for example: with the jaeger propagation format this would be
%% all keys found with prefix "uberctx-"
-type carrier_keys() :: fun((otel_propagator:carrier()) -> [unicode:latin1_binary()]).
-type carrier_get() :: fun((otel_propagator:carrier(), unicode:latin1_binary()) -> unicode:latin1_binary()).
-type carrier_set() :: fun((otel_propagator:carrier(), unicode:latin1_binary(), unicode:latin1_binary()) -> otel_propagator:carrier()).

-type default_text_map_carrier() :: [{unicode:latin1_binary(), unicode:latin1_binary()}].

-export_type([]).

inject(Carrier) ->
    Context = otel_ctx:get_current(),
    inject(Context, Carrier, fun ?MODULE:default_carrier_set/3).

inject(Context, Carrier) ->
    inject(Context, Carrier, fun ?MODULE:default_carrier_set/3).

inject(Context, Carrier, CarrierSet) ->
    Injectors = opentelemetry:get_text_map_injectors(),
    run_injectors(Context, Carrier, Injectors, CarrierSet).

extract(Carrier) ->
    Context = otel_ctx:get_current(),
    extract(Context, Carrier, fun ?MODULE:default_carrier_keys/1, fun ?MODULE:default_carrier_get/2).

extract(Context, Carrier) ->
    extract(Context, Carrier, fun ?MODULE:default_carrier_keys/1, fun ?MODULE:default_carrier_get/2).

extract(Context, Carrier, CarrierKeysFun, CarrierGet) ->
    Extractors = opentelemetry:get_text_map_extractors(),
    Context1 = run_extractors(Context, Carrier, Extractors, CarrierKeysFun, CarrierGet),
    otel_ctx:attach(Context1).

run_extractors(Context, Carrier, Extractors, CarrierKeysFun, Getter) ->
    lists:foldl(fun(Extract, ContextAcc) ->
                        try Extract:extract(ContextAcc, Carrier, CarrierKeysFun, Getter)
                        catch
                            C:E:S ->
                                ?LOG_INFO("text map propagator failed to extract from carrier",
                                          #{extractor => Extract, carrier => Carrier,
                                            class => C, exception => E, stacktrace => S}),
                                ContextAcc
                        end
                end, Context, Extractors).

run_injectors(Context, Carrier, Injectors, Setter) ->
    lists:foldl(fun(Inject, CarrierAcc) ->
                        try Inject:inject(Context, CarrierAcc, Setter)
                        catch
                            C:E:S ->
                                ?LOG_INFO("text map propagator failed to inject to carrier",
                                          #{injector => Inject, carrier => CarrierAcc,
                                            class => C, exception => E, stacktrace => S}),
                                CarrierAcc
                        end
                end, Carrier, Injectors).


%% case-insensitive finding of a key string in a list of ASCII strings
%% if there are multiple entries in the list for the same key the values
%% will be combined and separated by commas. This is the method defined
%% in RFC7230 for HTTP headers.
-spec default_carrier_get(field_key(), default_text_map_carrier()) -> field_value() | undefined.
default_carrier_get(Key, List) ->
    default_carrier_get(Key, List, []).

default_carrier_get(_, [], []) ->
    undefined;
default_carrier_get(_, [], Result) ->
    unicode:characters_to_binary(lists:join($,, lists:reverse(Result)), latin1);
default_carrier_get(Key, [{H, V} | Rest], Result) ->
    case string:equal(Key, H, true, none) of
        true ->
            default_carrier_get(Key, Rest, [V | Result]);
        false ->
            default_carrier_get(Key, Rest, Result)
    end.

%% case-insensitive ASCII string based lists:keyreplace
-spec default_carrier_set(field_key(), field_value(), default_text_map_carrier())
                         -> default_text_map_carrier().
default_carrier_set(Key, Value, []) ->
    [{Key, Value}];
default_carrier_set(Key, Value, [{H, _}=Elem | Rest]) ->
    case string:equal(Key, H, true, none) of
        true ->
            [{Key, Value} | Rest];
        false ->
            [Elem | default_carrier_set(Key, Value, Rest)]
    end.

-spec default_carrier_keys(default_text_map_carrier()) -> [field_key()].
default_carrier_keys([]) ->
    [];
default_carrier_keys([{K, _} | Rest]) ->
    [K | default_carrier_keys(Rest)].
