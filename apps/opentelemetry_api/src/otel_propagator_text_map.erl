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
%% @doc A TextMap Propagator is a Propagator that performs injection and
%% extraction with ASCII keys and values.
%%
%% An example of
%% configuring the TextMap Propagator to inject and extract Baggage and
%% TraceContext:
%%
%% ```
%% {text_map_propagators, [trace_context, baggage]},
%% '''
%%
%% The propagators are then used at the points that cross service
%% communication is performed. By default `inject' and `extract' work on a
%% generic list of 2-tuple's with binary string keys and values. A user
%% defined function for setting a key/value in the carrier and for getting
%% the value of a key from a carrier can be passed as an argument. For
%% example, injecting and extracting to and from Hackney headers could be
%% done with <a href="https://github.com/benoitc/hackney">Hackney</a> specific functions:
%%
%% ```
%% set_header(Headers, Key, Value) ->
%%   hackney_headers:store(Key, Value, Headers).
%%
%% some_fun_calling_hackney() ->
%%   Headers = otel_propagator_text_map:inject(hackney_headers:new(), fun set_header/2),
%%   ...
%% '''
%%
%% An example of extraction in an <a href="https://github.com/elli-lib/elli">Elli</a> request handler:
%%
%% ```
%% get_header(Req, Key) ->
%%   elli_request:get_header(Key, Req, Default).
%%
%% handle(Req, _Args) ->
%%   otel_propagator_text_map:extract(Req, fun get_header/2),
%%   ...
%%   {ok, [], <<"hello world">>}.
%% '''
%% @end
%%%-------------------------------------------------------------------------
-module(otel_propagator_text_map).

-behaviour(otel_propagator).

-export([inject/1,
         inject/2,
         inject_from/2,
         inject_from/3,
         extract/1,
         extract/2,
         extract_to/2,
         extract_to/3]).

-export([default_carrier_get/2,
         default_carrier_set/3,
         default_carrier_keys/1]).

-include_lib("kernel/include/logger.hrl").

%% Sets a value into a carrier
-callback inject(otel_ctx:t(), otel_propagator:carrier(), carrier_set()) -> otel_propagator:carrier().

%% Extracts values from a carrier and sets them in the context
-callback extract(otel_ctx:t(), otel_propagator:carrier(), carrier_keys(), carrier_get()) -> term().

%% Returns all the keys the propagator sets with `inject'
-callback fields() -> [field_key()].

-type field_key() :: unicode:latin1_binary().
-type field_value() :: unicode:latin1_binary().

%% return all matching keys from the carrier
%% for example: with the jaeger propagation format this would be
%% all keys found with prefix "uberctx-"
-type carrier_keys() :: fun((otel_propagator:carrier()) -> [unicode:latin1_binary()]).
-type carrier_get() :: fun((otel_propagator:carrier(), unicode:latin1_binary()) -> unicode:latin1_binary() | undefined).
-type carrier_set() :: fun((otel_propagator:carrier(), unicode:latin1_binary(), unicode:latin1_binary()) -> otel_propagator:carrier()).

-type default_text_map_carrier() :: [{unicode:latin1_binary(), unicode:latin1_binary()}].

-type inject_options() :: #{carrier_set_fun => carrier_set(),
                            propagators => [otel_propagator:t()]}.

-type extract_options() :: #{carrier_get_fun => carrier_get(),
                             carrier_keys_fun => carrier_keys(),
                             propagators => [otel_propagator:t()]}.
-export_type([]).

-spec inject(otel_propagator:carrier()) -> otel_propagator:carrier().
inject(Carrier) ->
    inject(Carrier, #{}).

-spec inject(otel_propagator:carrier(), inject_options()) -> otel_propagator:carrier().
inject(Carrier, InjectOptions) ->
    Context = otel_ctx:get_current(),
    inject_from(Context, Carrier, InjectOptions).

-spec inject_from(otel_ctx:t(), otel_propagator:carrier()) -> otel_propagator:carrier().
inject_from(Context, Carrier) ->
    inject_from(Context, Carrier, #{}).

-spec inject_from(otel_ctx:t(), otel_propagator:carrier(), inject_options()) -> otel_propagator:carrier().
inject_from(Context, Carrier, InjectOptions) when is_map(InjectOptions)->
    Injectors = maps:get(propagators, InjectOptions, opentelemetry:get_text_map_injectors()),
    CarrierSetFun = maps:get(carrier_set_fun, InjectOptions, fun ?MODULE:default_carrier_set/3),
    run_injectors(Context, Injectors, Carrier, CarrierSetFun);
inject_from(_Context, Carrier, InjectOptions) ->
    ?LOG_INFO("inject failed. InjectOptions must be a map but instead got: ~p", [InjectOptions]),
    Carrier.

-spec extract(otel_propagator:carrier()) -> otel_ctx:t().
extract(Carrier) ->
    extract(Carrier, #{}).

-spec extract(otel_propagator:carrier(), extract_options()) -> otel_ctx:t().
extract(Carrier, ExtractOptions) ->
    Context = otel_ctx:get_current(),
    Context1 = extract_to(Context, Carrier, ExtractOptions),
    otel_ctx:attach(Context1).

-spec extract_to(otel_ctx:t(), otel_propagator:carrier()) -> otel_ctx:t().
extract_to(Context, Carrier) ->
    extract_to(Context, Carrier, #{}).

-spec extract_to(otel_ctx:t(), otel_propagator:carrier(), extract_options()) -> otel_ctx:t().
extract_to(Context, Carrier, ExtractOptions) when is_map(ExtractOptions) ->
    Extractors = maps:get(propagators, ExtractOptions, opentelemetry:get_text_map_extractors()),
    CarrierKeysFun = maps:get(carrier_keys_fun, ExtractOptions, fun ?MODULE:default_carrier_keys/1),
    CarrierGetFun = maps:get(carrier_get_fun, ExtractOptions, fun ?MODULE:default_carrier_get/2),
    run_extractors(Context, Extractors, Carrier, CarrierKeysFun, CarrierGetFun);
extract_to(Context, _Carrier, ExtractOptions) ->
    ?LOG_INFO("extract failed. ExtractOptions must be a map but instead got: ~p", [ExtractOptions]),
    Context.

run_extractors(Context, Extractors, Carrier, CarrierKeysFun, CarrierGetFun) when is_list(Extractors) ->
    lists:foldl(fun(Extract, ContextAcc) ->
                        try Extract:extract(ContextAcc, Carrier, CarrierKeysFun, CarrierGetFun)
                        catch
                            C:E:S ->
                                ?LOG_INFO("text map propagator failed to extract from carrier",
                                          #{extractor => Extract, carrier => Carrier,
                                            class => C, exception => E, stacktrace => S}),
                                ContextAcc
                        end
                end, Context, otel_propagator:builtins_to_modules(Extractors));
run_extractors(Context, Extractors, _, _, _) ->
    ?LOG_INFO("extract failed. Extractors must be a list but instead got: ~p", [Extractors]),
    Context.

run_injectors(Context, Injectors, Carrier, Setter) when is_list(Injectors) ->
    lists:foldl(fun(Inject, CarrierAcc) ->
                        try Inject:inject(Context, CarrierAcc, Setter)
                        catch
                            C:E:S ->
                                ?LOG_INFO("text map propagator failed to inject to carrier",
                                          #{injector => Inject, carrier => CarrierAcc,
                                            class => C, exception => E, stacktrace => S}),
                                CarrierAcc
                        end
                end, Carrier, otel_propagator:builtins_to_modules(Injectors));
run_injectors(_, Injectors, Carrier, _) ->
    ?LOG_INFO("inject failed. Injectors must be a list but instead got: ~p", [Injectors]),
    Carrier.

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
