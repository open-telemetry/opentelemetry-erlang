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
%% set_header(Key, Value, Headers) ->
%%   hackney_headers:store(Key, Value, Headers).
%%
%% some_fun_calling_hackney() ->
%%   Headers = otel_propagator_text_map:inject(opentelemetry:get_text_map_injector(), hackney_headers:new(), fun set_header/2),
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

-export([fields/1,
         inject/1,
         inject/2,
         inject/3,
         inject_from/2,
         inject_from/3,
         inject_from/4,
         extract/1,
         extract/2,
         extract/4,
         extract_to/2,
         extract_to/3,
         extract_to/5]).

-export([default_carrier_get/2,
         default_carrier_set/3,
         default_carrier_keys/1]).

-include_lib("kernel/include/logger.hrl").

%% Sets a value into a carrier
-callback inject(otel_ctx:t(), otel_propagator:carrier(), carrier_set(), propagator_options()) -> otel_propagator:carrier().

%% Extracts values from a carrier and sets them in the context
-callback extract(otel_ctx:t(), otel_propagator:carrier(), carrier_keys(), carrier_get(), propagator_options()) -> term().

%% Returns all the keys the propagator sets with `inject'
-callback fields(propagator_options()) -> [field_key()].

%% a textmap propagator can have any term associated with it
-type propagator_options() :: term().

-type field_key() :: unicode:latin1_binary().
-type field_value() :: unicode:latin1_binary().

%% return all matching keys from the carrier
%% for example: with the jaeger propagation format this would be
%% all keys found with prefix "uberctx-"
-type carrier_keys() :: fun((otel_propagator:carrier()) -> [unicode:latin1_binary()]).
-type carrier_get() :: fun((otel_propagator:carrier(), unicode:latin1_binary()) -> unicode:latin1_binary() | undefined).
-type carrier_set() :: fun((otel_propagator:carrier(), unicode:latin1_binary(), unicode:latin1_binary()) -> otel_propagator:carrier()).

-type default_text_map_carrier() :: [{unicode:latin1_binary(), unicode:latin1_binary()}].

%% 2-tuple form is a textmap propagator with options
%% an empty list is passed for options if the propagator is a module with no options
-type t() :: module() | {module(), propagator_options()}.

-export_type([t/0,
              carrier_set/0,
              carrier_get/0,
              carrier_keys/0,
              propagator_options/0]).

-spec fields(otel_propagator:t()) -> [field_key()].
fields(Propagator) when is_atom(Propagator) ->
    Propagator:fields([]);
fields({Module, Options}) ->
    Module:fields(Options).

-spec inject(otel_propagator:carrier()) -> otel_propagator:carrier().
inject(Carrier) ->
    Propagator = opentelemetry:get_text_map_injector(),
    inject(Propagator, Carrier, fun default_carrier_set/3).

-spec inject(otel_propagator:t(), otel_propagator:carrier()) -> otel_propagator:carrier().
inject(Propagator, Carrier) ->
    inject(Propagator, Carrier, fun default_carrier_set/3).

-spec inject(otel_propagator:t(), otel_propagator:carrier(), fun()) -> otel_propagator:carrier().
inject(Propagator, Carrier, CarrierSetFun) ->
    Context = otel_ctx:get_current(),
    inject_from(Context, Propagator, Carrier, CarrierSetFun).

-spec inject_from(otel_ctx:t(), otel_propagator:carrier()) -> otel_propagator:carrier().
inject_from(Context, Carrier) ->
    Propagator = opentelemetry:get_text_map_injector(),
    inject_from(Context, Propagator, Carrier, fun default_carrier_set/3).

-spec inject_from(otel_ctx:t(), otel_propagator:t(), otel_propagator:carrier()) -> otel_propagator:carrier().
inject_from(Context, Propagator, Carrier) ->
    inject_from(Context, Propagator, Carrier, fun default_carrier_set/3).

-spec inject_from(otel_ctx:t(), otel_propagator:t(), otel_propagator:carrier(), fun()) -> otel_propagator:carrier().
inject_from(Context, Module, Carrier, CarrierSetFun) when is_atom(Module) ->
     Module:inject(Context, Carrier, CarrierSetFun, []);
inject_from(Context, {Module, Options}, Carrier, CarrierSetFun) ->
     Module:inject(Context, Carrier, CarrierSetFun, Options).

-spec extract(otel_propagator:carrier()) -> otel_ctx:t().
extract(Carrier) ->
    Propagator = opentelemetry:get_text_map_extractor(),
    extract(Propagator, Carrier, fun default_carrier_keys/1, fun default_carrier_get/2).

-spec extract(otel_propagator:t(), otel_propagator:carrier()) -> otel_ctx:t().
extract(Propagator, Carrier) ->
    extract(Propagator, Carrier, fun default_carrier_keys/1, fun default_carrier_get/2).

-spec extract(otel_propagator:t(), otel_propagator:carrier(), fun(), fun()) -> otel_ctx:t().
extract(Propagator, Carrier, CarrierKeysFun, CarrierGetFun) ->
    Context = otel_ctx:get_current(),
    Context1 = extract_to(Context, Propagator, Carrier, CarrierKeysFun, CarrierGetFun),
    otel_ctx:attach(Context1).

-spec extract_to(otel_ctx:t(), otel_propagator:carrier()) -> otel_ctx:t().
extract_to(Context, Carrier) ->
    Propagator = opentelemetry:get_text_map_extractor(),
    extract_to(Context, Propagator, Carrier, fun default_carrier_keys/1, fun default_carrier_get/2).

-spec extract_to(otel_ctx:t(), otel_propagator:t(), otel_propagator:carrier()) -> otel_ctx:t().
extract_to(Context, Propagator, Carrier) ->
    extract_to(Context, Propagator, Carrier, fun default_carrier_keys/1, fun default_carrier_get/2).

-spec extract_to(otel_ctx:t(), otel_propagator:t(), otel_propagator:carrier(), fun(), fun()) -> otel_ctx:t().
extract_to(Context, Module, Carrier, CarrierKeysFun, CarrierGetFun) when is_atom(Module) ->
    Module:extract(Context, Carrier, CarrierKeysFun, CarrierGetFun, []);
extract_to(Context, {Module, Options}, Carrier, CarrierKeysFun, CarrierGetFun) ->
    Module:extract(Context, Carrier, CarrierKeysFun, CarrierGetFun, Options).

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
