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
%% @doc Ctx is responsible for propagating values within a process that
%% are associated with a particular Trace or set of Baggage.
%% `OpenTelemetry.Tracer' and `OpenTelemetry.Baggage' handle updating
%% the Context.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_ctx).

-export([new/0,
         set_value/2,
         set_value/3,
         get_value/1,
         get_value/2,
         get_value/3,
         remove/1,
         remove/2,
         clear/0,
         clear/1,

         attach/1,
         detach/1,
         get_current/0,

         text_map_extractor/2,
         text_map_injector/2,
         text_map_extractor_fun/3,
         text_map_injector_fun/3]).

-type t() :: map().
-type key() :: term().
-type value() :: term().

%% at this time the "token" is actually the context map itself
-type token() :: term().

-export_type([t/0,
              key/0,
              value/0
            ]).

-define(CURRENT_CTX, '$__current_otel_ctx').

-spec new() -> t().
new() ->
    #{}.

-spec set_value(term(), term()) -> ok.
set_value(Key, Value) ->
    erlang:put(?CURRENT_CTX, set_value(erlang:get(?CURRENT_CTX), Key, Value)),
    ok.

-spec set_value(t(), term(), term()) -> map().
set_value(Ctx, Key, Value) when is_map(Ctx) ->
    Ctx#{Key => Value};
set_value(_, Key, Value) ->
    #{Key => Value}.

-spec get_value(term()) -> term().
get_value(Key) ->
    get_value(erlang:get(?CURRENT_CTX), Key, undefined).

-spec get_value(term(), term()) -> term().
get_value(Key, Default) ->
    get_value(erlang:get(?CURRENT_CTX), Key, Default).

-spec get_value(t(), term(), term()) -> term().
get_value(undefined, _Key, Default) ->
    Default;
get_value(Ctx, Key, Default) when is_map(Ctx) ->
    maps:get(Key, Ctx, Default);
get_value(_, _, Default) ->
    Default.

-spec clear() -> ok.
clear() ->
    erlang:erase(?CURRENT_CTX),
    ok.

-spec clear(t()) -> t().
clear(_) ->
    new().

-spec remove(term()) -> ok.
remove(Key) ->
    case erlang:get(?CURRENT_CTX) of
        Map when is_map(Map) ->
            erlang:put(?CURRENT_CTX, maps:remove(Key, Map)),
            ok;
        _ ->
            ok
    end.

-spec remove(t(), term()) -> t().
remove(Ctx, Key) when is_map(Ctx) ->
    maps:remove(Key, Ctx);
remove(_, _) ->
    new().

-spec get_current() -> map().
get_current() ->
    case erlang:get(?CURRENT_CTX) of
        Map when is_map(Map) ->
            Map;
        _ ->
            #{}
    end.

-spec attach(map()) -> token().
attach(Ctx) ->
    erlang:put(?CURRENT_CTX, Ctx).

-spec detach(token()) -> ok.
detach(Token) ->
    erlang:put(?CURRENT_CTX, Token).


%% Extractor and Injector setup functions

text_map_extractor(Key, FromText) ->
    {fun ?MODULE:text_map_extractor_fun/3, {Key, FromText}}.

text_map_extractor_fun(TextMap, Key, FromText) ->
    New = FromText(TextMap, ?MODULE:get_value(Key, #{})),
    ?MODULE:set_value(Key, New).

text_map_injector(Key, ToText) ->
    {fun ?MODULE:text_map_injector_fun/3, {Key, ToText}}.

text_map_injector_fun(TextMap, Key, ToText) ->
    TextMap ++ ToText(?MODULE:get_value(Key, undefined)).
