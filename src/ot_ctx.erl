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
-module(ot_ctx).

-export([set_value/3,
         get_value/2,
         get_value/3,
         remove/2,
         clear/1,

         set_current/2,
         get_current/1,

         http_extractor/2,
         http_extractor/3,
         http_injector/2,
         http_injector/3,
         http_extractor_fun/3,
         http_extractor_fun/4,
         http_injector_fun/3,
         http_injector_fun/4]).

-type ctx() :: map().
-type namespace() :: term().
-type key() :: term().
-type value() :: term().

-callback set_value(namespace(), key(), value()) -> ok.
-callback get_value(namespace(), key()) -> value() | undefined.
-callback get_value(namespace(), key(), value()) -> value() | undefined.
-callback remove(namespace(), key()) -> ok.
-callback clear(namespace()) -> ok.
-callback set_current(namespace(), ctx()) -> ok.
-callback get_current(namespace()) -> ctx().

-export_type([ctx/0,
              key/0,
              value/0]).

-spec set_value(term(), term(), term()) -> ok.
set_value(Namespace, Key, Value) ->
    case erlang:get(Namespace) of
        Map when is_map(Map) ->
            erlang:put(Namespace, Map#{Key => Value}),
            ok;
        _ ->
            erlang:put(Namespace, #{Key => Value}),
            ok
    end.

-spec get_value(term(), term()) -> term().
get_value(Namespace, Key) ->
    get_value(Namespace, Key, undefined).

-spec get_value(term(), term(), term()) -> term().
get_value(Namespace, Key, Default) ->
    case erlang:get(Namespace) of
        undefined ->
            Default;
        Map when is_map(Map) ->
            maps:get(Key, Map, Default);
        _ ->
            Default
    end.

-spec clear(term()) -> ok.
clear(Namespace) ->
    erlang:erase(Namespace).

-spec remove(term(), term()) -> ok.
remove(Namespace, Key) ->
    case erlang:get(Namespace) of
        Map when is_map(Map) ->
            erlang:put(Namespace, maps:remove(Key, Map)),
            ok;
        _ ->
            ok
    end.

-spec get_current(term()) -> map().
get_current(Namespace) ->
    case erlang:get(Namespace) of
        Map when is_map(Map) ->
            Map;
        _ ->
            #{}
    end.

-spec set_current(term(), map()) -> ok.
set_current(Namespace, Ctx) ->
    erlang:put(Namespace, Ctx).

%% Extractor and Injector setup functions

http_extractor(Namespace, FromText) ->
    {fun ?MODULE:http_extractor_fun/3, {Namespace, FromText}}.

http_extractor_fun(Headers, Namespace, FromText) ->
    New = FromText(Headers, ?MODULE:get_current(Namespace)),
    ?MODULE:set_current(Namespace, New).

http_extractor(Namespace, Key, FromText) ->
    {fun ?MODULE:http_extractor_fun/4, {Namespace, Key, FromText}}.

http_extractor_fun(Headers, Namespace, Key, FromText) ->
    New = FromText(Headers, ?MODULE:get_value(Namespace, Key)),
    ?MODULE:set_value(Namespace, Key, New).

http_injector(Namespace, ToText) ->
    {fun ?MODULE:http_injector_fun/3, {Namespace, ToText}}.

http_injector_fun(Headers, Namespace, ToText) ->
    Headers ++ ToText(Headers, ?MODULE:get_current(Namespace)).

http_injector(Namespace, Key, ToText) ->
    {fun ?MODULE:http_injector_fun/4, {Namespace, Key, ToText}}.

http_injector_fun(Headers, Namespace, Key, ToText) ->
    Headers ++ ToText(Headers, ?MODULE:get_value(Namespace, Key)).
