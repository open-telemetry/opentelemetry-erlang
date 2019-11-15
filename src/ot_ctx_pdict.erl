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
-module(ot_ctx_pdict).

-behaviour(ot_ctx).

-export([set_value/3,
         get_value/2,
         get_value/3,
         get_current/1,
         set_current/2,
         clear/1,
         remove/2]).

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
