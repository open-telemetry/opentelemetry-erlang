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

-export([set_value/4,
         get_value/3,
         get_value/4,
         get_current/2,
         set_current/3,
         clear/2,
         remove/3]).

-spec set_value(ot_ctx:context_manager(), term(), term(), term()) -> ok.
set_value(_ContextManager, Namespace, Key, Value) ->
    case erlang:get(Namespace) of
        Map when is_map(Map) ->
            erlang:put(Namespace, Map#{Key => Value}),
            ok;
        _ ->
            erlang:put(Namespace, #{Key => Value}),
            ok
    end.

-spec get_value(ot_ctx:context_manager(), term(), term()) -> term().
get_value(_ContextManager, Namespace, Key) ->
    get_value(_ContextManager, Namespace, Key, undefined).

-spec get_value(ot_ctx:context_manager(), term(), term(), term()) -> term().
get_value(_ContextManager, Namespace, Key, Default) ->
    case erlang:get(Namespace) of
        undefined ->
            Default;
        Map when is_map(Map) ->
            maps:get(Key, Map, Default);
        _ ->
            Default
    end.

-spec clear(ot_ctx:context_manager(), term()) -> ok.
clear(_ContextManager, Namespace) ->
    erlang:erase(Namespace).

-spec remove(ot_ctx:context_manager(), term(), term()) -> ok.
remove(_ContextManager, Namespace, Key) ->
    case erlang:get(Namespace) of
        Map when is_map(Map) ->
            erlang:put(Namespace, maps:remove(Key, Map)),
            ok;
        _ ->
            ok
    end.

-spec get_current(ot_ctx:context_manager(), term()) -> map().
get_current(_ContextManager, Namespace) ->
    case erlang:get(Namespace) of
        Map when is_map(Map) ->
            Map;
        _ ->
            #{}
    end.

-spec set_current(ot_ctx:context_manager(), term(), map()) -> ok.
set_current(_ContextManager, Namespace, Ctx) ->
    erlang:put(Namespace, Ctx).
