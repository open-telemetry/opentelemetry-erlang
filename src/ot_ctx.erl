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
         remove/2,
         clear/1,
         set_current/2,
         get_current/1]).

-type ctx() :: map().
-type namespace() :: term().
-type key() :: string().
-type value() :: term().

-callback set_value(namespace(), key(), value()) -> ok.
-callback get_value(namespace(), key()) -> value() | undefined.
-callback remove(namespace(), key()) -> ok.
-callback clear(namespace()) -> ok.
-callback set_current(namespace(), ctx()) -> ok.
-callback get_current(namespace()) -> ctx().

-export_type([ctx/0,
              key/0,
              value/0]).

-spec set_value(namespace(), key(), value()) -> ok.
set_value(Namespace, Key, Value) ->
    set_value(opentelemetry:get_context_manager(), Namespace, Key, Value).

-spec set_value(module(), namespace(), key(), value()) -> ok.
set_value(CtxModule, Namespace, Key, Value) ->
    CtxModule:set_value(Namespace, Key, Value).

-spec get_value(namespace(), key()) -> value().
get_value(Namespace, Key) ->
    get_value(opentelemetry:get_context_manager(), Namespace, Key).

-spec get_value(module(), namespace(), key()) -> value().
get_value(Module, Namespace, Key) ->
    Module:get_value(Namespace, Key).

-spec remove(namespace(), key()) -> ok.
remove(Namespace, Key) ->
    remove(opentelemetry:get_context_manager(), Namespace, Key).

-spec remove(module(), namespace(), key()) -> ok.
remove(CtxModule, Namespace, Key) ->
    CtxModule:remove(Namespace, Key).

-spec clear(namespace()) -> ok.
clear(Namespace) ->
    clear(opentelemetry:get_context_manager(), Namespace).

-spec clear(module(), namespace()) -> ok.
clear(CtxModule, Namespace) ->
    CtxModule:clear(Namespace).

-spec set_current(namespace(), ctx()) -> ok.
set_current(Namespace, Ctx) ->
    set_current(opentelemetry:get_context_manager(), Namespace, Ctx).

-spec set_current(module(), namespace(), ctx()) -> ok.
set_current(CtxModule, Namespace, Ctx) ->
    CtxModule:set_current(Namespace, Ctx).

-spec get_current(namespace()) -> ctx().
get_current(Namespace) ->
    get_current(opentelemetry:get_context_manager(), Namespace).

-spec get_current(module(), namespace()) -> ctx().
get_current(CtxModule, Namespace) ->
    CtxModule:get_current(Namespace).
