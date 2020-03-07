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
         get_current/1,

         http_extractor/2,
         http_injector/2,
         http_extractor/3,
         http_injector/3,
         http_extractor_fun/4,
         http_extractor_fun/5,
         http_injector_fun/4,
         http_injector_fun/5]).

-type ctx() :: map().
-type namespace() :: term().
-type key() :: term().
-type value() :: term().
-type context_manager() :: {module(), term()}.

-callback set_value(context_manager(), namespace(), key(), value()) -> ok.
-callback get_value(context_manager(), namespace(), key()) -> value() | undefined.
-callback remove(context_manager(), namespace(), key()) -> ok.
-callback clear(context_manager(), namespace()) -> ok.
-callback set_current(context_manager(), namespace(), ctx()) -> ok.
-callback get_current(context_manager(), namespace()) -> ctx().

-export_type([ctx/0,
              key/0,
              value/0,
              context_manager/0]).

-spec set_value(namespace(), key(), value()) -> ok.
set_value(Namespace, Key, Value) ->
    set_value(opentelemetry:get_context_manager(), Namespace, Key, Value).

-spec set_value(context_manager(), namespace(), key(), value()) -> ok.
set_value(ContextManager={CtxModule, _}, Namespace, Key, Value) ->
    CtxModule:set_value(ContextManager, Namespace, Key, Value).

-spec get_value(namespace(), key()) -> value().
get_value(Namespace, Key) ->
    get_value(opentelemetry:get_context_manager(), Namespace, Key).

-spec get_value(context_manager(), namespace(), key()) -> value().
get_value(ContextManager={CtxModule, _}, Namespace, Key) ->
    CtxModule:get_value(ContextManager, Namespace, Key).

-spec remove(namespace(), key()) -> ok.
remove(Namespace, Key) ->
    remove(opentelemetry:get_context_manager(), Namespace, Key).

-spec remove(context_manager(), namespace(), key()) -> ok.
remove(ContextManager={CtxModule, _}, Namespace, Key) ->
    CtxModule:remove(ContextManager, Namespace, Key).

-spec clear(namespace()) -> ok.
clear(Namespace) ->
    clear(opentelemetry:get_context_manager(), Namespace).

-spec clear(context_manager(), namespace()) -> ok.
clear(ContextManager={CtxModule, _}, Namespace) ->
    CtxModule:clear(ContextManager, Namespace).

-spec set_current(namespace(), ctx()) -> ok.
set_current(Namespace, Ctx) ->
    set_current(opentelemetry:get_context_manager(), Namespace, Ctx).

-spec set_current(context_manager(), namespace(), ctx()) -> ok.
set_current(ContextManager={CtxModule, _}, Namespace, Ctx) ->
    CtxModule:set_current(ContextManager, Namespace, Ctx).

-spec get_current(namespace()) -> ctx().
get_current(Namespace) ->
    get_current(opentelemetry:get_context_manager(), Namespace).

-spec get_current(context_manager(), namespace()) -> ctx().
get_current(ContextManager={CtxModule, _}, Namespace) ->
    CtxModule:get_current(ContextManager, Namespace).

http_extractor(Namespace, FromText) ->
    http_extractor_(opentelemetry:get_context_manager(), Namespace, FromText).

http_extractor_(ContextManager, Namespace, FromText) ->
    {fun ?MODULE:http_extractor_fun/4, {ContextManager, Namespace, FromText}}.

http_extractor_fun(Headers, ContextManager={CtxModule, _}, Namespace, FromText) ->
    New = FromText(Headers, CtxModule:get_current(ContextManager, Namespace)),
    CtxModule:set_current(ContextManager, Namespace, New).

http_extractor(Namespace, Key, FromText) ->
    http_extractor_(opentelemetry:get_context_manager(), Namespace, Key, FromText).

http_extractor_(ContextManager, Namespace, Key, FromText) ->
    {fun ?MODULE:http_extractor_fun/5, {ContextManager, Namespace, Key, FromText}}.

http_extractor_fun(Headers, ContextManager={CtxModule, _}, Namespace, Key, FromText) ->
    New = FromText(Headers, CtxModule:get_value(ContextManager, Namespace, Key)),
    CtxModule:set_value(ContextManager, Namespace, Key, New).

http_injector(Namespace, ToText) ->
    http_injector_(opentelemetry:get_context_manager(), Namespace, ToText).

http_injector_(ContextManager, Namespace, ToText) ->
    {fun ?MODULE:http_injector_fun/4, {ContextManager, Namespace, ToText}}.

http_injector_fun(Headers, ContextManager={CtxModule, _}, Namespace, ToText) ->
    Headers ++ ToText(Headers, CtxModule:get_current(ContextManager, Namespace)).

http_injector(Namespace, Key, ToText) ->
    http_injector_(opentelemetry:get_context_manager(), Namespace, Key, ToText).

http_injector_(ContextManager, Namespace, Key, ToText) ->
    {fun ?MODULE:http_injector_fun/5, {ContextManager, Namespace, Key, ToText}}.

http_injector_fun(Headers, ContextManager={CtxModule, _}, Namespace, Key, ToText) ->
    Headers ++ ToText(Headers, CtxModule:get_value(ContextManager, Namespace, Key)).
