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
-module(ot_ctx_noop).

-behaviour(ot_ctx).

-export([set_value/4,
         get_value/3,
         get_value/4,
         remove/3,
         clear/2,
         set_current/3,
         get_current/2]).

-spec set_value(ot_ctx:context_manager(), ot_ctx:namespace(), ot_ctx:key(), ot_ctx:value()) -> ok.
set_value(_ContextManager, _Namespace, _Key, _Value) ->
    ok.

-spec get_value(ot_ctx:context_manager(), ot_ctx:namespace(), ot_ctx:key()) -> ot_ctx:value().
get_value(_ContextManager, _Namespace, _Key) ->
    undefined.

-spec get_value(ot_ctx:context_manager(), ot_ctx:namespace(), ot_ctx:key(), ot_ctx:value()) -> ot_ctx:value().
get_value(_ContextManager, _Namespace, _Key, _Default) ->
    undefined.

-spec remove(ot_ctx:context_manager(), ot_ctx:namespace(), ot_ctx:key()) -> ok.
remove(_ContextManager, _Namespace, _Key) ->
    ok.

-spec clear(ot_ctx:context_manager(), ot_ctx:namespace()) -> ok.
clear(_ContextManager, _Namespace) ->
    ok.

-spec set_current(ot_ctx:context_manager(), ot_ctx:namespace(), ot_ctx:ctx()) -> ok.
set_current(_ContextManager, _Namespace, _Ctx) ->
    ok.

-spec get_current(ot_ctx:context_manager(), ot_ctx:namespace()) -> ot_ctx:ctx().
get_current(_ContextManager, _Namespace) ->
    #{}.
