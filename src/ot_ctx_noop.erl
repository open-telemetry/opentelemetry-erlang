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

-export([set_value/3,
         get_value/2,
         remove/2,
         clear/1,
         set_current/2,
         get_current/1]).

-spec set_value(ot_ctx:namespace(), ot_ctx:key(), ot_ctx:value()) -> ok.
set_value(_Namespace, _Key, _Value) ->
    ok.

-spec get_value(ot_ctx:namespace(), ot_ctx:key()) -> ot_ctx:value().
get_value(_Namespace, _Key) ->
    undefined.

-spec remove(ot_ctx:namespace(), ot_ctx:key()) -> ok.
remove(_Namespace, _Key) ->
    ok.

-spec clear(ot_ctx:namespace()) -> ok.
clear(_Namespace) ->
    ok.

-spec set_current(ot_ctx:namespace(), ot_ctx:ctx()) -> ok.
set_current(_Namespace, _Ctx) ->
    ok.

-spec get_current(ot_ctx:namespace()) -> ot_ctx:ctx().
get_current(_Namespace) ->
    #{}.
