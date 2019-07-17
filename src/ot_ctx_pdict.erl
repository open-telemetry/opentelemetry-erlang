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

-export([with_value/2,
         with_value/3,
         get/1,
         get/2]).

-spec get(term()) -> term().
get(Key) ->
    erlang:get(Key).

-spec get(term(), term()) -> term().
get(Key, Default) ->
    case erlang:get(Key) of
        undefined ->
            Default;
        Value ->
            Value
    end.

-spec with_value(term(), term()) -> ok.
with_value(Key, Value) ->
    erlang:put(Key, Value).

-spec with_value(term(), term(), fun()) -> ok.
with_value(Key, Value, Fun) ->
    Orig = erlang:get(Key),
    try
        erlang:put(Key, Value),
        Fun()
    after
        erlang:put(Key, Orig)
    end.
