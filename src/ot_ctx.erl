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

-export([get/2,
         get/3,
         with_value/3,
         with_value/4]).

-callback get(term(), term()) -> term().
-callback with_value(term(), term()) -> ok.

-spec get(Impl :: module(), term()) -> term().
get(Module, Key) -> get(Module, Key, undefined).

-spec get(Impl :: module(), term(), term()) -> term().
get(Module, Key, Default) -> Module:get(Key, Default).

-spec with_value(Impl :: module(), term(), term()) -> term().
with_value(Module, Key, Value) -> Module:with_value(Key, Value).

-spec with_value(Impl :: module(), term(), term(), fun()) -> term().
with_value(Module, Key, Value, Fun) ->
    Orig = get(Module, Key),
    try
        with_value(Module, Key, Value),
        Fun()
    after
        with_value(Module, Key, Orig)
    end.
