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

-type key() :: term().
-type instrumented_fun() :: fun(() -> term()).

-export_type([key/0, instrumented_fun/0]).

%% Get value of `Key' from current context and return `Default' if none.
-callback get(Key :: key(), Default :: term()) -> term().

%% Set value of `Key' in current context to `Value'.
-callback with_value(Key :: key(), Value :: term()) -> ok.

-spec get(Impl :: module(), key()) -> term().
get(Module, Key) -> get(Module, Key, undefined).

-spec get(Impl :: module(), key(), term()) -> term().
get(Module, Key, Default) -> Module:get(Key, Default).

-spec with_value(Impl :: module(), key(), term()) -> term().
with_value(Module, Key, Value) -> Module:with_value(Key, Value).

-spec with_value(Impl :: module(), key(), term(), instrumented_fun()) -> term().
with_value(Module, Key, Value, Fun) ->
    Orig = get(Module, Key),
    try
        with_value(Module, Key, Value),
        Fun()
    after
        with_value(Module, Key, Orig)
    end.
