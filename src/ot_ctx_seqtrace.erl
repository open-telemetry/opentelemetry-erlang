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
-module(ot_ctx_seqtrace).

-behaviour(ot_ctx).

-export([get/1,
         get/2,
         with_value/2,
         with_value/3]).

-spec get(term()) -> term().
get(_Key) ->
    ok.

-spec get(term(), term()) -> term().
get(_Key, _Value) ->
    ok.

-spec with_value(term(), term()) -> ok.
with_value(_Key, _Value) ->
    ok.

-spec with_value(term(), term(), fun()) -> ok.
with_value(_Key, _Value, Fun) ->
    Fun().
