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
%%
%% @end
%%%-----------------------------------------------------------------------
-module(ot_resource).

-export([create/1,
         merge/2]).

-type key() :: io_lib:latin1_string().
-type value() :: io_lib:latin1_string() | integer() | float() | boolean().
-type resource() :: {resource, #{key() => value()}}.
-opaque t() :: resource().

-export_type([t/0]).

-define(MAX_LENGTH, 255).

%% verifies each key and value and drops any that don't pass verification
-spec create(#{key() => value()}) -> t().
create(Map) ->
    {resource, maps:filter(fun(K, V) ->
                                   %% TODO: log an info or debug message when dropping?
                                   check_key(K) andalso check_value(V)
                           end, Map)}.

%% In case of collision the current, first argument, resource takes precedence.
-spec merge(resource(), resource()) -> t().
merge({resource, CurrentResource}, {resource, OtherResource}) ->
    {resource, maps:merge(OtherResource, CurrentResource)}.

%%

%% all resource strings, key or value, must be latin1 with length less than 255
check_string(S) ->
    length(S) =< ?MAX_LENGTH andalso io_lib:deep_latin1_char_list(S).

%% a resource key must be a non-empty latin1 string
check_key(K) ->
    is_list(K) andalso length(K) > 0 andalso check_string(K).

%% a resource value can be a latin1 string, integer, float or boolean
check_value(V) when is_integer(V) ;
                    is_float(V) ;
                    is_boolean(V) ->
    true;
check_value(V) when is_list(V) ->
    check_string(V);
check_value(_) ->
    false.
