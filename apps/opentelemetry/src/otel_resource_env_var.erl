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
%% @doc Resource detector ({@link otel_resource_detector}) which adds attributes
%% to the `Resource' based on environment variables.
%%
%% This resource detector reads the `OTEL_RESOURCE_ATTRIBUTES' environment
%% variable and parses it as a comma-separated list of key-value pairs. For
% example, `key1=val1,key2=val2'.
%%
%% This detector is on by default (see the default configuration for `resource_detectors' in the
%% `opentelemetry' application environment).
%% @end
%%%-----------------------------------------------------------------------
-module(otel_resource_env_var).

-behaviour(otel_resource_detector).

-export([get_resource/1,
         parse/1]).

-define(OS_ENV, "OTEL_RESOURCE_ATTRIBUTES").
-define(LABEL_LIST_SPLITTER, ",").
-define(LABEL_KEY_VALUE_SPLITTER, "=").

%% @private
get_resource(_Config) ->
    otel_resource:create(parse(os:getenv(?OS_ENV))).

%%

%% @private
-spec parse(false | string()) -> list().
parse(false) ->
    [];
parse(RawLabels) ->
    Labels = string:split(RawLabels, ?LABEL_LIST_SPLITTER, all),
    lists:filtermap(fun(Label) ->
                            case string:split(Label, ?LABEL_KEY_VALUE_SPLITTER, all) of
                                [K, V] ->
                                    V1 = re:replace(string:trim(V), "^\"|\"$", "", [global, {return, list}]),
                                    {true, {string:trim(K), V1}};
                                _ ->
                                    false
                            end
                    end, Labels).
