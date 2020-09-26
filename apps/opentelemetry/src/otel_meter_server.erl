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
-module(otel_meter_server).

-behaviour(otel_meter_provider).

-export([init/1,
         register_meter/3]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_meter.hrl").
-include("otel_span.hrl").

-record(state, {meter :: meter(),
                deny_list :: [atom() | {atom(), string()}]}).

init(Opts) ->
    DenyList = proplists:get_value(deny_list, Opts, []),

    Meter = #meter{module=otel_meter_default},
    opentelemetry:set_default_meter({otel_meter_default, Meter}),

    {ok, #state{meter=Meter,
                deny_list=DenyList}}.

register_meter(Name, Vsn, #state{meter=Meter,
                                 deny_list=DenyList}) ->
    %% TODO: support semver constraints in denylist
    case proplists:is_defined(Name, DenyList) of
        true ->
            opentelemetry:set_meter(Name, {otel_meter_noop, []});
        false ->
            InstrumentationLibrary = otel_utils:instrumentation_library(Name, Vsn),
            opentelemetry:set_meter(Name, {Meter#meter.module,
                                           Meter#meter{instrumentation_library=InstrumentationLibrary}})
    end.

%%
