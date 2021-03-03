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
-module(opentelemetry_experimental).

-export([set_meter/2,
         set_default_meter/1,
         get_meter/0,
         get_meter/1,
         register_meter/2,
         register_application_meter/1]).

-include_lib("kernel/include/logger.hrl").

-export_type([meter/0]).

-type meter() :: {module(), term()}.

-spec set_default_meter(meter()) -> boolean().
set_default_meter(Meter) ->
    opentelemetry:verify_and_set_term(?MODULE, Meter, default_meter, otel_meter).

-spec set_meter(atom(), meter()) -> boolean().
set_meter(Name, Meter) ->
    opentelemetry:verify_and_set_term(?MODULE, Meter, Name, otel_meter).

-spec register_meter(atom(), string()) -> boolean().
register_meter(Name, Vsn) ->
    otel_meter_provider:register_meter(Name, Vsn).

-spec register_application_meter(atom()) -> boolean().
register_application_meter(Name) ->
    otel_meter_provider:register_application_meter(Name).

-spec get_meter() -> meter().
get_meter() ->
    persistent_term:get({?MODULE, default_meter}, {otel_meter_noop, []}).

-spec get_meter(atom()) -> meter().
get_meter(Name) ->
    persistent_term:get({?MODULE, Name}, get_meter()).
