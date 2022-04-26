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
         get_meter/1]).

-include_lib("kernel/include/logger.hrl").

-export_type([meter/0]).

-type meter() :: {module(), term()}.

-define(METER_KEY(Name), {?MODULE, meter, Name}).

-spec set_default_meter(meter()) -> boolean().
set_default_meter(Meter) ->
    opentelemetry:verify_and_set_term(Meter, ?METER_KEY(default_meter), otel_meter).

-spec set_meter(atom(), meter()) -> boolean().
set_meter(Name, Meter) ->
    opentelemetry:verify_and_set_term(Meter, ?METER_KEY(Name), otel_meter).

-spec get_meter() -> meter().
get_meter() ->
    persistent_term:get(?METER_KEY(default_meter), {otel_meter_noop, []}).

-spec get_meter(atom()) -> meter().
get_meter(Name) ->
    persistent_term:get(?METER_KEY(Name), get_meter()).
