%%%------------------------------------------------------------------------
%% Copyright 2020, OpenTelemetry Authors
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
%% @doc All measurements are associated with an instrument. To record
%% measurements for an instrument it must first be created with `new' and
%% then can be referenced by name.
%% @end
%%%-------------------------------------------------------------------------
-module(ot_instrument).

%% @doc Calls the SDK to create a new instrument which can then be referenced
%% by name.
%% @end
-callback new(opentelemetry:meter(), ot_meter:name()) -> boolean().
-callback new(opentelemetry:meter(), ot_meter:name(), ot_meter:instrument_opts()) -> boolean().

%% @doc Returns an instrument definition which can be used to create a new instrument
%% by passing to `ot_meter:new_instruments/1'
%% @end
-callback definition(ot_meter:name()) -> ot_meter:instrument_definition().
-callback definition(ot_meter:name(), ot_meter:instrument_opts()) -> ot_meter:instrument_definition().

%% @doc Used by additive instruments to record measurements.
-callback add(ot_meter:bound_instrument(), number()) -> ok.
-callback add(opentelemetry:meter(), ot_meter:name(), number(), ot_meter:labels()) -> ok.

%% @doc Used by non-additive instruments to record measurements.
-callback record(ot_meter:bound_instrument(), number()) -> ok.
-callback record(opentelemetry:meter(), ot_meter:name(), number(), ot_meter:labels()) -> ok.

%% @doc Returns a measurement tuple that can be based to a batch recording through `ot_meter:batch_record/3'
-callback measurement(ot_meter:bound_instrument() | ot_meter:name(), number()) ->
    {ot_meter:bound_instrument() | ot_meter:name(), number()}.

-optional_callbacks([add/2,
                     add/4,
                     record/2,
                     record/4,
                     measurement/2]).
