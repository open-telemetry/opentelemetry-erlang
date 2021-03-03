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
-module(otel_instrument).

%% Calls the SDK to create a new instrument which can then be referenced by name.
-callback new(opentelemetry:meter(), otel_meter:name()) -> boolean().
-callback new(opentelemetry:meter(), otel_meter:name(), otel_meter:instrument_opts()) -> boolean().

%% Returns an instrument definition which can be used to create a new instrument
%% by passing to `otel_meter:new_instruments/1'
-callback definition(otel_meter:name()) -> otel_meter:instrument_definition().
-callback definition(otel_meter:name(), otel_meter:instrument_opts()) -> otel_meter:instrument_definition().

%% Used by additive instruments to record measurements.
-callback add(otel_meter:bound_instrument(), number()) -> ok.
-callback add(opentelemetry:meter(), otel_meter:name(), number(), otel_meter:labels()) -> ok.

%% Used by non-additive instruments to record measurements.
-callback record(otel_meter:bound_instrument(), number()) -> ok.
-callback record(opentelemetry:meter(), otel_meter:name(), number(), otel_meter:labels()) -> ok.

%% Returns a measurement tuple that can be based to a batch recording through `otel_meter:batch_record/3'
-callback measurement(otel_meter:bound_instrument() | otel_meter:name(), number()) ->
    {otel_meter:bound_instrument() | otel_meter:name(), number()}.

-optional_callbacks([add/2,
                     add/4,
                     record/2,
                     record/4,
                     measurement/2]).
