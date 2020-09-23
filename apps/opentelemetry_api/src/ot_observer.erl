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
%% @doc An `Observer' is a callback based instrument with a `LastValue' aggregator.
%% On each collection cycle each Observer callback is run and passed an
%% `ObserverInstrument' variable to use when `observe'ing the new value, passing
%% the `ObserverInstrument', the new value and a list of attributes, key/value pairs.
%% @end
%%%-------------------------------------------------------------------------
-module(ot_observer).

%% value containing all information needed by the SDK to record an update
-type instrument() :: term().

%% function called with an `observer_instrument' argument to update observer
-type callback() :: fun((instrument()) -> ok).

-export_type([callback/0]).

-callback set_callback(opentelemetry:meter(), ot_meter:name(), callback()) -> ok.
-callback observe(instrument(), number(), ot_meter:labels()) -> ok.
