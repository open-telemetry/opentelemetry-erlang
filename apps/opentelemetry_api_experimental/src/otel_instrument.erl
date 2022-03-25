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
%% @doc All measurements are associated with an instrument.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_instrument).

-export([new/7]).

-type name() :: atom() | unicode:latin1_charlist().
-type description() :: unicode:unicode_binary().
-type kind() :: counter | observable_counter | histogram |
                observable_gauge | updown_counter | observable_updowncounter.
-type unit() :: atom(). %% latin1, maximum length of 63 characters
-type value_type() :: integer | float.

-type t() :: #{meter       := otel_meter:t(),
               name        := name(),
               description := description() | undefined,
               kind        := kind(),
               value_type  := value_type(),
               unit        := unit() | undefined}.

-export_type([t/0,
              name/0,
              description/0,
              kind/0,
              value_type/0,
              unit/0]).

-spec new(module(), otel_meter:t(), kind(), name(), description(), unit(), value_type()) -> t().
new(Module, Meter, Kind, Name, Description, Unit, ValueType) ->
    #{module      => Module,
      meter       => Meter,
      name        => Name,
      description => Description,
      kind        => Kind,
      value_type  => ValueType,
      unit        => Unit}.
