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
%% @doc A test reporter for sending trace spans to an Erlang PID as message.
%% @end
%%%-----------------------------------------------------------------------
-module(ot_exporter_pid).

-behaviour(ot_exporter).

-export([init/1,
         export/2]).

init(Pid) ->
    Pid.

export(SpansTid, Pid) ->
    ets:foldl(fun(Span, _Acc) ->
                      Pid ! {span, Span}
              end, [], SpansTid),
    ok.
