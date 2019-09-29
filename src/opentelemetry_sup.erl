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
-module(opentelemetry_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Children, Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Children, Opts]).

init([Children, Opts]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},

    ReporterOpts = proplists:get_value(reporter, Opts, []),
    Reporter = #{id => ot_reporter,
                 start => {ot_reporter, start_link, [ReporterOpts]},
                 restart => permanent,
                 shutdown => 1000,
                 type => worker,
                 modules => [ot_reporter]},

    ChildSpecs = [#{id => ot_span_sup,
                    start => {ot_span_sup, start_link, [Opts]},
                    type => supervisor},
                  Reporter | Children],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
