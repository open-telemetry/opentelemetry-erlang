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
-module(opentelemetry_app).

-behaviour(application).

-export([start/2,
         prep_stop/1,
         stop/1]).

start(_StartType, _StartArgs) ->
    Opts = application:get_all_env(opentelemetry),

    %% set the global propagators for HTTP based on the application env
    setup_http_propagators(Opts),

    opentelemetry_sup:start_link(Opts).

%% called before the supervision tree is shutdown.
prep_stop(_State) ->
    %% on application stop set tracer to the noop implementation.
    %% This is to ensure no crashes if the sdk isn't the last
    %% thing to shutdown or if the opentelemetry application crashed.
    opentelemetry:set_default_tracer({ot_tracer_noop, []}),
    opentelemetry:set_default_meter({ot_meter_noop, []}),
    ok.

stop(_State) ->
    ok.

%% internal functions

setup_http_propagators(Opts) ->
    Propagators = proplists:get_value(http_propagators, Opts, []),

    {Extractors, Injectors} =
        lists:foldl(fun(F, {ExtractorsAcc, InjectorsAcc}) ->
                            {Extractor, Injector} = F(),
                            {[Extractor | ExtractorsAcc], [Injector | InjectorsAcc]}
                    end, {[], []}, Propagators),

    opentelemetry:set_http_extractor(Extractors),
    opentelemetry:set_http_injector(Injectors).
