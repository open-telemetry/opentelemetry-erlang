%%%------------------------------------------------------------------------
%% Copyright 2024, OpenTelemetry Authors
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
%% configure opentelemetry_experimental with:
%%
%%     {opentelemetry_experimental,
%%      [{readers,
%%        [
%%         #{module => erccn_otel_metric_reader_prometheus,
%%           config => #{add_scope_info => false,
%%                       add_total_suffix => true
%%                       server_name => otel_cowboy_prometheus_reader}
%%          }
%%        ]}
%%      ]}
%%
%% and add something like this to your cowboy routes:
%%
%%  {"/metrics/[:registry]", otel_cowboy_prometheus_h, #{server_name => otel_cowboy_prometheus_reader}}
%%
%% @end
%%%-------------------------------------------------------------------------
-module(otel_cowboy_prometheus_h).

-behavior(cowboy_rest).

-export([init/2, content_types_provided/2,
         handle_request_text/2,
         allowed_methods/2]).

-ignore_xref([handle_request_text/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"plain">>, '*'} , handle_request_text}
     ], Req, State}.

handle_request_text(Req0, #{server_name := ServerName} = State) ->
    Metrics = erccn_otel_metric_reader_prometheus:collect(ServerName),
    Body = iolist_to_binary(Metrics),

    Req = cowboy_req:reply(200, #{}, Body, Req0),
    {stop, Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
