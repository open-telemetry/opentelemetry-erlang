%%%------------------------------------------------------------------------
%% Copyright 2022, OpenTelemetry Authors
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
%% @doc TODO
%% @end
%%%-------------------------------------------------------------------------
-module(otel_metric_reader_prometheus).

-behaviour(supervisor).

-export([start_link/3,
         collect/1,
         shutdown/1]).

-export([init/1,
        do/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("inets/include/httpd.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").

-define(TEMPORALITY_MAPPING, #{
    ?KIND_COUNTER => ?TEMPORALITY_CUMULATIVE,
    ?KIND_OBSERVABLE_COUNTER => ?TEMPORALITY_CUMULATIVE,
    ?KIND_HISTOGRAM => ?TEMPORALITY_CUMULATIVE,
    ?KIND_OBSERVABLE_GAUGE => ?TEMPORALITY_CUMULATIVE,
    ?KIND_UPDOWN_COUNTER => ?TEMPORALITY_CUMULATIVE,
    ?KIND_OBSERVABLE_UPDOWNCOUNTER => ?TEMPORALITY_CUMULATIVE
}).

start_link(ReaderId, ProviderSup, Config) ->
    supervisor:start_link(?MODULE, [ReaderId, ProviderSup, Config]).

init([ReaderId, ProviderSup, Config]) ->
    % TODO warning if default_temporality_mapping, export_interval_ms, exporter
    %      are present in the configuration
    Config1 = maps:put(default_temporality_mapping, ?TEMPORALITY_MAPPING, Config),
    Config2 = maps:remove(export_interval_ms, Config1),
    Config3 = maps:put(exporter, {otel_metric_exporter_prometheus, Config2}, Config2),

    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},

    ReaderChildSpec = #{
        id => ReaderId,
        start => {otel_metric_reader, start_link, [ReaderId, ProviderSup, Config3]},
        type => worker,
        restart => permanent,
        shutdown => 1000
    },

    ChildSpecs = case maps:get(endpoint_port, Config, undefined) of
        undefined ->
            [ReaderChildSpec];
        HttpdPort when is_integer(HttpdPort) ->
             HttpdOpts = [
                {server_name, "OTel Prometheus exporter"}, 
                {server_tokens, {private, "TODO"}},
                {server_root, "/tmp"},
                {document_root, "/tmp"}, 
                {port, HttpdPort}, 
                {modules, [?MODULE]},
                {otel_metric_reader, {self(), ReaderId}},
                {pt_key, make_ref()}
            ],
            HttpdChildSpec = #{
                id => make_ref(),
                start => {inets, start, [httpd, HttpdOpts, stand_alone]},
                type => worker,
                restart => permanent,
                shutdown => 1000
            },
            [ReaderChildSpec, HttpdChildSpec]
    end,

    {ok, {SupFlags, ChildSpecs}}.

collect(ReaderPid) ->
    otel_metric_reader:collect(ReaderPid).

shutdown(ReaderPid) ->
    otel_metric_reader:shutdown(ReaderPid).

do(#mod{method="GET",request_uri="/metrics",config_db=ConfigDb}) ->
    ReaderPid = get_reader_pid(ConfigDb),
    Metrics = collect(ReaderPid),
    Headers = [
        {code, 200},
        {content_length, integer_to_list(iolist_size(Metrics))},
        {content_type, "text/plain; version=0.0.4"}
    ],
    {proceed, [{response, {response, Headers, Metrics}}]};
do(#mod{}) ->
    {proceed, [{response, {404, "Not found"}}]}.

get_reader_pid(ConfigDb) ->
    [PTKey] = ets:lookup_element(ConfigDb, pt_key, 2),
    case persistent_term:get(PTKey, undefined) of
        undefined ->
            [{ReaderSupPid, ReaderId}] = ets:lookup_element(ConfigDb, otel_metric_reader, 2),
            Children = supervisor:which_children(ReaderSupPid),
            {value, {_, ReaderPid, _, _}} = lists:search(fun({Id, _, _, _}) -> Id == ReaderId end, Children),
            persistent_term:put(PTKey, ReaderPid),
            ReaderPid;
        ReaderPid ->
            ReaderPid
    end.