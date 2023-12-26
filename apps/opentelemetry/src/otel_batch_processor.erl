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
%% @doc The Batch Span Processor implements the {@link otel_span_processor}
%% behaviour.
%% 
%% It stores finished Spans in a ETS table buffer and exports
%% them on an interval or when the table reaches a maximum size.
%%
%% You can configure these timeouts:
%%
%% <ul>
%% <li>`exporting_timeout_ms': how long to let the exports run before killing.</li>
%% <li>`check_table_size_ms': timeout to check the size of the export table.</li>
%% <li>`scheduled_delay_ms': how often to trigger running the exporters.</li>
%% </ul>
%%
%% The size limit of the current table where finished spans are stored can
%% be configured with the `max_queue_size' option.
%% @end
%%%-----------------------------------------------------------------------
-module(otel_batch_processor).


-behaviour(otel_span_processor).

-export([start_link/1,
         on_start/3,
         on_end/2,
         force_flush/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("kernel/include/logger.hrl").
-include("otel_span.hrl").

-type batch_processor_config() ::
        #{name := atom(),
          max_queue_size => otel_batch_olp:max_queue_size(),
          exporting_timeout_ms => otel_batch_olp:otel_timeout_ms(),
          scheduled_delay_ms => otel_batch_olp:otel_timeout_ms(),
          exporter => otel_exporter:exporter_config()}.

-define(DEFAULT_MAX_QUEUE_SIZE, 2048).
-define(DEFAULT_SCHEDULED_DELAY_MS, timer:seconds(5)).
-define(DEFAULT_EXPORTER_TIMEOUT_MS, timer:seconds(30)).
-define(DEFAULT_EXPORTER_MODULE, opentelemetry_exporter).
-define(DEFAULT_EXPORTER, {?DEFAULT_EXPORTER_MODULE, #{}}).

-spec start_link(batch_processor_config()) ->
          {ok, pid(), otel_batch_olp:otel_batch_olp_state()} | {error, term()}.
start_link(#{name := Name} = Config) ->
    %% TODO: resource should be passed in from the tracer server, now it is detectd by otel_batch_olp
    Config1 = maps:merge(default_config(), maps:remove(name, Config)),
    Config2 = Config1#{reg_name => ?REG_NAME(Name),
                       cb_module => ?MODULE,
                       otel_signal => traces,
                       %% trace_id is expected to have high entropy and, thus, can improve concurrency,
                       %% it is not unique per span, but it doesn't matter as the table type
                       %% is `duplicate_bag`
                       extra_ets_opts => [{keypos, #span.trace_id}]},
    case otel_batch_olp:init_conf(Config2) of
        {ok, OlpState} ->
            case otel_batch_olp:start_link(OlpState, Config) of
                {ok, Pid} ->
                    {ok, Pid, OlpState};
                Err ->
                    Err
            end;
        Err ->
            Err
    end.

%% @private
-spec on_start(otel_ctx:t(), opentelemetry:span(), otel_span_processor:processor_config())
              -> opentelemetry:span().
on_start(_Ctx, Span, _) ->
    Span.

-spec on_end(opentelemetry:span(),  otel_batch_olp:otel_batch_olp_state()) ->
          true | dropped | {error, term()}.
on_end(#span{trace_flags=TraceFlags}, _) when not(?IS_SAMPLED(TraceFlags)) ->
    dropped;
on_end(Span=#span{}, Config) ->
    otel_batch_olp:insert_signal(Span, Config);
on_end(_Span, _) ->
    {error, invalid_span}.

-spec force_flush(otel_batch_olp:otel_batch_olp_state()) -> ok.
force_flush(Config) ->
    otel_batch_olp:force_flush(Config).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

default_config() ->
    #{max_queue_size => ?DEFAULT_MAX_QUEUE_SIZE,
      exporting_timeout_ms => ?DEFAULT_EXPORTER_TIMEOUT_MS,
      scheduled_delay_ms => ?DEFAULT_SCHEDULED_DELAY_MS,
      exporter => ?DEFAULT_EXPORTER}.
