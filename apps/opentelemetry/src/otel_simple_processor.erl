%%%------------------------------------------------------------------------
%% Copyright 2021, OpenTelemetry Authors
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
%% @doc This Span Processor synchronously exports each ended Span.
%%
%% Use this processor if ending a Span should block until it has been
%% exported. This is useful for cases like a serverless environment where
%% the application will possibly be suspended after handling a request.
%%
%% @end
%%%-----------------------------------------------------------------------
-module(otel_simple_processor).

-behaviour(gen_statem).
-behaviour(otel_span_processor).

-export([start_link/1,
         on_start/3,
         on_end/2,
         force_flush/1,
         report_cb/1,

         %% deprecated
         set_exporter/1,
         set_exporter/2,
         set_exporter/3]).

-export([init/1,
         callback_mode/0,
         idle/3,
         exporting/3,
         terminate/3]).

%% uncomment when OTP-23 becomes the minimum required version
%% -deprecated({set_exporter, 1, "set through the otel_tracer_provider instead"}).
%% -deprecated({set_exporter, 2, "set through the otel_tracer_provider instead"}).
%% -deprecated({set_exporter, 3, "set through the otel_tracer_provider instead"}).

-eqwalizer({nowarn_function, on_end/2}).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("kernel/include/logger.hrl").
-include("otel_span.hrl").

-record(data, {exporter             :: {module(), term()} | undefined,
               exporter_config      :: {module(), term()} | undefined | none,
               current_from         :: gen_statem:from() | undefined,
               resource             :: otel_resource:t(),
               handed_off_table     :: atom() | undefined,
               runner_pid           :: pid() | undefined,
               exporting_timeout_ms :: integer()}).

-define(DEFAULT_EXPORTER_TIMEOUT_MS, timer:minutes(5)).
-define(NAME_TO_ATOM(Name, Unique), list_to_atom(lists:concat([Name, "_", Unique]))).

%% require a unique name to distiguish multiple simple processors while
%% still having a single name, instead of a possibly changing pid, to
%% communicate with the processor
%% @doc Starts a Simple Span Processor.
%% @end
-spec start_link(#{name := atom() | list()}) -> {ok, pid(), map()}.
start_link(Config=#{name := Name}) ->
    RegisterName = ?NAME_TO_ATOM(?MODULE, Name),
    Config1 = Config#{reg_name => RegisterName},
    {ok, Pid} = gen_statem:start_link({local, RegisterName}, ?MODULE, [Config1], []),
    {ok, Pid, Config1}.

%% @deprecated Please use {@link otel_tracer_provider}
set_exporter(Exporter) ->
    set_exporter(global, Exporter, []).

%% @deprecated Please use {@link otel_tracer_provider}
-spec set_exporter(module(), term()) -> ok.
set_exporter(Exporter, Options) ->
    gen_statem:call(?REG_NAME(global), {set_exporter, {Exporter, Options}}).

%% @deprecated Please use {@link otel_tracer_provider}
-spec set_exporter(atom(), module(), term()) -> ok.
set_exporter(Name, Exporter, Options) ->
    gen_statem:call(?REG_NAME(Name), {set_exporter, {Exporter, Options}}).

%% @private
-spec on_start(otel_ctx:t(), opentelemetry:span(), otel_span_processor:processor_config())
              -> opentelemetry:span().
on_start(_Ctx, Span, _) ->
    Span.

%% @private
-spec on_end(opentelemetry:span(), otel_span_processor:processor_config())
            -> true | dropped | {error, invalid_span} | {error, no_export_buffer}.
on_end(#span{trace_flags=TraceFlags}, _) when not(?IS_SAMPLED(TraceFlags)) ->
    dropped;
on_end(Span=#span{}, #{reg_name := RegName}) ->
    gen_statem:call(RegName, {export, Span});
on_end(_Span, _) ->
    {error, invalid_span}.

%% @private
-spec force_flush(#{reg_name := gen_statem:server_ref()}) -> ok.
force_flush(#{reg_name := RegName}) ->
    gen_statem:cast(RegName, force_flush).

%% @private
init([Args]) ->
    process_flag(trap_exit, true),

    ExportingTimeout = maps:get(exporting_timeout_ms, Args, ?DEFAULT_EXPORTER_TIMEOUT_MS),

    %% TODO: this should be passed in from the tracer server
    Resource = case maps:find(resource, Args) of
                   {ok, R} ->
                       R;
                   error ->
                       otel_resource_detector:get_resource()
               end,
    %% Resource = otel_tracer_provider:resource(),

    {ok, idle, #data{exporter=undefined,
                     exporter_config=maps:get(exporter, Args, none),
                     resource = Resource,
                     handed_off_table=undefined,
                     exporting_timeout_ms=ExportingTimeout},
     [{next_event, internal, init_exporter}]}.

%% @private
callback_mode() ->
    state_functions.

%% @private
idle({call, From}, {export, _Span}, #data{exporter=undefined}) ->
    {keep_state_and_data, [{reply, From, dropped}]};
idle({call, From}, {export, Span}, Data) ->
    {next_state, exporting, Data, [{next_event, internal, {export, From, Span}}]};
idle(EventType, Event, Data) ->
    handle_event_(idle, EventType, Event, Data).

%% @private
exporting({call, _From}, {export, _}, _) ->
    {keep_state_and_data, [postpone]};
exporting(internal, {export, From, Span}, Data=#data{exporting_timeout_ms=ExportingTimeout}) ->
    {OldTableName, RunnerPid} = export_span(Span, Data),
    {keep_state, Data#data{runner_pid=RunnerPid,
                           current_from=From,
                           handed_off_table=OldTableName},
     [{state_timeout, ExportingTimeout, exporting_timeout}]};
exporting(state_timeout, exporting_timeout, Data=#data{current_from=From,
                                                       handed_off_table=_ExportingTable}) ->
    %% kill current exporting process because it is taking too long
    %% which deletes the exporting table, so create a new one and
    %% repeat the state to force another span exporting immediately
    Data1 = kill_runner(Data),
    {next_state, idle, Data1, [{reply, From, {error, timeout}}]};
%% important to verify runner_pid and FromPid are the same in case it was sent
%% after kill_runner was called but before it had done the unlink
exporting(info, {'EXIT', FromPid, _}, Data=#data{runner_pid=FromPid}) ->
    complete_exporting(Data);
%% important to verify runner_pid and FromPid are the same in case it was sent
%% after kill_runner was called but before it had done the unlink
exporting(info, {completed, FromPid}, Data=#data{runner_pid=FromPid}) ->
    complete_exporting(Data);
exporting(EventType, Event, Data) ->
    handle_event_(exporting, EventType, Event, Data).

handle_event_(_, internal, init_exporter, Data=#data{exporter=undefined,
                                                     exporter_config=ExporterConfig}) ->
    Exporter = otel_exporter:init(ExporterConfig),
    {keep_state, Data#data{exporter=Exporter}};
handle_event_(_, {call, From}, {set_exporter, Exporter}, Data=#data{exporter=OldExporter}) ->
    otel_exporter:shutdown(OldExporter),
    {keep_state, Data#data{exporter=otel_exporter:init(Exporter)}, [{reply, From, ok}]};
handle_event_(_, _, _, _) ->
    keep_state_and_data.

%% @private
terminate(_, _, _Data) ->
    ok.

%%

complete_exporting(Data=#data{current_from=From,
                              handed_off_table=ExportingTable})
  when ExportingTable =/= undefined ->
    {next_state, idle, Data#data{current_from=undefined,
                                 runner_pid=undefined,
                                 handed_off_table=undefined},
     [{reply, From, ok}]}.

kill_runner(Data=#data{runner_pid=RunnerPid}) when RunnerPid =/= undefined ->
    Mon = erlang:monitor(process, RunnerPid),
    erlang:unlink(RunnerPid),
    erlang:exit(RunnerPid, kill),
    %% Wait for the runner process termination to be sure that
    %% the export table is destroyed and can be safely recreated
    receive
        {'DOWN', Mon, process, RunnerPid, _} ->
            Data#data{runner_pid=undefined, handed_off_table=undefined}
    end.

new_export_table(Name) ->
     ets:new(Name, [public,
                    {write_concurrency, true},
                    duplicate_bag,
                    %% OpenTelemetry exporter protos group by the
                    %% instrumentation_scope. So using instrumentation_scope
                    %% as the key means we can easily lookup all spans for
                    %% for each instrumentation_scope and export together.
                    {keypos, #span.instrumentation_scope}]).

export_span(Span, #data{exporter=Exporter,
                        resource=Resource}) ->
    Table = new_export_table(otel_simple_processor_table),
    _ = ets:insert(Table, Span),
    Self = self(),
    RunnerPid = erlang:spawn_link(fun() -> send_spans(Self, Resource, Exporter) end),
    ets:give_away(Table, RunnerPid, export),
    {Table, RunnerPid}.

%% Additional benefit of using a separate process is calls to `register` won't
%% timeout if the actual exporting takes longer than the call timeout
send_spans(FromPid, Resource, Exporter) ->
    receive
        {'ETS-TRANSFER', Table, FromPid, export} ->
            export(Exporter, Resource, Table),
            ets:delete(Table),
            completed(FromPid)
    end.

completed(FromPid) ->
    FromPid ! {completed, self()}.

export(undefined, _, _) ->
    true;
export({ExporterModule, Config}, Resource, SpansTid) ->
    %% don't let a exporter exception crash us
    %% and return true if exporter failed
    try
        otel_exporter:export_traces(ExporterModule, SpansTid, Resource, Config)
    catch
        Kind:Reason:StackTrace ->
            ?LOG_INFO(#{source => exporter,
                        during => export,
                        kind => Kind,
                        reason => Reason,
                        exporter => ExporterModule,
                        stacktrace => StackTrace}, #{report_cb => fun ?MODULE:report_cb/1}),
            true
    end.

%% logger format functions
%% @private
report_cb(#{source := exporter,
            during := export,
            kind := Kind,
            reason := Reason,
            exporter := ExporterModule,
            stacktrace := StackTrace}) ->
    {"exporter threw exception: exporter=~p ~ts",
     [ExporterModule, otel_utils:format_exception(Kind, Reason, StackTrace)]}.
