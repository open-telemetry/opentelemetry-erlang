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
%% @doc
%% Process that can optionally monitor the process a span is in and end the
%% span if the process stops for any reason with the span still unfinished.
%% @end
%%%-------------------------------------------------------------------------
-module(ot_span_monitor).

-behaviour(gen_server).

-export([start_link/0,
         add_self/0,
         add/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-include("ot_span_ets.hrl").
-include("ot_span.hrl").
-include("ot_tracer.hrl").

-define(SERVER, ?MODULE).

-record(state, {monitors :: #{reference() => pid()},
                monitored_pids :: sets:set(pid())}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Monitor the current process and end all spans in it if the process stops.
-spec add_self() -> ok.
add_self() ->
    gen_server:call(?SERVER, {monitor, self()}).

%% @doc Monitor another process and end all spans in it if the process stops.
-spec add(pid()) -> ok.
add(Pid) ->
    gen_server:call(?SERVER, {monitor, Pid}).

init(_Opts) ->
    {ok, #state{monitors=#{},
                monitored_pids=sets:new()}}.

handle_call({monitor, Pid}, _From, State=#state{monitors=Monitors,
                                                monitored_pids=MonitoredPids}) ->
    case sets:is_element(Pid, MonitoredPids) of
        true ->
            {reply, ok, State};
        false ->
            Ref = erlang:monitor(process, Pid),
            {reply, ok, State#state{monitors=Monitors#{Ref => Pid},
                                    monitored_pids=sets:add_element(Pid, MonitoredPids)}}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Info}, State=#state{monitors=Monitors,
                                                             monitored_pids=MonitoredPids}) ->
    case maps:take(Ref, Monitors) of
        {P, Monitors1} when P =:= Pid ->
            end_spans(Pid),
            {noreply, State#state{monitors=Monitors1,
                                  monitored_pids=sets:del_element(Pid, MonitoredPids)}};
        error ->
            {noreply, State}
    end.

%%

%% ignore these functions because dialyzer doesn't like match spec use of '_'
-dialyzer({nowarn_function, end_spans/1}).
-dialyzer({nowarn_function, match_spec/2}).
-dialyzer({nowarn_function, end_span/1}).
-dialyzer({nowarn_function, select/1}).

%% TODO: need a `select_take' or `match_take' in ets
end_spans(Pid) ->
    Spans = select(Pid),
    [begin
         case ets:take(?SPAN_TAB, SpanId) of
             [] ->
                 ok;
             [Span] ->
                 end_span(Span)
         end
     end || SpanId <- Spans],
    ok.

select(Pid) ->
    ets:select(?SPAN_TAB, match_spec(Pid, '$1')).

match_spec(Pid, Return) ->
    [{#span{span_id='$1', pid='$2', _='_'},
      [{'=:=', '$2', Pid}],
      [Return]}].

end_span(Span=#span{tracestate=Tracestate}) ->
    %% hack to not lose tracestate when ending without span ctx
    Span1 = ot_span_utils:end_span(Span#span{tracestate=Tracestate}),
    {_, #tracer{on_end_processors=Processors}} = opentelemetry:get_tracer(),
    Processors(Span1).
