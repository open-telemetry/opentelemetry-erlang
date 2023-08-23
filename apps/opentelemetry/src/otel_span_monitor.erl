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
-module(otel_span_monitor).

-behaviour(gen_server).

-export([start_link/0,
         add/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-include("otel_span_ets.hrl").
-include("otel_span.hrl").
-include("otel_tracer.hrl").

-define(SERVER, ?MODULE).

-record(state, {monitors :: #{pid() => reference()}}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Monitor a process and end all spans that have been active in it
%% and are still alive the process stops.
-spec add(pid()) -> ok.
add(Pid) ->
    gen_server:call(?SERVER, {monitor, Pid}).

init(_Opts) ->
    {ok, #state{monitors=#{}}}.

handle_call({monitor, Pid}, _From, State=#state{monitors=Monitors})
  when is_map_key(Pid, Monitors) ->
    %% already being monitored
    {reply, ok, State};
handle_call({monitor, Pid}, _From, State=#state{monitors=Monitors}) ->
    Ref = erlang:monitor(process, Pid),
    {reply, ok, State#state{monitors=Monitors#{Pid => Ref}}}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, Reason}, State=#state{monitors=Monitors}) ->
    case maps:take(Pid, Monitors) of
        {Ref, Monitors1} ->
            end_spans(Pid, Reason),
            {noreply, State#state{monitors=Monitors1}};
        error ->
            {noreply, State}
    end.

%%

%% ignore these functions because dialyzer doesn't like match spec use of '_'
-dialyzer({nowarn_function, end_spans/2}).
-dialyzer({nowarn_function, match_spec/2}).
-dialyzer({nowarn_function, end_span/3}).
-dialyzer({nowarn_function, select/1}).

%% TODO: need a `select_take' or `match_take' in ets
end_spans(Pid, Reason) ->
    ReasonString = otel_utils:assert_to_binary(io_lib:format("~p", [Reason])),
    DownAttributes = otel_span:process_attributes(#{finished_by_monitor => true}),
    DownEvent = opentelemetry:event('process died', #{reason => ReasonString}),
    Spans = select(Pid),
    [begin
         case ets:take(?SPAN_TAB, SpanId) of
             [] ->
                 ok;
             [Span] ->
                 end_span(Span, DownEvent, DownAttributes)
         end
     end || SpanId <- Spans],
    ok.

select(Pid) ->
    ets:select(?SPAN_TAB, match_spec(Pid, '$1')).

match_spec(Pid, Return) ->
    [{#span{span_id='$1', pid='$2', _='_'},
      [{'=:=', '$2', Pid}],
      [Return]}].

end_span(Span=#span{attributes=Attributes,
                    events=Events,
                    on_end_processors=Processors}, DownEvent, DownAttributes) ->
    Span1 = Span#span{attributes=otel_attributes:set(DownAttributes, Attributes),
                      events=otel_events:add([DownEvent], Events)},
    Processors(Span1).
