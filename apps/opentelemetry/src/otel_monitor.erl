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
-module(otel_monitor).

-behaviour(gen_server).

-export([start_link/0,
				 monitor/1]).

-export([init/1,
				 handle_call/3,
				 handle_cast/2,
				 handle_info/2]).

-include("otel_tracer.hrl").

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, nil, [{name, ?SERVER}]).

init(nil) ->
  _TableId = ets:new(?TABLE, [set, named_table]),
	{ok, nil}.

handle_call({monitor, SpanCtx, Pid}, _From, State) ->
  Ref = erlang:monitor(process, Pid),
	true = ets:insert(?TABLE, {Ref, SpanCtx}),
	{reply, ok, State}.

handle_info({'DOWN', Ref, process, _Pid, normal}, State) ->
  case ets:take(?TABLE, Ref) of
		[] -> nil;
		[{_Ref, SpanCtx}] -> otel_span:end_span(SpanCtx)
	end,
  {noreply, State};

handle_info({'DOWN', Ref, process, _Pid, {shutdown, _}}, State) ->
  case ets:take(?TABLE, Ref) of
		[] -> nil;
		[{_Ref, SpanCtx}] -> otel_span:end_span(SpanCtx)
	end,
  {noreply, State};

handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
  case ets:take(?TABLE, Ref) of
		[] -> nil;
		[{_Ref, SpanCtx}] ->
      otel_span:add_event(SpanCtx, 'Process died', #{<<"reason">> => iolist_to_binary(io_lib:format("~p", [Reason]))}),
			otel_span:end_span(SpanCtx)
	end,
  {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

monitor(SpanCtx) ->
  ok = gen_server:call(?SERVER, {monitor, SpanCtx, self()}),
  true.
