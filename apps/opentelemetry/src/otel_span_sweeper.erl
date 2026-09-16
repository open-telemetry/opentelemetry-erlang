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
%% @doc The span sweeper is a process that can be configured to remove,
%% either by finishing or deleting, spans that are still active after a
%% period of time.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_span_sweeper).

-behaviour(gen_statem).

-export([start_link/1]).

-export([init/1,
         callback_mode/0,
         handle_event/4,
         code_change/4,
         terminate/3]).

-export([storage_size/0]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_span_ets.hrl").
-include("otel_tracer.hrl").
-include("otel_span.hrl").
-include_lib("kernel/include/logger.hrl").

-record(data, {interval :: integer() | infinity,
               strategy :: drop | end_span | failed_attribute_and_end_span | fun((opentelemetry:span()) -> ok),
               ttl :: integer() | infinity,
               storage_size :: integer() | infinity}).

storage_size() ->
    {ets:info(?SPAN_TAB, size), ets:info(?SPAN_TAB, memory) * erlang:system_info({wordsize, external})}.

start_link(Config) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Config], []).

init([#{interval := Interval,
        strategy := Strategy,
        span_ttl := TTL,
        storage_size := StorageSize}]) ->
    {ok, ready, #data{interval=Interval,
                      strategy=Strategy,
                      ttl=maybe_convert_time_unit(TTL),
                      storage_size=StorageSize},
     [hibernate, {state_timeout, Interval, sweep}]}.

maybe_convert_time_unit(infinity) ->
    infinity;
maybe_convert_time_unit(TTL) ->
    erlang:convert_time_unit(TTL, millisecond, native).

callback_mode() ->
    handle_event_function.

handle_event(state_timeout, sweep, _, #data{interval=Interval} = Data) ->
    do_gc(Data),
    {keep_state_and_data, [hibernate, {state_timeout, Interval, sweep}]};
handle_event(_, _, _, _Data) ->
    keep_state_and_data.

code_change(_, State, Data, _) ->
    {ok, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

%%
do_gc(#data{strategy=Strategy,
            ttl=TTL,
            storage_size=infinity}) ->
    sweep_spans(Strategy, TTL);
do_gc(#data{strategy=Strategy,
            ttl=TTL,
            storage_size=MaxSize}) ->

    {_, StorageSize} = storage_size(),

    if
        StorageSize >= 2 * MaxSize ->
            %% High overload kill storage.
            ets:delete_all_objects(?SPAN_TAB);
        StorageSize >= MaxSize ->
            %% Low overload, reduce TTL
            sweep_spans(Strategy, overload_ttl(TTL));
        true ->
            sweep_spans(Strategy, TTL)
    end.

overload_ttl(infinity) ->
    infinity;
overload_ttl(TTL) ->
    TTL div 10.

sweep_spans(_, infinity) ->
    ok;
sweep_spans(drop, TTL) ->
    TooOld = erlang:monotonic_time() - TTL,
    case ets:select_delete(?SPAN_TAB, expired_match_spec(TooOld, true)) of
        0 ->
            ok;
        NumDeleted ->
            ?LOG_INFO("sweep old spans: ttl=~p num_dropped=~p", [TTL, NumDeleted])
    end;
sweep_spans(end_span, TTL) ->
    Expired = select_expired(TTL),
    [begin
         case ets:take(?SPAN_TAB, SpanId) of
             [] ->
                 %% must have finished without needing to be swept
                 ok;
             [Span] ->
                 end_span(Span)
         end
     end || SpanId <- Expired],
    ok;
sweep_spans(failed_attribute_and_end_span, TTL) ->
    ExpiredSpanIds = select_expired(TTL),
    [begin
         case ets:take(?SPAN_TAB, SpanId) of
             [] ->
                 %% must have finished without needing to be swept
                 ok;
             [Span=#span{attributes=Attributes}] ->
                 Span1 = Span#span{attributes=otel_attributes:set(<<"finished_by_sweeper">>, true, Attributes)},
                 end_span(Span1)
         end
     end || SpanId <- ExpiredSpanIds],
    ok;
sweep_spans(Fun, TTL) when is_function(Fun) ->
    Expired = select_expired(TTL),
    [Fun(Span) || Span <- Expired],
    ok.

%% ignore these functions because dialyzer doesn't like match spec use of '_'
-dialyzer({nowarn_function, expired_match_spec/2}).
-dialyzer({nowarn_function, end_span/1}).
-dialyzer({nowarn_function, select_expired/1}).

select_expired(TTL) ->
    TooOld = erlang:monotonic_time() - TTL,
    ets:select(?SPAN_TAB, expired_match_spec(TooOld, '$1')).

expired_match_spec(Time, Return) ->
    [{#span{span_id='$1', start_time='$2', _='_'},
      [{'<', '$2', Time}],
      [Return]}].

end_span(Span=#span{tracestate=Tracestate}) ->
    %% hack to not lose tracestate when ending without span ctx
    Span1 = otel_span_utils:end_span(Span#span{tracestate=Tracestate}),
    {_, #tracer{on_end_processors=Processors}} = opentelemetry:get_tracer(),
    Processors(Span1).
