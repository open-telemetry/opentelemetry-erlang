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
-module(ot_span_sweeper).

-behaviour(gen_statem).

-export([start_link/0]).

-export([init/1,
         callback_mode/0,
         handle_event/4,
         code_change/4,
         terminate/3]).

-export([storage_size/0]).

-include("opentelemetry.hrl").
-include("ot_span_ets.hrl").
-include_lib("kernel/include/logger.hrl").

-record(data, {interval :: integer() | infinity,
               strategy :: drop | finish | failed_attribute_and_finish | fun((opencensus:span()) -> ok),
               ttl :: integer() | infinity,
               storage_size :: integer() | infinity}).

storage_size() ->
    {ets:info(?SPAN_TAB, size), ets:info(?SPAN_TAB, memory) * erlang:system_info({wordsize, external})}.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    SweeperConfig = application:get_env(opencensus, sweeper, #{}),

    Interval = maps:get(interval, SweeperConfig, timer:minutes(5)),
    Strategy = maps:get(strategy, SweeperConfig, drop),
    TTL = maps:get(span_ttl, SweeperConfig, timer:minutes(5)),
    StorageSize = maps:get(storage_size, SweeperConfig, infinity),
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
sweep_spans(finish, TTL) ->
    Expired = select_expired(TTL),
    [finish_span(Span) || Span <- Expired],
    ok;
sweep_spans(failed_attribute_and_finish, TTL) ->
    Expired = select_expired(TTL),
    [finish_span(oc_span:put_attribute(<<"finished_by_sweeper">>, true, Span)) || Span <- Expired],
    ok;
sweep_spans(Fun, TTL) when is_function(Fun) ->
    Expired = select_expired(TTL),
    [Fun(Span) || Span <- Expired],
    ok.

%% ignore these functions because dialyzer doesn't like match spec use of '_'
-dialyzer({nowarn_function, expired_match_spec/2}).
-dialyzer({nowarn_function, finish_span/1}).
-dialyzer({nowarn_function, select_expired/1}).

expired_match_spec(Time, Return) ->
    [{#span{start_time={'$1', '_'}, _='_'},
      [{'<', '$1', Time}],
      [Return]}].

finish_span(S=#span{span_id=SpanId,
                    tracestate=Tracestate}) ->
    %% hack to not lose tracestate when finishing without span ctx
    oc_span:finish_span(#span_ctx{tracestate=Tracestate}, S),
    ets:delete(?SPAN_TAB, SpanId).

select_expired(TTL) ->
    TooOld = erlang:monotonic_time() - TTL,
    ets:select(?SPAN_TAB, expired_match_spec(TooOld, '$_')).
