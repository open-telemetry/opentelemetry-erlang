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
-module(ot_ctx_seqtrace).

-export([set_value/4,
         get_value/3,
         get_value/4,
         get_current/2,
         set_current/3,
         clear/2,
         remove/3]).

-behaviour(ot_ctx).

-include_lib("kernel/include/logger.hrl").

%% needed until type specs for seq_trace are fixed
-dialyzer({nowarn_function, get_value/3}).
-dialyzer({nowarn_function, get_value/4}).
-dialyzer({nowarn_function, set_value/4}).
-dialyzer({nowarn_function, clear/2}).
-dialyzer({nowarn_function, remove/3}).
-dialyzer({nowarn_function, get_current/2}).
-dialyzer({nowarn_function, set_current/3}).
-dialyzer({nowarn_function, get_context/0}).

-spec set_value(ot_ctx:context_manager(), term(), term(), term()) -> ok.
set_value(_ContextManager, Namespace, Key, Value) ->
    case get_context() of
        undefined ->
            ok;
        Context ->
            case maps:get(Namespace, Context, undefined) of
                NamespaceContext when is_map(NamespaceContext) ->
                    NamespaceContext1 = maps:put(Key, Value, NamespaceContext),
                    seq_trace:set_token(label, Context#{Namespace => NamespaceContext1}),
                    ok;
                _ ->
                    seq_trace:set_token(label, Context#{Namespace => #{Key => Value}}),
                    ok
            end
    end.

-spec get_value(ot_ctx:context_manager(), term(), term()) -> term().
get_value(ContextManager, Namespace, Key) ->
    get_value(ContextManager, Namespace, Key, undefined).

-spec get_value(ot_ctx:context_manager(), term(), term(), term()) -> term().
get_value(_ContextManager, Namespace, Key, Default) ->
    case get_context() of
        undefined ->
            ok;
        Context ->
            NamespaceContext = maps:get(Namespace, Context, #{}),
            maps:get(Key, NamespaceContext, Default)
    end.

-spec clear(ot_ctx:context_manager(), term()) -> ok.
clear(_ContextManager, Namespace) ->
    case get_context() of
        undefined ->
            ok;
        Context ->
            seq_trace:set_token(label, maps:remove(Namespace, Context)),
            ok
    end.

-spec remove(ot_ctx:context_manager(), term(), term()) -> ok.
remove(_ContextManager, Namespace, Key) ->
    case get_context() of
        undefined ->
            ok;
        Context ->
            case maps:get(Namespace, Context, undefined) of
                NamespaceContext when is_map(NamespaceContext) ->
                    NamespaceContext1 = maps:remove(Key, NamespaceContext),
                    seq_trace:set_token(label, Context#{Namespace => NamespaceContext1}),
                    ok;
                _ ->
                    ok
            end
    end.

-spec get_current(ot_ctx:context_manager(), term()) -> map().
get_current(_ContextManager, Namespace) ->
    case get_context() of
        undefined ->
            #{};
        Context ->
            case maps:get(Namespace, Context) of
                Map when is_map(Map) ->
                    Map;
                _ ->
                    #{}
            end
    end.

-spec set_current(ot_ctx:context_manager(), term(), map()) -> ok.
set_current(_ContextManager, Namespace, Ctx) ->
    erlang:put(Namespace, Ctx).

%% internal functions

get_context() ->
    case seq_trace:get_token(label) of
        {label, Label} when is_map(Label) ->
            Label;
        [] ->
            #{};
        {label, _Label} ->
            log_warning(),
            undefined
    end.

log_warning() ->
    ?LOG_WARNING("seq_trace label must be a map to be used for opentelemetry span context").
