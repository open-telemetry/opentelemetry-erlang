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
%% @doc Ctx is responsible for propagating values within a process that
%% are associated with a particular Trace or set of Baggage.
%% `OpenTelemetry.Tracer' and `OpenTelemetry.Baggage' handle updating
%% the Context.
%%
%% Functions in this module include variants that explicitly take a `Ctx'
%% argument and variants that implicitly use the <i>current context</i>, which is
%% the context stored in the process dictionary.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_ctx).

-export([new/0,
         set_value/2,
         set_value/3,
         get_value/1,
         get_value/2,
         get_value/3,
         remove/1,
         remove/2,
         clear/0,
         clear/1,

         attach/1,
         detach/1,
         with_ctx/2,
         get_current/0,

         text_map_extractor/2,
         text_map_injector/2,
         text_map_extractor_fun/3,
         text_map_injector_fun/3]).

-type t() :: map() | undefined.
%% A context map.

-type key() :: term().
%% A context key.

-type value() :: term().
%% A context value.

%% at this time the "token" is actually the context map itself
-opaque token() :: t().
%% An opaque token that represents a context.

-export_type([t/0,
              token/0,
              key/0,
              value/0
            ]).

-define(CURRENT_CTX, '$__current_otel_ctx').

%% @doc Creates a new context.
-spec new() -> t().
new() ->
    #{}.

%% @doc Sets a value in the current context under the given key.
-spec set_value(term(), term()) -> ok.
set_value(Key, Value) ->
    erlang:put(?CURRENT_CTX, set_value(erlang:get(?CURRENT_CTX), Key, Value)),
    ok.

%% @doc Sets a value in the given context under the given key.
%%
%% Returns the updated context.
-spec set_value(t(), term(), term()) -> t().
set_value(Ctx, Key, Value) when is_map(Ctx) ->
    Ctx#{Key => Value};
set_value(_, Key, Value) ->
    #{Key => Value}.

%% @doc Gets a value from the current context under the given key.
-spec get_value(term()) -> eqwalizer:dynamic().
get_value(Key) ->
    get_value(erlang:get(?CURRENT_CTX), Key, undefined).

%% @doc Gets a value from the current context under the given key, or returns the default value
%% if the key is not present in the current context.
-spec get_value(term(), term()) -> eqwalizer:dynamic().
get_value(Key, Default) ->
    get_value(erlang:get(?CURRENT_CTX), Key, Default).

%% @doc Gets a value from the given context under the given key, or returns the default value
%% if the key is not present in the given context or if `Ctx' is `undefined'.
-spec get_value(t(), term(), term()) -> eqwalizer:dynamic().
get_value(undefined, _Key, Default) ->
    Default;
get_value(Ctx, Key, Default) when is_map(Ctx) ->
    maps:get(Key, Ctx, Default);
get_value(_, _, Default) ->
    Default.

%% @doc Removes all key-value pairs from the current context.
-spec clear() -> ok.
clear() ->
    erlang:erase(?CURRENT_CTX),
    ok.

%% @doc Removes all key-value pairs from the given context.
%%
%% Returns an empty context.
-spec clear(t()) -> t().
clear(_Ctx) ->
    new().

%% @doc Removes the value under the given key from the current context.
-spec remove(term()) -> ok.
remove(Key) ->
    case erlang:get(?CURRENT_CTX) of
        Map when is_map(Map) ->
            erlang:put(?CURRENT_CTX, maps:remove(Key, Map)),
            ok;
        _ ->
            ok
    end.

%% @doc Removes the value under the given key from the given context.
%%
%% Returns the updated context.
-spec remove(t(), term()) -> t().
remove(Ctx, Key) when is_map(Ctx) ->
    maps:remove(Key, Ctx);
remove(_, _) ->
    new().

%% @doc Returns the current context.
-spec get_current() -> map().
get_current() ->
    case erlang:get(?CURRENT_CTX) of
        Map when is_map(Map) ->
            Map;
        _ ->
            #{}
    end.

%% @doc Attaches the given context to the current process.
%%
%% Essentially, this sets `Ctx' as the <i>current context</i>
%% .
-spec attach(t()) -> token().
attach(Ctx) ->
    update_logger_process_metadata(Ctx),
    erlang:put(?CURRENT_CTX, Ctx).

%% @doc Detaches the given context from the current process.
-spec detach(token()) -> t() | undefined.
detach(Token) ->
    %% at this time `Token' is a context
    update_logger_process_metadata(Token),
    erlang:put(?CURRENT_CTX, Token).

%% @doc Attaches a context and runs a function, detaching the context at the end.
%%
%% Returns the detached context.
-spec with_ctx(t(), fun(() -> term())) -> {term(), t()}.
with_ctx(Ctx, Fun) ->
    Token = otel_ctx:attach(Ctx),
    try
        Result = Fun(),
        {Result, otel_ctx:detach(Token)}
    catch
        C:T:S ->
            otel_ctx:detach(Token),
            erlang:raise(C, T, S)
    end.


%% Extractor and Injector setup functions

%% @private
text_map_extractor(Key, FromText) ->
    {fun ?MODULE:text_map_extractor_fun/3, {Key, FromText}}.

%% @private
text_map_extractor_fun(TextMap, Key, FromText) ->
    New = FromText(TextMap, ?MODULE:get_value(Key, #{})),
    ?MODULE:set_value(Key, New).

%% @private
text_map_injector(Key, ToText) ->
    {fun ?MODULE:text_map_injector_fun/3, {Key, ToText}}.

%% @private
text_map_injector_fun(TextMap, Key, ToText) ->
    TextMap ++ ToText(?MODULE:get_value(Key, undefined)).

%%

update_logger_process_metadata(undefined) ->
    ok;
update_logger_process_metadata(Ctx) ->
    otel_tracer:update_logger_process_metadata(Ctx).
