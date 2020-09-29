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
%%%-----------------------------------------------------------------------
-module(custom_propagator).

-behaviour(otel_propagator).

%% functions for interacting with the custom context key/value
-export([add_to_context/1,
         context_content/0]).

%% functions for setting up the injector and extractor for custom context key
%% as well as the propagator behaviour implementation inject/extract
-export([propagators/0,
         inject/1,
         extract/2]).

-define(SOMETHING_CTX_KEY, ?MODULE).
-define(SOMETHING_TEXT_ID, <<"something-header-id">>).

add_to_context(Something) ->
    otel_ctx:set_value(?SOMETHING_CTX_KEY, Something).

context_content() ->
    otel_ctx:get_value(?SOMETHING_CTX_KEY).

propagators() ->
    ToText = fun ?MODULE:inject/1,
    FromText = fun ?MODULE:extract/2,
    Inject = otel_ctx:text_map_injector(?SOMETHING_CTX_KEY, ToText),
    Extract = otel_ctx:text_map_extractor(?SOMETHING_CTX_KEY, FromText),

    {Extract, Inject}.

inject(undefined) ->
    [];
inject(Something) ->
    [{?SOMETHING_TEXT_ID, Something}].

extract(TextMap, _) when is_list(TextMap) ->
    case lists:search(fun({Key, _Value}) ->
                              string:equal(Key, ?SOMETHING_TEXT_ID, true, none)
                      end, TextMap) of
        {value, {_, Value}} ->
            Value;
        false ->
            undefined
    end.

%%

