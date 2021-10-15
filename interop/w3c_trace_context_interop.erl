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
-module(w3c_trace_context_interop).

-export([run/0,
         do/1]).

-include_lib("inets/include/httpd.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

run() ->
    {ok, _Pid} = inets:start(httpd, [{port, 5000},
                                     {server_root, "/tmp"},
                                     {document_root, "/tmp"},
                                     {server_name, "trace_context_interop"},
                                     {bind_address, "localhost"},
                                     {modules, [?MODULE]}]),
    ok.

do(Req) ->
    Body = Req#mod.entity_body,

    %% inets gives headers in the reverse order they came in
    Headers = headers_to_binary(lists:reverse(Req#mod.parsed_header)),
    List = jsone:decode(list_to_binary(Body)),

    lists:foreach(fun(#{<<"arguments">> := Arguments,
                        <<"url">> := Url}) ->
                          %% not really needed, but just to be safe
                          otel_ctx:clear(),

                          otel_propagator_text_map:extract(Headers),
                          ?with_span(<<"interop-test">>, #{},
                                     fun(_) ->
                                             InjectedHeaders = otel_propagator_text_map:inject([]),
                                             httpc:request(post, {binary_to_list(Url),
                                                                  headers_to_list(InjectedHeaders),
                                                                  "application/json",
                                                                  jsone:encode(Arguments)}, [], [{body_format, binary}])
                                     end)
                  end, List),

    {proceed, [{response, {200, "ok"}}]}.

%%

headers_to_binary(Headers) ->
    [{list_to_binary(K), list_to_binary(V)} || {K, V} <- Headers].

headers_to_list(Headers) ->
    [{binary_to_list(K), binary_to_list(iolist_to_binary(V))} || {K, V} <- Headers].
