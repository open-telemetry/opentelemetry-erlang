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
%% HTTP Status helper module. Provides functions to working with HTTP status
%% codes to status records.
%% @end
%%%-------------------------------------------------------------------------
-module(ot_http_status).

-export([to_status/1,
         to_status/2]).

-include("opentelemetry.hrl").

-type http_status_code() :: 1..599.

-callback to_status(http_status_code()) -> opentelemetry:status().
-callback to_status(http_status_code(), unicode:unicode_binary()) -> opentelemetry:status().

-spec to_status(HttpStatusCode) -> opentelemetry:status() when
      HttpStatusCode :: http_status_code().
to_status(HttpStatusCode) ->
    to_status(HttpStatusCode, <<"">>).

-spec to_status(HttpStatusCode, Message) -> opentelemetry:status() when
      HttpStatusCode :: http_status_code(),
      Message :: unicode:unicode_binary().
to_status(HttpStatusCode, Message) ->
    Status =
        case HttpStatusCode of
            Code when Code >= 100 andalso Code < 300 ->
                opentelemetry:status(?OTEL_STATUS_OK, Message);
            Code when Code >= 300 andalso Code < 400 ->
                opentelemetry:status(?OTEL_STATUS_OK, Message);
            401 ->
                opentelemetry:status(?OTEL_STATUS_UNAUTHENTICATED, Message);
            403 ->
                opentelemetry:status(?OTEL_STATUS_PERMISSION_DENIED, Message);
            404 ->
                opentelemetry:status(?OTEL_STATUS_NOT_FOUND, Message);
            412 ->
                opentelemetry:status(?OTEL_STATUS_FAILED_PRECONDITION, Message);
            416 ->
                opentelemetry:status(?OTEL_STATUS_OUT_OF_RANGE, Message);
            429 ->
                opentelemetry:status(?OTEL_STATUS_RESOURCE_EXHAUSTED, Message);
            Code when Code >= 400 andalso Code < 500 ->
                opentelemetry:status(?OTEL_STATUS_INVALID_ARGUMENT, Message);
            501 ->
                opentelemetry:status(?OTEL_STATUS_UNIMPLEMENTED, Message);
            503 ->
                opentelemetry:status(?OTEL_STATUS_UNAVAILABLE, Message);
            504 ->
                opentelemetry:status(?OTEL_STATUS_DEADLINE_EXCEEDED, Message);
            Code when Code >= 500 ->
                opentelemetry:status(?OTEL_STATUS_INTERNAL, Message);
            _ ->
                opentelemetry:status(?OTEL_STATUS_UNKNOWN, Message)
        end,
    status_message(Status).

status_message(#status{code = Code, message = <<"">>} = Status) ->
    Message =
        case Code of
            ?OTEL_STATUS_OK ->
                <<"Ok">>;
            ?OTEL_STATUS_UNKNOWN ->
                <<"Unknown Status">>;
            ?OTEL_STATUS_INVALID_ARGUMENT ->
                <<"Bad Argument">>;
            ?OTEL_STATUS_DEADLINE_EXCEEDED ->
                <<"Gateway Timeout">>;
            ?OTEL_STATUS_NOT_FOUND ->
                <<"Not Found">>;
            ?OTEL_STATUS_PERMISSION_DENIED ->
                <<"Forbidden">>;
            ?OTEL_STATUS_RESOURCE_EXHAUSTED ->
                <<"Too Many Requests">>;
            ?OTEL_STATUS_FAILED_PRECONDITION ->
                <<"Failed Precondition">>;
            ?OTEL_STATUS_OUT_OF_RANGE ->
                <<"Range Not Satisfiable">>;
            ?OTEL_STATUS_UNIMPLEMENTED ->
                <<"Not Implemented">>;
            ?OTEL_STATUS_INTERNAL ->
                <<"Internal Error">>;
            ?OTEL_STATUS_UNAVAILABLE ->
                <<"Service Unavailable">>;
            ?OTEL_STATUS_UNAUTHENTICATED ->
                <<"Unauthorized">>
        end,
    Status#status{message = Message};
status_message(Status) ->
    Status.
