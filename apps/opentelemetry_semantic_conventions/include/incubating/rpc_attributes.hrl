
%%%------------------------------------------------------------------------
%% Copyright The OpenTelemetry Authors
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
%%%-------------------------------------------------------------------------
%% @deprecated Replaced by `rpc.message.compressed_size`.
%% Deprecated, use `rpc.message.compressed_size` instead.
-define(MESSAGE_COMPRESSEDSIZE, 'message.compressed_size').

%% @deprecated Replaced by `rpc.message.id`.
%% Deprecated, use `rpc.message.id` instead.
-define(MESSAGE_ID, 'message.id').

%% @deprecated Replaced by `rpc.message.type`.
%% Deprecated, use `rpc.message.type` instead.
-define(MESSAGE_TYPE, 'message.type').

-define('MESSAGE_TYPE_VALUES.sent', 'SENT').

-define('MESSAGE_TYPE_VALUES.received', 'RECEIVED').


%% @deprecated Replaced by `rpc.message.uncompressed_size`.
%% Deprecated, use `rpc.message.uncompressed_size` instead.
-define(MESSAGE_UNCOMPRESSEDSIZE, 'message.uncompressed_size').


%% The [error codes](https://connect.build/docs/protocol/#error-codes) of the Connect request. Error codes are always string values.
-define(RPC_CONNECTRPC_ERRORCODE, 'rpc.connect_rpc.error_code').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.cancelled', 'cancelled').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.unknown', 'unknown').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.invalid_argument', 'invalid_argument').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.deadline_exceeded', 'deadline_exceeded').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.not_found', 'not_found').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.already_exists', 'already_exists').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.permission_denied', 'permission_denied').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.resource_exhausted', 'resource_exhausted').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.failed_precondition', 'failed_precondition').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.aborted', 'aborted').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.out_of_range', 'out_of_range').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.unimplemented', 'unimplemented').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.internal', 'internal').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.unavailable', 'unavailable').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.data_loss', 'data_loss').

-define('RPC_CONNECTRPC_ERRORCODE_VALUES.unauthenticated', 'unauthenticated').



%% Connect request metadata, `<key>` being the normalized Connect Metadata key (lowercase), the value being the metadata values.
%%  
-define(RPC_CONNECTRPC_REQUEST_METADATA, 'rpc.connect_rpc.request.metadata').


%% Connect response metadata, `<key>` being the normalized Connect Metadata key (lowercase), the value being the metadata values.
%%  
-define(RPC_CONNECTRPC_RESPONSE_METADATA, 'rpc.connect_rpc.response.metadata').


%% gRPC request metadata, `<key>` being the normalized gRPC Metadata key (lowercase), the value being the metadata values.
%%  
-define(RPC_GRPC_REQUEST_METADATA, 'rpc.grpc.request.metadata').


%% gRPC response metadata, `<key>` being the normalized gRPC Metadata key (lowercase), the value being the metadata values.
%%  
-define(RPC_GRPC_RESPONSE_METADATA, 'rpc.grpc.response.metadata').


%% The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request.
-define(RPC_GRPC_STATUSCODE, 'rpc.grpc.status_code').

-define('RPC_GRPC_STATUSCODE_VALUES.ok', '0').

-define('RPC_GRPC_STATUSCODE_VALUES.cancelled', '1').

-define('RPC_GRPC_STATUSCODE_VALUES.unknown', '2').

-define('RPC_GRPC_STATUSCODE_VALUES.invalid_argument', '3').

-define('RPC_GRPC_STATUSCODE_VALUES.deadline_exceeded', '4').

-define('RPC_GRPC_STATUSCODE_VALUES.not_found', '5').

-define('RPC_GRPC_STATUSCODE_VALUES.already_exists', '6').

-define('RPC_GRPC_STATUSCODE_VALUES.permission_denied', '7').

-define('RPC_GRPC_STATUSCODE_VALUES.resource_exhausted', '8').

-define('RPC_GRPC_STATUSCODE_VALUES.failed_precondition', '9').

-define('RPC_GRPC_STATUSCODE_VALUES.aborted', '10').

-define('RPC_GRPC_STATUSCODE_VALUES.out_of_range', '11').

-define('RPC_GRPC_STATUSCODE_VALUES.unimplemented', '12').

-define('RPC_GRPC_STATUSCODE_VALUES.internal', '13').

-define('RPC_GRPC_STATUSCODE_VALUES.unavailable', '14').

-define('RPC_GRPC_STATUSCODE_VALUES.data_loss', '15').

-define('RPC_GRPC_STATUSCODE_VALUES.unauthenticated', '16').



%% `error.code` property of response if it is an error response.
-define(RPC_JSONRPC_ERRORCODE, 'rpc.jsonrpc.error_code').


%% `error.message` property of response if it is an error response.
-define(RPC_JSONRPC_ERRORMESSAGE, 'rpc.jsonrpc.error_message').


%% `id` property of request or response. Since protocol allows id to be int, string, `null` or missing (for notifications), value is expected to be cast to string for simplicity. Use empty string in case of `null` value. Omit entirely if this is a notification.
%%  
-define(RPC_JSONRPC_REQUESTID, 'rpc.jsonrpc.request_id').


%% Protocol version as in `jsonrpc` property of request/response. Since JSON-RPC 1.0 doesn't specify this, the value can be omitted.
-define(RPC_JSONRPC_VERSION, 'rpc.jsonrpc.version').


%% Compressed size of the message in bytes.
-define(RPC_MESSAGE_COMPRESSEDSIZE, 'rpc.message.compressed_size').


%% MUST be calculated as two different counters starting from `1` one for sent messages and one for received message.
-define(RPC_MESSAGE_ID, 'rpc.message.id').


%% Whether this is a received or sent message.
-define(RPC_MESSAGE_TYPE, 'rpc.message.type').

-define('RPC_MESSAGE_TYPE_VALUES.sent', 'SENT').

-define('RPC_MESSAGE_TYPE_VALUES.received', 'RECEIVED').



%% Uncompressed size of the message in bytes.
-define(RPC_MESSAGE_UNCOMPRESSEDSIZE, 'rpc.message.uncompressed_size').


%% The name of the (logical) method being called, must be equal to the $method part in the span name.
-define(RPC_METHOD, 'rpc.method').


%% The full (logical) name of the service being called, including its package name, if applicable.
-define(RPC_SERVICE, 'rpc.service').


%% A string identifying the remoting system. See below for a list of well-known identifiers.
-define(RPC_SYSTEM, 'rpc.system').

-define('RPC_SYSTEM_VALUES.grpc', 'grpc').

-define('RPC_SYSTEM_VALUES.java_rmi', 'java_rmi').

-define('RPC_SYSTEM_VALUES.dotnet_wcf', 'dotnet_wcf').

-define('RPC_SYSTEM_VALUES.apache_dubbo', 'apache_dubbo').

-define('RPC_SYSTEM_VALUES.connect_rpc', 'connect_rpc').

