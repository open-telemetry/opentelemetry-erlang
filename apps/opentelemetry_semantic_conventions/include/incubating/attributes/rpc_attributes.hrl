
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
%% @deprecated Deprecated, no replacement at this time.
%% Deprecated, no replacement at this time.
-define(MESSAGE_COMPRESSED_SIZE, 'message.compressed_size').

%% @deprecated Deprecated, no replacement at this time.
%% Deprecated, no replacement at this time.
-define(MESSAGE_ID, 'message.id').

%% @deprecated Deprecated, no replacement at this time.
%% Deprecated, no replacement at this time.
-define(MESSAGE_TYPE, 'message.type').

-define(MESSAGE_TYPE_VALUES_SENT, 'SENT').

-define(MESSAGE_TYPE_VALUES_RECEIVED, 'RECEIVED').


%% @deprecated Deprecated, no replacement at this time.
%% Deprecated, no replacement at this time.
-define(MESSAGE_UNCOMPRESSED_SIZE, 'message.uncompressed_size').

%% @deprecated Replaced by `rpc.response.status_code`.
%% Deprecated, use `rpc.response.status_code` attribute instead.
-define(RPC_CONNECT_RPC_ERROR_CODE, 'rpc.connect_rpc.error_code').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_CANCELLED, 'cancelled').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_UNKNOWN, 'unknown').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_INVALID_ARGUMENT, 'invalid_argument').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_DEADLINE_EXCEEDED, 'deadline_exceeded').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_NOT_FOUND, 'not_found').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_ALREADY_EXISTS, 'already_exists').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_PERMISSION_DENIED, 'permission_denied').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_RESOURCE_EXHAUSTED, 'resource_exhausted').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_FAILED_PRECONDITION, 'failed_precondition').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_ABORTED, 'aborted').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_OUT_OF_RANGE, 'out_of_range').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_UNIMPLEMENTED, 'unimplemented').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_INTERNAL, 'internal').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_UNAVAILABLE, 'unavailable').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_DATA_LOSS, 'data_loss').

-define(RPC_CONNECT_RPC_ERROR_CODE_VALUES_UNAUTHENTICATED, 'unauthenticated').


%% @deprecated Replaced by `rpc.request.metadata`.
%% Deprecated, use `rpc.request.metadata` instead.
%%  
-define(RPC_CONNECT_RPC_REQUEST_METADATA, 'rpc.connect_rpc.request.metadata').

%% @deprecated Replaced by `rpc.response.metadata`.
%% Deprecated, use `rpc.response.metadata` instead.
%%  
-define(RPC_CONNECT_RPC_RESPONSE_METADATA, 'rpc.connect_rpc.response.metadata').

%% @deprecated Replaced by `rpc.request.metadata`.
%% Deprecated, use `rpc.request.metadata` instead.
%%  
-define(RPC_GRPC_REQUEST_METADATA, 'rpc.grpc.request.metadata').

%% @deprecated Replaced by `rpc.response.metadata`.
%% Deprecated, use `rpc.response.metadata` instead.
%%  
-define(RPC_GRPC_RESPONSE_METADATA, 'rpc.grpc.response.metadata').

%% @deprecated Use string representation of the gRPC status code on the `rpc.response.status_code` attribute.
%% Deprecated, use string representation on the `rpc.response.status_code` attribute instead.
-define(RPC_GRPC_STATUS_CODE, 'rpc.grpc.status_code').

-define(RPC_GRPC_STATUS_CODE_VALUES_OK, '0').

-define(RPC_GRPC_STATUS_CODE_VALUES_CANCELLED, '1').

-define(RPC_GRPC_STATUS_CODE_VALUES_UNKNOWN, '2').

-define(RPC_GRPC_STATUS_CODE_VALUES_INVALID_ARGUMENT, '3').

-define(RPC_GRPC_STATUS_CODE_VALUES_DEADLINE_EXCEEDED, '4').

-define(RPC_GRPC_STATUS_CODE_VALUES_NOT_FOUND, '5').

-define(RPC_GRPC_STATUS_CODE_VALUES_ALREADY_EXISTS, '6').

-define(RPC_GRPC_STATUS_CODE_VALUES_PERMISSION_DENIED, '7').

-define(RPC_GRPC_STATUS_CODE_VALUES_RESOURCE_EXHAUSTED, '8').

-define(RPC_GRPC_STATUS_CODE_VALUES_FAILED_PRECONDITION, '9').

-define(RPC_GRPC_STATUS_CODE_VALUES_ABORTED, '10').

-define(RPC_GRPC_STATUS_CODE_VALUES_OUT_OF_RANGE, '11').

-define(RPC_GRPC_STATUS_CODE_VALUES_UNIMPLEMENTED, '12').

-define(RPC_GRPC_STATUS_CODE_VALUES_INTERNAL, '13').

-define(RPC_GRPC_STATUS_CODE_VALUES_UNAVAILABLE, '14').

-define(RPC_GRPC_STATUS_CODE_VALUES_DATA_LOSS, '15').

-define(RPC_GRPC_STATUS_CODE_VALUES_UNAUTHENTICATED, '16').


%% @deprecated Use string representation of the error code on the `rpc.response.status_code` attribute.
%% Deprecated, use string representation on the `rpc.response.status_code` attribute instead.
-define(RPC_JSONRPC_ERROR_CODE, 'rpc.jsonrpc.error_code').

%% @deprecated Use the span status description when reporting JSON-RPC spans.
%% Deprecated, use the span status description when reporting JSON-RPC spans.
-define(RPC_JSONRPC_ERROR_MESSAGE, 'rpc.jsonrpc.error_message').

%% @deprecated Replaced by `jsonrpc.request.id`.
%% Deprecated, use `jsonrpc.request.id` instead.
-define(RPC_JSONRPC_REQUEST_ID, 'rpc.jsonrpc.request_id').

%% @deprecated Replaced by `jsonrpc.protocol.version`.
%% Deprecated, use `jsonrpc.protocol.version` instead.
-define(RPC_JSONRPC_VERSION, 'rpc.jsonrpc.version').

%% @deprecated Deprecated, no replacement at this time.
%% Compressed size of the message in bytes.
-define(RPC_MESSAGE_COMPRESSED_SIZE, 'rpc.message.compressed_size').

%% @deprecated Deprecated, no replacement at this time.
%% MUST be calculated as two different counters starting from `1` one for sent messages and one for received message.
-define(RPC_MESSAGE_ID, 'rpc.message.id').

%% @deprecated Deprecated, no replacement at this time.
%% Whether this is a received or sent message.
-define(RPC_MESSAGE_TYPE, 'rpc.message.type').

-define(RPC_MESSAGE_TYPE_VALUES_SENT, 'SENT').

-define(RPC_MESSAGE_TYPE_VALUES_RECEIVED, 'RECEIVED').


%% @deprecated Deprecated, no replacement at this time.
%% Uncompressed size of the message in bytes.
-define(RPC_MESSAGE_UNCOMPRESSED_SIZE, 'rpc.message.uncompressed_size').


%% The fully-qualified logical name of the method from the RPC interface perspective.
-define(RPC_METHOD, 'rpc.method').


%% The original name of the method used by the client.
%%  
-define(RPC_METHOD_ORIGINAL, 'rpc.method_original').


%% RPC request metadata, `<key>` being the normalized RPC metadata key (lowercase), the value being the metadata values.
%%  
-define(RPC_REQUEST_METADATA, 'rpc.request.metadata').


%% RPC response metadata, `<key>` being the normalized RPC metadata key (lowercase), the value being the metadata values.
%%  
-define(RPC_RESPONSE_METADATA, 'rpc.response.metadata').


%% Status code of the RPC returned by the RPC server or generated by the client
-define(RPC_RESPONSE_STATUS_CODE, 'rpc.response.status_code').

%% @deprecated Value should be included in `rpc.method` which is expected to be a fully-qualified name.
%% Deprecated, use fully-qualified `rpc.method` instead.
-define(RPC_SERVICE, 'rpc.service').

%% @deprecated Replaced by `rpc.system.name`.
%% Deprecated, use `rpc.system.name` attribute instead.
-define(RPC_SYSTEM, 'rpc.system').

-define(RPC_SYSTEM_VALUES_GRPC, 'grpc').

-define(RPC_SYSTEM_VALUES_JAVA_RMI, 'java_rmi').

-define(RPC_SYSTEM_VALUES_DOTNET_WCF, 'dotnet_wcf').

-define(RPC_SYSTEM_VALUES_APACHE_DUBBO, 'apache_dubbo').

-define(RPC_SYSTEM_VALUES_CONNECT_RPC, 'connect_rpc').

-define(RPC_SYSTEM_VALUES_ONC_RPC, 'onc_rpc').

-define(RPC_SYSTEM_VALUES_JSONRPC, 'jsonrpc').



%% The Remote Procedure Call (RPC) system.
-define(RPC_SYSTEM_NAME, 'rpc.system.name').

-define(RPC_SYSTEM_NAME_VALUES_GRPC, 'grpc').

-define(RPC_SYSTEM_NAME_VALUES_DUBBO, 'dubbo').

-define(RPC_SYSTEM_NAME_VALUES_CONNECTRPC, 'connectrpc').

-define(RPC_SYSTEM_NAME_VALUES_JSONRPC, 'jsonrpc').

