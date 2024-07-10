
%% @deprecated Replaced by `rpc.message.compressed_size`.
%% Deprecated, use `rpc.message.compressed_size` instead.
-define(MESSAGE_COMPRESSEDSIZE, 'message.compressed_size').

%% @deprecated Replaced by `rpc.message.id`.
%% Deprecated, use `rpc.message.id` instead.
-define(MESSAGE_ID, 'message.id').

%% @deprecated Replaced by `rpc.message.type`.
%% Deprecated, use `rpc.message.type` instead.

-define('message_type.sent', 'SENT').

-define('message_type.received', 'RECEIVED').

-define(message_type.(Custom), Custom).

%% @deprecated Replaced by `rpc.message.uncompressed_size`.
%% Deprecated, use `rpc.message.uncompressed_size` instead.
-define(MESSAGE_UNCOMPRESSEDSIZE, 'message.uncompressed_size').


%% The [error codes](https://connect.build/docs/protocol/#error-codes) of the Connect request. Error codes are always string values.

-define('rpc_connectrpc_errorcode.cancelled', 'cancelled').

-define('rpc_connectrpc_errorcode.unknown', 'unknown').

-define('rpc_connectrpc_errorcode.invalid_argument', 'invalid_argument').

-define('rpc_connectrpc_errorcode.deadline_exceeded', 'deadline_exceeded').

-define('rpc_connectrpc_errorcode.not_found', 'not_found').

-define('rpc_connectrpc_errorcode.already_exists', 'already_exists').

-define('rpc_connectrpc_errorcode.permission_denied', 'permission_denied').

-define('rpc_connectrpc_errorcode.resource_exhausted', 'resource_exhausted').

-define('rpc_connectrpc_errorcode.failed_precondition', 'failed_precondition').

-define('rpc_connectrpc_errorcode.aborted', 'aborted').

-define('rpc_connectrpc_errorcode.out_of_range', 'out_of_range').

-define('rpc_connectrpc_errorcode.unimplemented', 'unimplemented').

-define('rpc_connectrpc_errorcode.internal', 'internal').

-define('rpc_connectrpc_errorcode.unavailable', 'unavailable').

-define('rpc_connectrpc_errorcode.data_loss', 'data_loss').

-define('rpc_connectrpc_errorcode.unauthenticated', 'unauthenticated').

-define(rpc_connectrpc_errorcode.(Custom), Custom).


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

-define('rpc_grpc_statuscode.ok', '0').

-define('rpc_grpc_statuscode.cancelled', '1').

-define('rpc_grpc_statuscode.unknown', '2').

-define('rpc_grpc_statuscode.invalid_argument', '3').

-define('rpc_grpc_statuscode.deadline_exceeded', '4').

-define('rpc_grpc_statuscode.not_found', '5').

-define('rpc_grpc_statuscode.already_exists', '6').

-define('rpc_grpc_statuscode.permission_denied', '7').

-define('rpc_grpc_statuscode.resource_exhausted', '8').

-define('rpc_grpc_statuscode.failed_precondition', '9').

-define('rpc_grpc_statuscode.aborted', '10').

-define('rpc_grpc_statuscode.out_of_range', '11').

-define('rpc_grpc_statuscode.unimplemented', '12').

-define('rpc_grpc_statuscode.internal', '13').

-define('rpc_grpc_statuscode.unavailable', '14').

-define('rpc_grpc_statuscode.data_loss', '15').

-define('rpc_grpc_statuscode.unauthenticated', '16').

-define(rpc_grpc_statuscode.(Custom), Custom).


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

-define('rpc_message_type.sent', 'SENT').

-define('rpc_message_type.received', 'RECEIVED').

-define(rpc_message_type.(Custom), Custom).


%% Uncompressed size of the message in bytes.
-define(RPC_MESSAGE_UNCOMPRESSEDSIZE, 'rpc.message.uncompressed_size').


%% The name of the (logical) method being called, must be equal to the $method part in the span name.
-define(RPC_METHOD, 'rpc.method').


%% The full (logical) name of the service being called, including its package name, if applicable.
-define(RPC_SERVICE, 'rpc.service').


%% A string identifying the remoting system. See below for a list of well-known identifiers.

-define('rpc_system.grpc', 'grpc').

-define('rpc_system.java_rmi', 'java_rmi').

-define('rpc_system.dotnet_wcf', 'dotnet_wcf').

-define('rpc_system.apache_dubbo', 'apache_dubbo').

-define('rpc_system.connect_rpc', 'connect_rpc').

-define(rpc_system.(Custom), Custom).
