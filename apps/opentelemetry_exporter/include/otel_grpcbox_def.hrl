%% Local copy of grpcbox's `#grpcbox_def{}' service-definition record.
%%
%% The generated gRPC client stub modules (opentelemetry_*_service.erl) only
%% need this one record from grpcbox at compile time. Defining it here lets
%% those modules compile when grpcbox is not installed, so grpcbox can be an
%% optional dependency for users that only export over http_protobuf.
-record(grpcbox_def, {service :: atom(),
                      message_type = <<>> :: binary(),
                      marshal_fun :: fun((map()) -> binary()),
                      unmarshal_fun :: fun((binary()) -> map())}).
