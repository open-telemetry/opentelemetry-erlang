%% The schema url for telemetry resources
-define(METRIC_SCHEMA_URL, <<"https://opentelemetry.io/schemas/1.25.0">>).

%% The CPU state for this data point. A container SHOULD be characterized _either_ by data points with no `state` labels, _or only_ data points with `state` labels
-define(CONTAINER_CPU_STATE, 'container.cpu.state').

%% The disk IO operation direction
-define(DISK_IO_DIRECTION, 'disk.io.direction').

%% The device identifier
-define(SYSTEM_DEVICE, 'system.device').

%% The network IO operation direction
-define(NETWORK_IO_DIRECTION, 'network.io.direction').

%% The name of the connection pool; unique within the instrumented application. In case the connection pool implementation doesn't provide a name, instrumentation should use a combination of `server.address` and `server.port` attributes formatted as `server.address:server.port`
-define(POOL_NAME, 'pool.name').

%% The state of a connection in the pool
-define(STATE, 'state').

%% The name being queried
%% If the name field contains non-printable characters (below 32 or above 126), those characters should be represented as escaped base 10 integers (\DDD). Back slashes and quotes should be escaped. Tabs, carriage returns, and line feeds should be converted to \t, \r, and \n respectively
-define(DNS_QUESTION_NAME, 'dns.question.name').

%% Describes the error the DNS lookup failed with
%% Instrumentations SHOULD use error code such as one of errors reported by `getaddrinfo`([Linux or other POSIX systems](https://man7.org/linux/man-pages/man3/getaddrinfo.3.html) / [Windows](https://learn.microsoft.com/windows/win32/api/ws2tcpip/nf-ws2tcpip-getaddrinfo)) or one reported by the runtime or client library. If error code is not available, the full name of exception type SHOULD be used
-define(ERROR_TYPE, 'error.type').

%% Match result - success or failure
-define(ASPNETCORE_ROUTING_MATCH_STATUS, 'aspnetcore.routing.match_status').

%% A value that indicates whether the matched route is a fallback route
-define(ASPNETCORE_ROUTING_IS_FALLBACK, 'aspnetcore.routing.is_fallback').

%% The matched route, that is, the path template in the format used by the respective server framework
%% MUST NOT be populated when this is not supported by the HTTP server framework as the route attribute should have low-cardinality and the URI path can NOT substitute it.
%% SHOULD include the [application root](/docs/http/http-spans.md#http-server-definitions) if there is one
-define(HTTP_ROUTE, 'http.route').

%% ASP.NET Core exception middleware handling result
-define(ASPNETCORE_DIAGNOSTICS_EXCEPTION_RESULT, 'aspnetcore.diagnostics.exception.result').

%% Full type name of the [`IExceptionHandler`](https://learn.microsoft.com/dotnet/api/microsoft.aspnetcore.diagnostics.iexceptionhandler) implementation that handled the exception
-define(ASPNETCORE_DIAGNOSTICS_HANDLER_TYPE, 'aspnetcore.diagnostics.handler.type').

%% Rate limiting policy name
-define(ASPNETCORE_RATE_LIMITING_POLICY, 'aspnetcore.rate_limiting.policy').

%% Rate-limiting result, shows whether the lease was acquired or contains a rejection reason
-define(ASPNETCORE_RATE_LIMITING_RESULT, 'aspnetcore.rate_limiting.result').

%% [OSI transport layer](https://osi-model.com/transport-layer/) or [inter-process communication method](https://wikipedia.org/wiki/Inter-process_communication)
%% The value SHOULD be normalized to lowercase.
%% 
%% Consider always setting the transport when setting a port number, since
%% a port number is ambiguous without knowing the transport. For example
%% different processes could be listening on TCP port 12345 and UDP port 12345
-define(NETWORK_TRANSPORT, 'network.transport').

%% [OSI network layer](https://osi-model.com/network-layer/) or non-OSI equivalent
%% The value SHOULD be normalized to lowercase
-define(NETWORK_TYPE, 'network.type').

%% Server domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name
%% When observed from the client side, and when communicating through an intermediary, `server.address` SHOULD represent the server address behind any intermediaries, for example proxies, if it's available
-define(SERVER_ADDRESS, 'server.address').

%% Server port number
%% When observed from the client side, and when communicating through an intermediary, `server.port` SHOULD represent the server port behind any intermediaries, for example proxies, if it's available
-define(SERVER_PORT, 'server.port').

%% [OSI application layer](https://osi-model.com/application-layer/) or non-OSI equivalent
%% The value SHOULD be normalized to lowercase
-define(NETWORK_PROTOCOL_NAME, 'network.protocol.name').

%% The actual version of the protocol used for network communication
%% If protocol version is subject to negotiation (for example using [ALPN](https://www.rfc-editor.org/rfc/rfc7301.html)), this attribute SHOULD be set to the negotiated version. If the actual protocol version is not known, this attribute SHOULD NOT be set
-define(NETWORK_PROTOCOL_VERSION, 'network.protocol.version').

%% Numeric part of the version parsed from the original string of the negotiated [SSL/TLS protocol version](https://www.openssl.org/docs/man1.1.1/man3/SSL_get_version.html#RETURN-VALUES)
-define(TLS_PROTOCOL_VERSION, 'tls.protocol.version').

%% SignalR HTTP connection closure status
-define(SIGNALR_CONNECTION_STATUS, 'signalr.connection.status').

%% [SignalR transport type](https://github.com/dotnet/aspnetcore/blob/main/src/SignalR/docs/specs/TransportProtocols.md)
-define(SIGNALR_TRANSPORT, 'signalr.transport').

%% Type of the trigger which caused this function invocation
-define(FAAS_TRIGGER, 'faas.trigger').

%% HTTP request method
%% HTTP request method value SHOULD be "known" to the instrumentation.
%% By default, this convention defines "known" methods as the ones listed in [RFC9110](https://www.rfc-editor.org/rfc/rfc9110.html#name-methods)
%% and the PATCH method defined in [RFC5789](https://www.rfc-editor.org/rfc/rfc5789.html).
%% 
%% If the HTTP request method is not known to instrumentation, it MUST set the `http.request.method` attribute to `_OTHER`.
%% 
%% If the HTTP instrumentation could end up converting valid HTTP request methods to `_OTHER`, then it MUST provide a way to override
%% the list of known HTTP methods. If this override is done via environment variable, then the environment variable MUST be named
%% OTEL_INSTRUMENTATION_HTTP_KNOWN_METHODS and support a comma-separated list of case-sensitive known HTTP methods
%% (this list MUST be a full override of the default known method, it is not a list of known methods in addition to the defaults).
%% 
%% HTTP method names are case-sensitive and `http.request.method` attribute value MUST match a known HTTP method name exactly.
%% Instrumentations for specific web frameworks that consider HTTP methods to be case insensitive, SHOULD populate a canonical equivalent.
%% Tracing instrumentations that do so, MUST also set `http.request.method_original` to the original value
-define(HTTP_REQUEST_METHOD, 'http.request.method').

%% The [URI scheme](https://www.rfc-editor.org/rfc/rfc3986#section-3.1) component identifying the used protocol
%% The scheme of the original client request, if known (e.g. from [Forwarded#proto](https://developer.mozilla.org/docs/Web/HTTP/Headers/Forwarded#proto), [X-Forwarded-Proto](https://developer.mozilla.org/docs/Web/HTTP/Headers/X-Forwarded-Proto), or a similar header). Otherwise, the scheme of the immediate peer request
-define(URL_SCHEME, 'url.scheme').

%% [HTTP response status code](https://tools.ietf.org/html/rfc7231#section-6)
-define(HTTP_RESPONSE_STATUS_CODE, 'http.response.status_code').

%% State of the HTTP connection in the HTTP connection pool
-define(HTTP_CONNECTION_STATE, 'http.connection.state').

%% Peer address of the network connection - IP address or Unix domain socket name
-define(NETWORK_PEER_ADDRESS, 'network.peer.address').

%% Name of the memory pool
%% Pool names are generally obtained via [MemoryPoolMXBean#getName()](https://docs.oracle.com/en/java/javase/11/docs/api/java.management/java/lang/management/MemoryPoolMXBean.html#getName())
-define(JVM_MEMORY_POOL_NAME, 'jvm.memory.pool.name').

%% The type of memory
-define(JVM_MEMORY_TYPE, 'jvm.memory.type').

%% Name of the buffer pool
%% Pool names are generally obtained via [BufferPoolMXBean#getName()](https://docs.oracle.com/en/java/javase/11/docs/api/java.management/java/lang/management/BufferPoolMXBean.html#getName())
-define(JVM_BUFFER_POOL_NAME, 'jvm.buffer.pool.name').

%% Name of the garbage collector action
%% Garbage collector action is generally obtained via [GarbageCollectionNotificationInfo#getGcAction()](https://docs.oracle.com/en/java/javase/11/docs/api/jdk.management/com/sun/management/GarbageCollectionNotificationInfo.html#getGcAction())
-define(JVM_GC_ACTION, 'jvm.gc.action').

%% Name of the garbage collector
%% Garbage collector name is generally obtained via [GarbageCollectionNotificationInfo#getGcName()](https://docs.oracle.com/en/java/javase/11/docs/api/jdk.management/com/sun/management/GarbageCollectionNotificationInfo.html#getGcName())
-define(JVM_GC_NAME, 'jvm.gc.name').

%% Whether the thread is daemon or not
-define(JVM_THREAD_DAEMON, 'jvm.thread.daemon').

%% State of the thread
-define(JVM_THREAD_STATE, 'jvm.thread.state').

%% An identifier for the messaging system being used. See below for a list of well-known identifiers
-define(MESSAGING_SYSTEM, 'messaging.system').

%% The message destination name
%% Destination name SHOULD uniquely identify a specific queue, topic or other entity within the broker. If
%% the broker doesn't have such notion, the destination name SHOULD uniquely identify the broker
-define(MESSAGING_DESTINATION_NAME, 'messaging.destination.name').

%% Low cardinality representation of the messaging destination name
%% Destination names could be constructed from templates. An example would be a destination name involving a user name or product id. Although the destination name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation
-define(MESSAGING_DESTINATION_TEMPLATE, 'messaging.destination.template').

%% The CPU state for this data point. A process SHOULD be characterized _either_ by data points with no `state` labels, _or only_ data points with `state` labels
-define(PROCESS_CPU_STATE, 'process.cpu.state').

%% Specifies whether the context switches for this data point were voluntary or involuntary
-define(PROCESS_CONTEXT_SWITCH_TYPE, 'process.context_switch_type').

%% The type of page fault for this data point. Type `major` is for major/hard page faults, and `minor` is for minor/soft page faults
-define(PROCESS_PAGING_FAULT_TYPE, 'process.paging.fault_type').

%% The logical CPU number [0..n-1]
-define(SYSTEM_CPU_LOGICAL_NUMBER, 'system.cpu.logical_number').

%% The CPU state for this data point. A system's CPU SHOULD be characterized *either* by data points with no `state` labels, *or only* data points with `state` labels
-define(SYSTEM_CPU_STATE, 'system.cpu.state').

%% The memory state
-define(SYSTEM_MEMORY_STATE, 'system.memory.state').

%% The memory paging state
-define(SYSTEM_PAGING_STATE, 'system.paging.state').

%% The memory paging type
-define(SYSTEM_PAGING_TYPE, 'system.paging.type').

%% The paging access direction
-define(SYSTEM_PAGING_DIRECTION, 'system.paging.direction').

%% The filesystem mode
-define(SYSTEM_FILESYSTEM_MODE, 'system.filesystem.mode').

%% The filesystem mount path
-define(SYSTEM_FILESYSTEM_MOUNTPOINT, 'system.filesystem.mountpoint').

%% The filesystem state
-define(SYSTEM_FILESYSTEM_STATE, 'system.filesystem.state').

%% The filesystem type
-define(SYSTEM_FILESYSTEM_TYPE, 'system.filesystem.type').

%% A stateless protocol MUST NOT set this attribute
-define(SYSTEM_NETWORK_STATE, 'system.network.state').

%% The process state, e.g., [Linux Process State Codes](https://man7.org/linux/man-pages/man1/ps.1.html#PROCESS_STATE_CODES)
-define(SYSTEM_PROCESS_STATUS, 'system.process.status').
