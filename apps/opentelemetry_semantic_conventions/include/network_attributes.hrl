

%% Local address of the network connection - IP address or Unix domain socket name.
-define(NETWORK_LOCAL_ADDRESS, 'network.local.address').


%% Local port number of the network connection.
-define(NETWORK_LOCAL_PORT, 'network.local.port').


%% Peer address of the network connection - IP address or Unix domain socket name.
-define(NETWORK_PEER_ADDRESS, 'network.peer.address').


%% Peer port number of the network connection.
-define(NETWORK_PEER_PORT, 'network.peer.port').


%% [OSI application layer](https://osi-model.com/application-layer/) or non-OSI equivalent.
-define(NETWORK_PROTOCOL_NAME, 'network.protocol.name').


%% The actual version of the protocol used for network communication.
-define(NETWORK_PROTOCOL_VERSION, 'network.protocol.version').


%% [OSI transport layer](https://osi-model.com/transport-layer/) or [inter-process communication method](https://wikipedia.org/wiki/Inter-process_communication).
%%  

-define('network_transport.tcp', 'tcp').

-define('network_transport.udp', 'udp').

-define('network_transport.pipe', 'pipe').

-define('network_transport.unix', 'unix').

-define(network_transport.(Custom), Custom).


%% [OSI network layer](https://osi-model.com/network-layer/) or non-OSI equivalent.

-define('network_type.ipv4', 'ipv4').

-define('network_type.ipv6', 'ipv6').

-define(network_type.(Custom), Custom).
