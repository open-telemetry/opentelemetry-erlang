defmodule OpenTelemetry.SemConv.NetworkAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Network attributes.
  """

  @doc """
  Local address of the network connection - IP address or Unix domain socket name.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["10.1.2.80", "/tmp/my.sock"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.NetworkAttributes.network_local_address()
      :"network.local.address"

  ### Erlang

  ```erlang
  ?NETWORK_LOCAL_ADDRESS.
  'network.local.address'
  ```

  <!-- tabs-close -->
  """
  @spec network_local_address :: :"network.local.address"
  def network_local_address do
    :"network.local.address"
  end

  @doc """
  Local port number of the network connection.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [65123]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.NetworkAttributes.network_local_port()
      :"network.local.port"

  ### Erlang

  ```erlang
  ?NETWORK_LOCAL_PORT.
  'network.local.port'
  ```

  <!-- tabs-close -->
  """
  @spec network_local_port :: :"network.local.port"
  def network_local_port do
    :"network.local.port"
  end

  @doc """
  Peer address of the network connection - IP address or Unix domain socket name.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["10.1.2.80", "/tmp/my.sock"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.NetworkAttributes.network_peer_address()
      :"network.peer.address"

  ### Erlang

  ```erlang
  ?NETWORK_PEER_ADDRESS.
  'network.peer.address'
  ```

  <!-- tabs-close -->
  """
  @spec network_peer_address :: :"network.peer.address"
  def network_peer_address do
    :"network.peer.address"
  end

  @doc """
  Peer port number of the network connection.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [65123]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.NetworkAttributes.network_peer_port()
      :"network.peer.port"

  ### Erlang

  ```erlang
  ?NETWORK_PEER_PORT.
  'network.peer.port'
  ```

  <!-- tabs-close -->
  """
  @spec network_peer_port :: :"network.peer.port"
  def network_peer_port do
    :"network.peer.port"
  end

  @doc """
  [OSI application layer](https://osi-model.com/application-layer/) or non-OSI equivalent.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The value **SHOULD** be normalized to lowercase.
  ### Examples

  ```
  ["amqp", "http", "mqtt"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.NetworkAttributes.network_protocol_name()
      :"network.protocol.name"

  ### Erlang

  ```erlang
  ?NETWORK_PROTOCOL_NAME.
  'network.protocol.name'
  ```

  <!-- tabs-close -->
  """
  @spec network_protocol_name :: :"network.protocol.name"
  def network_protocol_name do
    :"network.protocol.name"
  end

  @doc """
  The actual version of the protocol used for network communication.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  If protocol version is subject to negotiation (for example using [ALPN](https://www.rfc-editor.org/rfc/rfc7301.html)), this attribute **SHOULD** be set to the negotiated version. If the actual protocol version is not known, this attribute **SHOULD** **NOT** be set.

  ### Examples

  ```
  ["1.1", "2"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.NetworkAttributes.network_protocol_version()
      :"network.protocol.version"

  ### Erlang

  ```erlang
  ?NETWORK_PROTOCOL_VERSION.
  'network.protocol.version'
  ```

  <!-- tabs-close -->
  """
  @spec network_protocol_version :: :"network.protocol.version"
  def network_protocol_version do
    :"network.protocol.version"
  end

  @typedoc """
  [OSI transport layer](https://osi-model.com/transport-layer/) or [inter-process communication method](https://wikipedia.org/wiki/Inter-process_communication).


  ### Enum Values
  * `:tcp` - TCP
  * `:udp` - UDP
  * `:pipe` - Named or anonymous pipe.
  * `:unix` - Unix domain socket
  * `:quic` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - QUIC
  """
  @type network_transport_values() :: %{
          :tcp => :tcp,
          :udp => :udp,
          :pipe => :pipe,
          :unix => :unix,
          :quic => :quic
        }
  @doc """
  [OSI transport layer](https://osi-model.com/transport-layer/) or [inter-process communication method](https://wikipedia.org/wiki/Inter-process_communication).


  ### Notes

  The value **SHOULD** be normalized to lowercase.

  Consider always setting the transport when setting a port number, since
  a port number is ambiguous without knowing the transport. For example
  different processes could be listening on TCP port 12345 and UDP port 12345.

  ### Examples

  ```
  ["tcp", "udp"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.NetworkAttributes.network_transport()
      :"network.transport"

      iex> OpenTelemetry.SemConv.NetworkAttributes.network_transport_values().tcp
      :tcp

      iex> %{OpenTelemetry.SemConv.NetworkAttributes.network_transport() => OpenTelemetry.SemConv.NetworkAttributes.network_transport_values().tcp}
      %{:"network.transport" => :tcp}

  ### Erlang

  ```erlang
  ?NETWORK_TRANSPORT.
  'network.transport'

  ?NETWORK_TRANSPORT_VALUES_TCP.
  'tcp'

  \#{?NETWORK_TRANSPORT => ?NETWORK_TRANSPORT_VALUES_TCP}.
  \#{'network.transport' => 'tcp'}
  ```

  <!-- tabs-close -->
  """
  @spec network_transport :: :"network.transport"
  def network_transport do
    :"network.transport"
  end

  @spec network_transport_values() :: network_transport_values()
  def network_transport_values() do
    %{
      :tcp => :tcp,
      :udp => :udp,
      :pipe => :pipe,
      :unix => :unix,
      :quic => :quic
    }
  end

  @typedoc """
  [OSI network layer](https://osi-model.com/network-layer/) or non-OSI equivalent.

  ### Enum Values
  * `:ipv4` - IPv4
  * `:ipv6` - IPv6
  """
  @type network_type_values() :: %{
          :ipv4 => :ipv4,
          :ipv6 => :ipv6
        }
  @doc """
  [OSI network layer](https://osi-model.com/network-layer/) or non-OSI equivalent.

  ### Notes

  The value **SHOULD** be normalized to lowercase.
  ### Examples

  ```
  ["ipv4", "ipv6"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.NetworkAttributes.network_type()
      :"network.type"

      iex> OpenTelemetry.SemConv.NetworkAttributes.network_type_values().ipv4
      :ipv4

      iex> %{OpenTelemetry.SemConv.NetworkAttributes.network_type() => OpenTelemetry.SemConv.NetworkAttributes.network_type_values().ipv4}
      %{:"network.type" => :ipv4}

  ### Erlang

  ```erlang
  ?NETWORK_TYPE.
  'network.type'

  ?NETWORK_TYPE_VALUES_IPV4.
  'ipv4'

  \#{?NETWORK_TYPE => ?NETWORK_TYPE_VALUES_IPV4}.
  \#{'network.type' => 'ipv4'}
  ```

  <!-- tabs-close -->
  """
  @spec network_type :: :"network.type"
  def network_type do
    :"network.type"
  end

  @spec network_type_values() :: network_type_values()
  def network_type_values() do
    %{
      :ipv4 => :ipv4,
      :ipv6 => :ipv6
    }
  end
end
