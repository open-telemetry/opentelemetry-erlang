defmodule OpenTelemetry.SemanticConventions.NetworkAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Network attributes.
  """

  @deprecated """
  Replaced by `network.local.address`.
  """
  @spec net_host_ip :: :"net.host.ip"
  def net_host_ip do
    :"net.host.ip"
  end

  @deprecated """
  Replaced by `server.address`.
  """
  @spec net_host_name :: :"net.host.name"
  def net_host_name do
    :"net.host.name"
  end

  @deprecated """
  Replaced by `server.port`.
  """
  @spec net_host_port :: :"net.host.port"
  def net_host_port do
    :"net.host.port"
  end

  @deprecated """
  Replaced by `network.peer.address`.
  """
  @spec net_peer_ip :: :"net.peer.ip"
  def net_peer_ip do
    :"net.peer.ip"
  end

  @deprecated """
  Replaced by `server.address` on client spans and `client.address` on server spans.
  """
  @spec net_peer_name :: :"net.peer.name"
  def net_peer_name do
    :"net.peer.name"
  end

  @deprecated """
  Replaced by `server.port` on client spans and `client.port` on server spans.
  """
  @spec net_peer_port :: :"net.peer.port"
  def net_peer_port do
    :"net.peer.port"
  end

  @deprecated """
  Replaced by `network.protocol.name`.
  """
  @spec net_protocol_name :: :"net.protocol.name"
  def net_protocol_name do
    :"net.protocol.name"
  end

  @deprecated """
  Replaced by `network.protocol.version`.
  """
  @spec net_protocol_version :: :"net.protocol.version"
  def net_protocol_version do
    :"net.protocol.version"
  end

  @typedoc """
  Deprecated, use `network.transport` and `network.type`.

  ### Enum Values
  * `:inet` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - IPv4 address
  * `:inet6` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - IPv6 address
  * `:unix` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Unix domain socket path
  """
  @type net_sock_family() :: %{
          :inet => :inet,
          :inet6 => :inet6,
          :unix => :unix
        }
  @deprecated """
  Split to `network.transport` and `network.type`.
  """
  @spec net_sock_family() :: net_sock_family()
  def net_sock_family() do
    %{
      :inet => :inet,
      :inet6 => :inet6,
      :unix => :unix
    }
  end

  @spec net_sock_family(atom() | String.t()) :: atom() | String.t()
  def net_sock_family(custom_value) do
    custom_value
  end

  @deprecated """
  Replaced by `network.local.address`.
  """
  @spec net_sock_host_addr :: :"net.sock.host.addr"
  def net_sock_host_addr do
    :"net.sock.host.addr"
  end

  @deprecated """
  Replaced by `network.local.port`.
  """
  @spec net_sock_host_port :: :"net.sock.host.port"
  def net_sock_host_port do
    :"net.sock.host.port"
  end

  @deprecated """
  Replaced by `network.peer.address`.
  """
  @spec net_sock_peer_addr :: :"net.sock.peer.addr"
  def net_sock_peer_addr do
    :"net.sock.peer.addr"
  end

  @deprecated """
  Removed.
  """
  @spec net_sock_peer_name :: :"net.sock.peer.name"
  def net_sock_peer_name do
    :"net.sock.peer.name"
  end

  @deprecated """
  Replaced by `network.peer.port`.
  """
  @spec net_sock_peer_port :: :"net.sock.peer.port"
  def net_sock_peer_port do
    :"net.sock.peer.port"
  end

  @typedoc """
  Deprecated, use `network.transport`.

  ### Enum Values
  * `:ip_tcp` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:ip_udp` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:pipe` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Named or anonymous pipe.
  * `:inproc` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - In-process communication.
  * `:other` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Something else (non IP-based).
  """
  @type net_transport() :: %{
          :ip_tcp => :ip_tcp,
          :ip_udp => :ip_udp,
          :pipe => :pipe,
          :inproc => :inproc,
          :other => :other
        }
  @deprecated """
  Replaced by `network.transport`.
  """
  @spec net_transport() :: net_transport()
  def net_transport() do
    %{
      :ip_tcp => :ip_tcp,
      :ip_udp => :ip_udp,
      :pipe => :pipe,
      :inproc => :inproc,
      :other => :other
    }
  end

  @spec net_transport(atom() | String.t()) :: atom() | String.t()
  def net_transport(custom_value) do
    custom_value
  end

  @doc """
  The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network.


  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_carrier_icc()
      :"network.carrier.icc"
  """
  @spec network_carrier_icc :: :"network.carrier.icc"
  def network_carrier_icc do
    :"network.carrier.icc"
  end

  @doc """
  The mobile carrier country code.


  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_carrier_mcc()
      :"network.carrier.mcc"
  """
  @spec network_carrier_mcc :: :"network.carrier.mcc"
  def network_carrier_mcc do
    :"network.carrier.mcc"
  end

  @doc """
  The mobile carrier network code.


  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_carrier_mnc()
      :"network.carrier.mnc"
  """
  @spec network_carrier_mnc :: :"network.carrier.mnc"
  def network_carrier_mnc do
    :"network.carrier.mnc"
  end

  @doc """
  The name of the mobile carrier.


  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_carrier_name()
      :"network.carrier.name"
  """
  @spec network_carrier_name :: :"network.carrier.name"
  def network_carrier_name do
    :"network.carrier.name"
  end

  @typedoc """
  This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection.

  ### Enum Values
  * `:gprs` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - GPRS
  * `:edge` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - EDGE
  * `:umts` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - UMTS
  * `:cdma` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - CDMA
  * `:evdo_0` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - EVDO Rel. 0
  * `:evdo_a` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - EVDO Rev. A
  * `:cdma2000_1xrtt` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - CDMA2000 1XRTT
  * `:hsdpa` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HSDPA
  * `:hsupa` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HSUPA
  * `:hspa` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HSPA
  * `:iden` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - IDEN
  * `:evdo_b` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - EVDO Rev. B
  * `:lte` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - LTE
  * `:ehrpd` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - EHRPD
  * `:hspap` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HSPAP
  * `:gsm` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - GSM
  * `:td_scdma` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - TD-SCDMA
  * `:iwlan` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - IWLAN
  * `:nr` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 5G NR (New Radio)
  * `:nrnsa` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 5G NRNSA (New Radio Non-Standalone)
  * `:lte_ca` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - LTE CA
  """
  @type network_connection_subtype() :: %{
          :gprs => :gprs,
          :edge => :edge,
          :umts => :umts,
          :cdma => :cdma,
          :evdo_0 => :evdo_0,
          :evdo_a => :evdo_a,
          :cdma2000_1xrtt => :cdma2000_1xrtt,
          :hsdpa => :hsdpa,
          :hsupa => :hsupa,
          :hspa => :hspa,
          :iden => :iden,
          :evdo_b => :evdo_b,
          :lte => :lte,
          :ehrpd => :ehrpd,
          :hspap => :hspap,
          :gsm => :gsm,
          :td_scdma => :td_scdma,
          :iwlan => :iwlan,
          :nr => :nr,
          :nrnsa => :nrnsa,
          :lte_ca => :lte_ca
        }
  @doc """
  This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection.


  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_connection_subtype().gprs
      :gprs
      
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_connection_subtype(:custom_value)
      :custom_value
  """
  @spec network_connection_subtype() :: network_connection_subtype()
  def network_connection_subtype() do
    %{
      :gprs => :gprs,
      :edge => :edge,
      :umts => :umts,
      :cdma => :cdma,
      :evdo_0 => :evdo_0,
      :evdo_a => :evdo_a,
      :cdma2000_1xrtt => :cdma2000_1xrtt,
      :hsdpa => :hsdpa,
      :hsupa => :hsupa,
      :hspa => :hspa,
      :iden => :iden,
      :evdo_b => :evdo_b,
      :lte => :lte,
      :ehrpd => :ehrpd,
      :hspap => :hspap,
      :gsm => :gsm,
      :td_scdma => :td_scdma,
      :iwlan => :iwlan,
      :nr => :nr,
      :nrnsa => :nrnsa,
      :lte_ca => :lte_ca
    }
  end

  @spec network_connection_subtype(atom() | String.t()) :: atom() | String.t()
  def network_connection_subtype(custom_value) do
    custom_value
  end

  @typedoc """
  The internet connection type.

  ### Enum Values
  * `:wifi` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:wired` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:cell` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:unavailable` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:unknown` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  """
  @type network_connection_type() :: %{
          :wifi => :wifi,
          :wired => :wired,
          :cell => :cell,
          :unavailable => :unavailable,
          :unknown => :unknown
        }
  @doc """
  The internet connection type.


  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_connection_type().wifi
      :wifi
      
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_connection_type(:custom_value)
      :custom_value
  """
  @spec network_connection_type() :: network_connection_type()
  def network_connection_type() do
    %{
      :wifi => :wifi,
      :wired => :wired,
      :cell => :cell,
      :unavailable => :unavailable,
      :unknown => :unknown
    }
  end

  @spec network_connection_type(atom() | String.t()) :: atom() | String.t()
  def network_connection_type(custom_value) do
    custom_value
  end

  @typedoc """
  The network IO operation direction.

  ### Enum Values
  * `:transmit` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:receive` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  """
  @type network_io_direction() :: %{
          :transmit => :transmit,
          :receive => :receive
        }
  @doc """
  The network IO operation direction.


  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_io_direction().transmit
      :transmit
      
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_io_direction(:custom_value)
      :custom_value
  """
  @spec network_io_direction() :: network_io_direction()
  def network_io_direction() do
    %{
      :transmit => :transmit,
      :receive => :receive
    }
  end

  @spec network_io_direction(atom() | String.t()) :: atom() | String.t()
  def network_io_direction(custom_value) do
    custom_value
  end

  @doc """
  Local address of the network connection - IP address or Unix domain socket name.


  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_local_address()
      :"network.local.address"
  """
  @spec network_local_address :: :"network.local.address"
  def network_local_address do
    :"network.local.address"
  end

  @doc """
  Local port number of the network connection.


  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_local_port()
      :"network.local.port"
  """
  @spec network_local_port :: :"network.local.port"
  def network_local_port do
    :"network.local.port"
  end

  @doc """
  Peer address of the network connection - IP address or Unix domain socket name.


  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_peer_address()
      :"network.peer.address"
  """
  @spec network_peer_address :: :"network.peer.address"
  def network_peer_address do
    :"network.peer.address"
  end

  @doc """
  Peer port number of the network connection.


  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_peer_port()
      :"network.peer.port"
  """
  @spec network_peer_port :: :"network.peer.port"
  def network_peer_port do
    :"network.peer.port"
  end

  @doc """
  [OSI application layer](https://osi-model.com/application-layer/) or non-OSI equivalent.
  ### Notes

  The value **SHOULD** be normalized to lowercase.

  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_protocol_name()
      :"network.protocol.name"
  """
  @spec network_protocol_name :: :"network.protocol.name"
  def network_protocol_name do
    :"network.protocol.name"
  end

  @doc """
  The actual version of the protocol used for network communication.
  ### Notes

  If protocol version is subject to negotiation (for example using [ALPN](https://www.rfc-editor.org/rfc/rfc7301.html)), this attribute **SHOULD** be set to the negotiated version. If the actual protocol version is not known, this attribute **SHOULD** **NOT** be set.


  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_protocol_version()
      :"network.protocol.version"
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
  """
  @type network_transport() :: %{
          :tcp => :tcp,
          :udp => :udp,
          :pipe => :pipe,
          :unix => :unix
        }
  @doc """
  [OSI transport layer](https://osi-model.com/transport-layer/) or [inter-process communication method](https://wikipedia.org/wiki/Inter-process_communication).

  ### Notes

  The value **SHOULD** be normalized to lowercase.

  Consider always setting the transport when setting a port number, since
  a port number is ambiguous without knowing the transport. For example
  different processes could be listening on TCP port 12345 and UDP port 12345.


  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_transport().tcp
      :tcp
      
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_transport(:custom_value)
      :custom_value
  """
  @spec network_transport() :: network_transport()
  def network_transport() do
    %{
      :tcp => :tcp,
      :udp => :udp,
      :pipe => :pipe,
      :unix => :unix
    }
  end

  @spec network_transport(atom() | String.t()) :: atom() | String.t()
  def network_transport(custom_value) do
    custom_value
  end

  @typedoc """
  [OSI network layer](https://osi-model.com/network-layer/) or non-OSI equivalent.

  ### Enum Values
  * `:ipv4` - IPv4
  * `:ipv6` - IPv6
  """
  @type network_type() :: %{
          :ipv4 => :ipv4,
          :ipv6 => :ipv6
        }
  @doc """
  [OSI network layer](https://osi-model.com/network-layer/) or non-OSI equivalent.
  ### Notes

  The value **SHOULD** be normalized to lowercase.

  ### Example
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_type().ipv4
      :ipv4
      
      iex> OpenTelemetry.SemanticConventions.NetworkAttributes.network_type(:custom_value)
      :custom_value
  """
  @spec network_type() :: network_type()
  def network_type() do
    %{
      :ipv4 => :ipv4,
      :ipv6 => :ipv6
    }
  end

  @spec network_type(atom() | String.t()) :: atom() | String.t()
  def network_type(custom_value) do
    custom_value
  end
end
