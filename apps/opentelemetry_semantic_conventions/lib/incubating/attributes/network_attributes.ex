defmodule OpenTelemetry.SemConv.Incubating.NetworkAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Network attributes.
  """
  defdelegate network_local_address(), to: OpenTelemetry.SemConv.NetworkAttributes

  defdelegate network_local_port(), to: OpenTelemetry.SemConv.NetworkAttributes

  defdelegate network_peer_address(), to: OpenTelemetry.SemConv.NetworkAttributes

  defdelegate network_peer_port(), to: OpenTelemetry.SemConv.NetworkAttributes

  defdelegate network_protocol_name(), to: OpenTelemetry.SemConv.NetworkAttributes

  defdelegate network_protocol_version(), to: OpenTelemetry.SemConv.NetworkAttributes

  defdelegate network_transport(), to: OpenTelemetry.SemConv.NetworkAttributes

  defdelegate network_transport_values(), to: OpenTelemetry.SemConv.NetworkAttributes

  defdelegate network_type(), to: OpenTelemetry.SemConv.NetworkAttributes

  defdelegate network_type_values(), to: OpenTelemetry.SemConv.NetworkAttributes

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
  * `:inet` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - IPv4 address
  * `:inet6` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - IPv6 address
  * `:unix` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Unix domain socket path
  """
  @type net_sock_family_values() :: %{
          :inet => :inet,
          :inet6 => :inet6,
          :unix => :unix
        }
  @deprecated """
  Split to `network.transport` and `network.type`.
  """
  @spec net_sock_family :: :"net.sock.family"
  def net_sock_family do
    :"net.sock.family"
  end

  @spec net_sock_family_values() :: net_sock_family_values()
  def net_sock_family_values() do
    %{
      :inet => :inet,
      :inet6 => :inet6,
      :unix => :unix
    }
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
  * `:ip_tcp` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:ip_udp` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:pipe` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Named or anonymous pipe.
  * `:inproc` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - In-process communication.
  * `:other` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Something else (non IP-based).
  """
  @type net_transport_values() :: %{
          :ip_tcp => :ip_tcp,
          :ip_udp => :ip_udp,
          :pipe => :pipe,
          :inproc => :inproc,
          :other => :other
        }
  @deprecated """
  Replaced by `network.transport`.
  """
  @spec net_transport :: :"net.transport"
  def net_transport do
    :"net.transport"
  end

  @spec net_transport_values() :: net_transport_values()
  def net_transport_values() do
    %{
      :ip_tcp => :ip_tcp,
      :ip_udp => :ip_udp,
      :pipe => :pipe,
      :inproc => :inproc,
      :other => :other
    }
  end

  @doc """
  The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  DE
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_carrier_icc()
      :"network.carrier.icc"

  ### Erlang

  ```erlang
  ?NETWORK_CARRIER_ICC.
  'network.carrier.icc'
  ```

  <!-- tabs-close -->
  """
  @spec network_carrier_icc :: :"network.carrier.icc"
  def network_carrier_icc do
    :"network.carrier.icc"
  end

  @doc """
  The mobile carrier country code.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  310
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_carrier_mcc()
      :"network.carrier.mcc"

  ### Erlang

  ```erlang
  ?NETWORK_CARRIER_MCC.
  'network.carrier.mcc'
  ```

  <!-- tabs-close -->
  """
  @spec network_carrier_mcc :: :"network.carrier.mcc"
  def network_carrier_mcc do
    :"network.carrier.mcc"
  end

  @doc """
  The mobile carrier network code.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  001
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_carrier_mnc()
      :"network.carrier.mnc"

  ### Erlang

  ```erlang
  ?NETWORK_CARRIER_MNC.
  'network.carrier.mnc'
  ```

  <!-- tabs-close -->
  """
  @spec network_carrier_mnc :: :"network.carrier.mnc"
  def network_carrier_mnc do
    :"network.carrier.mnc"
  end

  @doc """
  The name of the mobile carrier.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  sprint
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_carrier_name()
      :"network.carrier.name"

  ### Erlang

  ```erlang
  ?NETWORK_CARRIER_NAME.
  'network.carrier.name'
  ```

  <!-- tabs-close -->
  """
  @spec network_carrier_name :: :"network.carrier.name"
  def network_carrier_name do
    :"network.carrier.name"
  end

  @typedoc """
  This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection.

  ### Enum Values
  * `:gprs` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - GPRS
  * `:edge` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - EDGE
  * `:umts` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - UMTS
  * `:cdma` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - CDMA
  * `:evdo_0` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - EVDO Rel. 0
  * `:evdo_a` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - EVDO Rev. A
  * `:cdma2000_1xrtt` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - CDMA2000 1XRTT
  * `:hsdpa` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - HSDPA
  * `:hsupa` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - HSUPA
  * `:hspa` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - HSPA
  * `:iden` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - IDEN
  * `:evdo_b` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - EVDO Rev. B
  * `:lte` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - LTE
  * `:ehrpd` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - EHRPD
  * `:hspap` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - HSPAP
  * `:gsm` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - GSM
  * `:td_scdma` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - TD-SCDMA
  * `:iwlan` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - IWLAN
  * `:nr` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 5G NR (New Radio)
  * `:nrnsa` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 5G NRNSA (New Radio Non-Standalone)
  * `:lte_ca` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - LTE CA
  """
  @type network_connection_subtype_values() :: %{
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

  ### Examples

  ```
  LTE
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_connection_subtype()
      :"network.connection.subtype"

      iex> OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_connection_subtype_values().gprs
      :gprs

      iex> %{OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_connection_subtype() => OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_connection_subtype_values().gprs}
      %{:"network.connection.subtype" => :gprs}

  ### Erlang

  ```erlang
  ?NETWORK_CONNECTION_SUBTYPE.
  'network.connection.subtype'

  ?NETWORK_CONNECTION_SUBTYPE_VALUES_GPRS.
  'gprs'

  \#{?NETWORK_CONNECTION_SUBTYPE => ?NETWORK_CONNECTION_SUBTYPE_VALUES_GPRS}.
  \#{'network.connection.subtype' => 'gprs'}
  ```

  <!-- tabs-close -->
  """
  @spec network_connection_subtype :: :"network.connection.subtype"
  def network_connection_subtype do
    :"network.connection.subtype"
  end

  @spec network_connection_subtype_values() :: network_connection_subtype_values()
  def network_connection_subtype_values() do
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

  @typedoc """
  The internet connection type.

  ### Enum Values
  * `:wifi` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:wired` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:cell` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:unavailable` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:unknown` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type network_connection_type_values() :: %{
          :wifi => :wifi,
          :wired => :wired,
          :cell => :cell,
          :unavailable => :unavailable,
          :unknown => :unknown
        }
  @doc """
  The internet connection type.

  ### Examples

  ```
  wifi
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_connection_type()
      :"network.connection.type"

      iex> OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_connection_type_values().wifi
      :wifi

      iex> %{OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_connection_type() => OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_connection_type_values().wifi}
      %{:"network.connection.type" => :wifi}

  ### Erlang

  ```erlang
  ?NETWORK_CONNECTION_TYPE.
  'network.connection.type'

  ?NETWORK_CONNECTION_TYPE_VALUES_WIFI.
  'wifi'

  \#{?NETWORK_CONNECTION_TYPE => ?NETWORK_CONNECTION_TYPE_VALUES_WIFI}.
  \#{'network.connection.type' => 'wifi'}
  ```

  <!-- tabs-close -->
  """
  @spec network_connection_type :: :"network.connection.type"
  def network_connection_type do
    :"network.connection.type"
  end

  @spec network_connection_type_values() :: network_connection_type_values()
  def network_connection_type_values() do
    %{
      :wifi => :wifi,
      :wired => :wired,
      :cell => :cell,
      :unavailable => :unavailable,
      :unknown => :unknown
    }
  end

  @typedoc """
  The network IO operation direction.

  ### Enum Values
  * `:transmit` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:receive` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type network_io_direction_values() :: %{
          :transmit => :transmit,
          :receive => :receive
        }
  @doc """
  The network IO operation direction.

  ### Examples

  ```
  ["transmit"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_io_direction()
      :"network.io.direction"

      iex> OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_io_direction_values().transmit
      :transmit

      iex> %{OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_io_direction() => OpenTelemetry.SemConv.Incubating.NetworkAttributes.network_io_direction_values().transmit}
      %{:"network.io.direction" => :transmit}

  ### Erlang

  ```erlang
  ?NETWORK_IO_DIRECTION.
  'network.io.direction'

  ?NETWORK_IO_DIRECTION_VALUES_TRANSMIT.
  'transmit'

  \#{?NETWORK_IO_DIRECTION => ?NETWORK_IO_DIRECTION_VALUES_TRANSMIT}.
  \#{'network.io.direction' => 'transmit'}
  ```

  <!-- tabs-close -->
  """
  @spec network_io_direction :: :"network.io.direction"
  def network_io_direction do
    :"network.io.direction"
  end

  @spec network_io_direction_values() :: network_io_direction_values()
  def network_io_direction_values() do
    %{
      :transmit => :transmit,
      :receive => :receive
    }
  end
end
