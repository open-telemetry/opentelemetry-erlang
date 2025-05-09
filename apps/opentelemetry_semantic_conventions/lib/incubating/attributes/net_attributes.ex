defmodule OpenTelemetry.SemConv.Incubating.NetAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Net attributes.
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
end
