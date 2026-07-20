defmodule OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Nfs metrics.
  """
  @doc """
  Reports the count of kernel NFS client TCP segments and UDP datagrams handled.

  Instrument: `counter`
  Unit: `{record}`
  ### Notes

  Linux: this metric is taken from the Linux kernel's svc_stat.netudpcnt and svc_stat.nettcpcnt


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_client_net_count()
      :"nfs.client.net.count"

  ### Erlang

  ```erlang
  ?NFS_CLIENT_NET_COUNT.
  'nfs.client.net.count'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_client_net_count :: :"nfs.client.net.count"
  def nfs_client_net_count do
    :"nfs.client.net.count"
  end

  @doc """
  Reports the count of kernel NFS client TCP connections accepted.

  Instrument: `counter`
  Unit: `{connection}`
  ### Notes

  Linux: this metric is taken from the Linux kernel's svc_stat.nettcpconn


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_client_net_tcp_connection_accepted()
      :"nfs.client.net.tcp.connection.accepted"

  ### Erlang

  ```erlang
  ?NFS_CLIENT_NET_TCP_CONNECTION_ACCEPTED.
  'nfs.client.net.tcp.connection.accepted'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_client_net_tcp_connection_accepted :: :"nfs.client.net.tcp.connection.accepted"
  def nfs_client_net_tcp_connection_accepted do
    :"nfs.client.net.tcp.connection.accepted"
  end

  @doc """
  Reports the count of kernel NFSv4+ client operations.

  Instrument: `counter`
  Unit: `{operation}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_client_operation_count()
      :"nfs.client.operation.count"

  ### Erlang

  ```erlang
  ?NFS_CLIENT_OPERATION_COUNT.
  'nfs.client.operation.count'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_client_operation_count :: :"nfs.client.operation.count"
  def nfs_client_operation_count do
    :"nfs.client.operation.count"
  end

  @doc """
  Reports the count of kernel NFS client procedures.

  Instrument: `counter`
  Unit: `{procedure}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_client_procedure_count()
      :"nfs.client.procedure.count"

  ### Erlang

  ```erlang
  ?NFS_CLIENT_PROCEDURE_COUNT.
  'nfs.client.procedure.count'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_client_procedure_count :: :"nfs.client.procedure.count"
  def nfs_client_procedure_count do
    :"nfs.client.procedure.count"
  end

  @doc """
  Reports the count of kernel NFS client RPC authentication refreshes.

  Instrument: `counter`
  Unit: `{authrefresh}`
  ### Notes

  Linux: this metric is taken from the Linux kernel's svc_stat.rpcauthrefresh


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_client_rpc_authrefresh_count()
      :"nfs.client.rpc.authrefresh.count"

  ### Erlang

  ```erlang
  ?NFS_CLIENT_RPC_AUTHREFRESH_COUNT.
  'nfs.client.rpc.authrefresh.count'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_client_rpc_authrefresh_count :: :"nfs.client.rpc.authrefresh.count"
  def nfs_client_rpc_authrefresh_count do
    :"nfs.client.rpc.authrefresh.count"
  end

  @doc """
  Reports the count of kernel NFS client RPCs sent, regardless of whether they're accepted/rejected by the server.

  Instrument: `counter`
  Unit: `{request}`
  ### Notes

  Linux: this metric is taken from the Linux kernel's svc_stat.rpccnt


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_client_rpc_count()
      :"nfs.client.rpc.count"

  ### Erlang

  ```erlang
  ?NFS_CLIENT_RPC_COUNT.
  'nfs.client.rpc.count'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_client_rpc_count :: :"nfs.client.rpc.count"
  def nfs_client_rpc_count do
    :"nfs.client.rpc.count"
  end

  @doc """
  Reports the count of kernel NFS client RPC retransmits.

  Instrument: `counter`
  Unit: `{retransmit}`
  ### Notes

  Linux: this metric is taken from the Linux kernel's svc_stat.rpcretrans


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_client_rpc_retransmit_count()
      :"nfs.client.rpc.retransmit.count"

  ### Erlang

  ```erlang
  ?NFS_CLIENT_RPC_RETRANSMIT_COUNT.
  'nfs.client.rpc.retransmit.count'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_client_rpc_retransmit_count :: :"nfs.client.rpc.retransmit.count"
  def nfs_client_rpc_retransmit_count do
    :"nfs.client.rpc.retransmit.count"
  end

  @doc """
  Reports the count of kernel NFS server stale file handles.

  Instrument: `counter`
  Unit: `{fh}`
  ### Notes

  Linux: this metric is taken from the Linux kernel NFSD_STATS_FH_STALE counter in the nfsd_net struct


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_server_fh_stale_count()
      :"nfs.server.fh.stale.count"

  ### Erlang

  ```erlang
  ?NFS_SERVER_FH_STALE_COUNT.
  'nfs.server.fh.stale.count'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_server_fh_stale_count :: :"nfs.server.fh.stale.count"
  def nfs_server_fh_stale_count do
    :"nfs.server.fh.stale.count"
  end

  @doc """
  Reports the count of kernel NFS server bytes returned to receive and transmit (read and write) requests.

  Instrument: `counter`
  Unit: `By`
  ### Notes

  Linux: this metric is taken from the Linux kernel NFSD_STATS_IO_READ and NFSD_STATS_IO_WRITE counters in the nfsd_net struct


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_server_io()
      :"nfs.server.io"

  ### Erlang

  ```erlang
  ?NFS_SERVER_IO.
  'nfs.server.io'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_server_io :: :"nfs.server.io"
  def nfs_server_io do
    :"nfs.server.io"
  end

  @doc """
  Reports the count of kernel NFS server TCP segments and UDP datagrams handled.

  Instrument: `counter`
  Unit: `{record}`
  ### Notes

  Linux: this metric is taken from the Linux kernel's svc_stat.nettcpcnt and svc_stat.netudpcnt


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_server_net_count()
      :"nfs.server.net.count"

  ### Erlang

  ```erlang
  ?NFS_SERVER_NET_COUNT.
  'nfs.server.net.count'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_server_net_count :: :"nfs.server.net.count"
  def nfs_server_net_count do
    :"nfs.server.net.count"
  end

  @doc """
  Reports the count of kernel NFS server TCP connections accepted.

  Instrument: `counter`
  Unit: `{connection}`
  ### Notes

  Linux: this metric is taken from the Linux kernel's svc_stat.nettcpconn


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_server_net_tcp_connection_accepted()
      :"nfs.server.net.tcp.connection.accepted"

  ### Erlang

  ```erlang
  ?NFS_SERVER_NET_TCP_CONNECTION_ACCEPTED.
  'nfs.server.net.tcp.connection.accepted'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_server_net_tcp_connection_accepted :: :"nfs.server.net.tcp.connection.accepted"
  def nfs_server_net_tcp_connection_accepted do
    :"nfs.server.net.tcp.connection.accepted"
  end

  @doc """
  Reports the count of kernel NFSv4+ server operations.

  Instrument: `counter`
  Unit: `{operation}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_server_operation_count()
      :"nfs.server.operation.count"

  ### Erlang

  ```erlang
  ?NFS_SERVER_OPERATION_COUNT.
  'nfs.server.operation.count'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_server_operation_count :: :"nfs.server.operation.count"
  def nfs_server_operation_count do
    :"nfs.server.operation.count"
  end

  @doc """
  Reports the count of kernel NFS server procedures.

  Instrument: `counter`
  Unit: `{procedure}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_server_procedure_count()
      :"nfs.server.procedure.count"

  ### Erlang

  ```erlang
  ?NFS_SERVER_PROCEDURE_COUNT.
  'nfs.server.procedure.count'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_server_procedure_count :: :"nfs.server.procedure.count"
  def nfs_server_procedure_count do
    :"nfs.server.procedure.count"
  end

  @doc """
  Reports the kernel NFS server reply cache request count by cache hit status.

  Instrument: `counter`
  Unit: `{request}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_server_repcache_requests()
      :"nfs.server.repcache.requests"

  ### Erlang

  ```erlang
  ?NFS_SERVER_REPCACHE_REQUESTS.
  'nfs.server.repcache.requests'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_server_repcache_requests :: :"nfs.server.repcache.requests"
  def nfs_server_repcache_requests do
    :"nfs.server.repcache.requests"
  end

  @doc """
  Reports the count of kernel NFS server RPCs handled.

  Instrument: `counter`
  Unit: `{request}`
  ### Notes

  Linux: this metric is taken from the Linux kernel's svc_stat.rpccnt, the count of good RPCs. This metric can have
  an error.type of "format", "auth", or "client" for svc_stat.badfmt, svc_stat.badauth, and svc_stat.badclnt.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_server_rpc_count()
      :"nfs.server.rpc.count"

  ### Erlang

  ```erlang
  ?NFS_SERVER_RPC_COUNT.
  'nfs.server.rpc.count'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_server_rpc_count :: :"nfs.server.rpc.count"
  def nfs_server_rpc_count do
    :"nfs.server.rpc.count"
  end

  @doc """
  Reports the count of kernel NFS server available threads.

  Instrument: `updowncounter`
  Unit: `{thread}`
  ### Notes

  Linux: this metric is taken from the Linux kernel nfsd_th_cnt variable


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.NfsMetrics.nfs_server_thread_count()
      :"nfs.server.thread.count"

  ### Erlang

  ```erlang
  ?NFS_SERVER_THREAD_COUNT.
  'nfs.server.thread.count'
  ```

  <!-- tabs-close -->
  """

  @spec nfs_server_thread_count :: :"nfs.server.thread.count"
  def nfs_server_thread_count do
    :"nfs.server.thread.count"
  end
end
