defmodule OpenTelemetry.SemConv.Incubating.Metrics.DBMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for DB metrics.
  """
  @doc """
  The number of connections that are currently in state described by the `state` attribute

  Instrument: `updowncounter`
  Unit: `{connection}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.DBMetrics.db_client_connection_count()
      :"db.client.connection.count"

  ### Erlang

  ```erlang
  ?DB_CLIENT_CONNECTION_COUNT.
  'db.client.connection.count'
  ```

  <!-- tabs-close -->
  """

  @spec db_client_connection_count :: :"db.client.connection.count"
  def db_client_connection_count do
    :"db.client.connection.count"
  end

  @doc """
  The time it took to create a new connection

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.DBMetrics.db_client_connection_create_time()
      :"db.client.connection.create_time"

  ### Erlang

  ```erlang
  ?DB_CLIENT_CONNECTION_CREATE_TIME.
  'db.client.connection.create_time'
  ```

  <!-- tabs-close -->
  """

  @spec db_client_connection_create_time :: :"db.client.connection.create_time"
  def db_client_connection_create_time do
    :"db.client.connection.create_time"
  end

  @doc """
  The maximum number of idle open connections allowed

  Instrument: `updowncounter`
  Unit: `{connection}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.DBMetrics.db_client_connection_idle_max()
      :"db.client.connection.idle.max"

  ### Erlang

  ```erlang
  ?DB_CLIENT_CONNECTION_IDLE_MAX.
  'db.client.connection.idle.max'
  ```

  <!-- tabs-close -->
  """

  @spec db_client_connection_idle_max :: :"db.client.connection.idle.max"
  def db_client_connection_idle_max do
    :"db.client.connection.idle.max"
  end

  @doc """
  The minimum number of idle open connections allowed

  Instrument: `updowncounter`
  Unit: `{connection}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.DBMetrics.db_client_connection_idle_min()
      :"db.client.connection.idle.min"

  ### Erlang

  ```erlang
  ?DB_CLIENT_CONNECTION_IDLE_MIN.
  'db.client.connection.idle.min'
  ```

  <!-- tabs-close -->
  """

  @spec db_client_connection_idle_min :: :"db.client.connection.idle.min"
  def db_client_connection_idle_min do
    :"db.client.connection.idle.min"
  end

  @doc """
  The maximum number of open connections allowed

  Instrument: `updowncounter`
  Unit: `{connection}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.DBMetrics.db_client_connection_max()
      :"db.client.connection.max"

  ### Erlang

  ```erlang
  ?DB_CLIENT_CONNECTION_MAX.
  'db.client.connection.max'
  ```

  <!-- tabs-close -->
  """

  @spec db_client_connection_max :: :"db.client.connection.max"
  def db_client_connection_max do
    :"db.client.connection.max"
  end

  @doc """
  The number of pending requests for an open connection, cumulative for the entire pool

  Instrument: `updowncounter`
  Unit: `{request}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.DBMetrics.db_client_connection_pending_requests()
      :"db.client.connection.pending_requests"

  ### Erlang

  ```erlang
  ?DB_CLIENT_CONNECTION_PENDING_REQUESTS.
  'db.client.connection.pending_requests'
  ```

  <!-- tabs-close -->
  """

  @spec db_client_connection_pending_requests :: :"db.client.connection.pending_requests"
  def db_client_connection_pending_requests do
    :"db.client.connection.pending_requests"
  end

  @doc """
  The number of connection timeouts that have occurred trying to obtain a connection from the pool

  Instrument: `counter`
  Unit: `{timeout}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.DBMetrics.db_client_connection_timeouts()
      :"db.client.connection.timeouts"

  ### Erlang

  ```erlang
  ?DB_CLIENT_CONNECTION_TIMEOUTS.
  'db.client.connection.timeouts'
  ```

  <!-- tabs-close -->
  """

  @spec db_client_connection_timeouts :: :"db.client.connection.timeouts"
  def db_client_connection_timeouts do
    :"db.client.connection.timeouts"
  end

  @doc """
  The time between borrowing a connection and returning it to the pool

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.DBMetrics.db_client_connection_use_time()
      :"db.client.connection.use_time"

  ### Erlang

  ```erlang
  ?DB_CLIENT_CONNECTION_USE_TIME.
  'db.client.connection.use_time'
  ```

  <!-- tabs-close -->
  """

  @spec db_client_connection_use_time :: :"db.client.connection.use_time"
  def db_client_connection_use_time do
    :"db.client.connection.use_time"
  end

  @doc """
  The time it took to obtain an open connection from the pool

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.DBMetrics.db_client_connection_wait_time()
      :"db.client.connection.wait_time"

  ### Erlang

  ```erlang
  ?DB_CLIENT_CONNECTION_WAIT_TIME.
  'db.client.connection.wait_time'
  ```

  <!-- tabs-close -->
  """

  @spec db_client_connection_wait_time :: :"db.client.connection.wait_time"
  def db_client_connection_wait_time do
    :"db.client.connection.wait_time"
  end

  @deprecated """
  Replaced by `db.client.connection.create_time`. Note: the unit also changed from `ms` to `s`.
  """

  @spec db_client_connections_create_time :: :"db.client.connections.create_time"
  def db_client_connections_create_time do
    :"db.client.connections.create_time"
  end

  @deprecated """
  Replaced by `db.client.connection.idle.max`.
  """

  @spec db_client_connections_idle_max :: :"db.client.connections.idle.max"
  def db_client_connections_idle_max do
    :"db.client.connections.idle.max"
  end

  @deprecated """
  Replaced by `db.client.connection.idle.min`.
  """

  @spec db_client_connections_idle_min :: :"db.client.connections.idle.min"
  def db_client_connections_idle_min do
    :"db.client.connections.idle.min"
  end

  @deprecated """
  Replaced by `db.client.connection.max`.
  """

  @spec db_client_connections_max :: :"db.client.connections.max"
  def db_client_connections_max do
    :"db.client.connections.max"
  end

  @deprecated """
  Replaced by `db.client.connection.pending_requests`.
  """

  @spec db_client_connections_pending_requests :: :"db.client.connections.pending_requests"
  def db_client_connections_pending_requests do
    :"db.client.connections.pending_requests"
  end

  @deprecated """
  Replaced by `db.client.connection.timeouts`.
  """

  @spec db_client_connections_timeouts :: :"db.client.connections.timeouts"
  def db_client_connections_timeouts do
    :"db.client.connections.timeouts"
  end

  @deprecated """
  Replaced by `db.client.connection.count`.
  """

  @spec db_client_connections_usage :: :"db.client.connections.usage"
  def db_client_connections_usage do
    :"db.client.connections.usage"
  end

  @deprecated """
  Replaced by `db.client.connection.use_time`. Note: the unit also changed from `ms` to `s`.
  """

  @spec db_client_connections_use_time :: :"db.client.connections.use_time"
  def db_client_connections_use_time do
    :"db.client.connections.use_time"
  end

  @deprecated """
  Replaced by `db.client.connection.wait_time`. Note: the unit also changed from `ms` to `s`.
  """

  @spec db_client_connections_wait_time :: :"db.client.connections.wait_time"
  def db_client_connections_wait_time do
    :"db.client.connections.wait_time"
  end

  @doc """
  Duration of database client operations.

  Instrument: `histogram`
  Unit: `s`
  ### Notes

  Batch operations **SHOULD** be recorded as a single operation.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.DBMetrics.db_client_operation_duration()
      :"db.client.operation.duration"

  ### Erlang

  ```erlang
  ?DB_CLIENT_OPERATION_DURATION.
  'db.client.operation.duration'
  ```

  <!-- tabs-close -->
  """

  @spec db_client_operation_duration :: :"db.client.operation.duration"
  def db_client_operation_duration do
    :"db.client.operation.duration"
  end
end
