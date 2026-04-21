defmodule OpenTelemetry.SemConv.Metrics.DBMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for DB metrics.
  """
  @doc """
  Duration of database client operations.

  Instrument: `histogram`
  Unit: `s`
  ### Notes

  Batch operations **SHOULD** be recorded as a single operation.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Metrics.DBMetrics.db_client_operation_duration()
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
