defmodule OpenTelemetry.SemConv.Incubating.Metrics.AzureMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Azure metrics.
  """
  @doc """
  Number of active client instances

  Instrument: `updowncounter`
  Unit: `{instance}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.AzureMetrics.azure_cosmosdb_client_active_instance_count()
      :"azure.cosmosdb.client.active_instance.count"

  ### Erlang

  ```erlang
  ?AZURE_COSMOSDB_CLIENT_ACTIVE_INSTANCE_COUNT.
  'azure.cosmosdb.client.active_instance.count'
  ```

  <!-- tabs-close -->
  """

  @spec azure_cosmosdb_client_active_instance_count ::
          :"azure.cosmosdb.client.active_instance.count"
  def azure_cosmosdb_client_active_instance_count do
    :"azure.cosmosdb.client.active_instance.count"
  end

  @doc """
  [Request units](https://learn.microsoft.com/azure/cosmos-db/request-units) consumed by the operation

  Instrument: `histogram`
  Unit: `{request_unit}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.AzureMetrics.azure_cosmosdb_client_operation_request_charge()
      :"azure.cosmosdb.client.operation.request_charge"

  ### Erlang

  ```erlang
  ?AZURE_COSMOSDB_CLIENT_OPERATION_REQUEST_CHARGE.
  'azure.cosmosdb.client.operation.request_charge'
  ```

  <!-- tabs-close -->
  """

  @spec azure_cosmosdb_client_operation_request_charge ::
          :"azure.cosmosdb.client.operation.request_charge"
  def azure_cosmosdb_client_operation_request_charge do
    :"azure.cosmosdb.client.operation.request_charge"
  end
end
