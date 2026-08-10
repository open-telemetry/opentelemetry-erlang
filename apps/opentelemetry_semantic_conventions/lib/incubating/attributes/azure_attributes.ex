defmodule OpenTelemetry.SemConv.Incubating.AzureAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Azure attributes.
  """

  @deprecated """
  Replaced by `azure.resource_provider.namespace`.
  """
  @spec az_namespace :: :"az.namespace"
  def az_namespace do
    :"az.namespace"
  end

  @deprecated """
  Replaced by `azure.service.request.id`.
  """
  @spec az_service_request_id :: :"az.service_request_id"
  def az_service_request_id do
    :"az.service_request_id"
  end

  @doc """
  The unique identifier of the client instance.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["3ba4827d-4422-483f-b59f-85b74211c11d", "storage-client-1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_client_id()
      :"azure.client.id"

  ### Erlang

  ```erlang
  ?AZURE_CLIENT_ID.
  'azure.client.id'
  ```

  <!-- tabs-close -->
  """
  @spec azure_client_id :: :"azure.client.id"
  def azure_client_id do
    :"azure.client.id"
  end

  @typedoc """
  Cosmos client connection mode.

  ### Enum Values
  * `:gateway` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Gateway (HTTP) connection.
  * `:direct` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Direct connection.
  """
  @type azure_cosmosdb_connection_mode_values() :: %{
          :gateway => :gateway,
          :direct => :direct
        }
  @doc """
  Cosmos client connection mode.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_cosmosdb_connection_mode()
      :"azure.cosmosdb.connection.mode"

      iex> OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_cosmosdb_connection_mode_values().gateway
      :gateway

      iex> %{OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_cosmosdb_connection_mode() => OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_cosmosdb_connection_mode_values().gateway}
      %{:"azure.cosmosdb.connection.mode" => :gateway}

  ### Erlang

  ```erlang
  ?AZURE_COSMOSDB_CONNECTION_MODE.
  'azure.cosmosdb.connection.mode'

  ?AZURE_COSMOSDB_CONNECTION_MODE_VALUES_GATEWAY.
  'gateway'

  \#{?AZURE_COSMOSDB_CONNECTION_MODE => ?AZURE_COSMOSDB_CONNECTION_MODE_VALUES_GATEWAY}.
  \#{'azure.cosmosdb.connection.mode' => 'gateway'}
  ```

  <!-- tabs-close -->
  """
  @spec azure_cosmosdb_connection_mode :: :"azure.cosmosdb.connection.mode"
  def azure_cosmosdb_connection_mode do
    :"azure.cosmosdb.connection.mode"
  end

  @spec azure_cosmosdb_connection_mode_values() :: azure_cosmosdb_connection_mode_values()
  def azure_cosmosdb_connection_mode_values() do
    %{
      :gateway => :gateway,
      :direct => :direct
    }
  end

  @typedoc """
  Account or request [consistency level](https://learn.microsoft.com/azure/cosmos-db/consistency-levels).

  ### Enum Values
  * `:strong` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Strong
  * `:bounded_staleness` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Bounded Staleness
  * `:session` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Session
  * `:eventual` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Eventual
  * `:consistent_prefix` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Consistent Prefix
  """
  @type azure_cosmosdb_consistency_level_values() :: %{
          :strong => :Strong,
          :bounded_staleness => :BoundedStaleness,
          :session => :Session,
          :eventual => :Eventual,
          :consistent_prefix => :ConsistentPrefix
        }
  @doc """
  Account or request [consistency level](https://learn.microsoft.com/azure/cosmos-db/consistency-levels).

  ### Examples

  ```
  ["Eventual", "ConsistentPrefix", "BoundedStaleness", "Strong", "Session"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_cosmosdb_consistency_level()
      :"azure.cosmosdb.consistency.level"

      iex> OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_cosmosdb_consistency_level_values().strong
      :Strong

      iex> %{OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_cosmosdb_consistency_level() => OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_cosmosdb_consistency_level_values().strong}
      %{:"azure.cosmosdb.consistency.level" => :Strong}

  ### Erlang

  ```erlang
  ?AZURE_COSMOSDB_CONSISTENCY_LEVEL.
  'azure.cosmosdb.consistency.level'

  ?AZURE_COSMOSDB_CONSISTENCY_LEVEL_VALUES_STRONG.
  'Strong'

  \#{?AZURE_COSMOSDB_CONSISTENCY_LEVEL => ?AZURE_COSMOSDB_CONSISTENCY_LEVEL_VALUES_STRONG}.
  \#{'azure.cosmosdb.consistency.level' => 'Strong'}
  ```

  <!-- tabs-close -->
  """
  @spec azure_cosmosdb_consistency_level :: :"azure.cosmosdb.consistency.level"
  def azure_cosmosdb_consistency_level do
    :"azure.cosmosdb.consistency.level"
  end

  @spec azure_cosmosdb_consistency_level_values() :: azure_cosmosdb_consistency_level_values()
  def azure_cosmosdb_consistency_level_values() do
    %{
      :strong => :Strong,
      :bounded_staleness => :BoundedStaleness,
      :session => :Session,
      :eventual => :Eventual,
      :consistent_prefix => :ConsistentPrefix
    }
  end

  @doc """
  List of regions contacted during operation in the order that they were contacted. If there is more than one region listed, it indicates that the operation was performed on multiple regions i.e. cross-regional call.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  Region name matches the format of `displayName` in [Azure Location API](https://learn.microsoft.com/rest/api/resources/subscriptions/list-locations)

  ### Examples

  ```
  [["North Central US", "Australia East", "Australia Southeast"]]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_cosmosdb_operation_contacted_regions()
      :"azure.cosmosdb.operation.contacted_regions"

  ### Erlang

  ```erlang
  ?AZURE_COSMOSDB_OPERATION_CONTACTED_REGIONS.
  'azure.cosmosdb.operation.contacted_regions'
  ```

  <!-- tabs-close -->
  """
  @spec azure_cosmosdb_operation_contacted_regions ::
          :"azure.cosmosdb.operation.contacted_regions"
  def azure_cosmosdb_operation_contacted_regions do
    :"azure.cosmosdb.operation.contacted_regions"
  end

  @doc """
  The number of request units consumed by the operation.

  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [46.18, 1.0]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_cosmosdb_operation_request_charge()
      :"azure.cosmosdb.operation.request_charge"

  ### Erlang

  ```erlang
  ?AZURE_COSMOSDB_OPERATION_REQUEST_CHARGE.
  'azure.cosmosdb.operation.request_charge'
  ```

  <!-- tabs-close -->
  """
  @spec azure_cosmosdb_operation_request_charge :: :"azure.cosmosdb.operation.request_charge"
  def azure_cosmosdb_operation_request_charge do
    :"azure.cosmosdb.operation.request_charge"
  end

  @doc """
  Request payload size in bytes.
  ### Value type

  Value must be of type `integer()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_cosmosdb_request_body_size()
      :"azure.cosmosdb.request.body.size"

  ### Erlang

  ```erlang
  ?AZURE_COSMOSDB_REQUEST_BODY_SIZE.
  'azure.cosmosdb.request.body.size'
  ```

  <!-- tabs-close -->
  """
  @spec azure_cosmosdb_request_body_size :: :"azure.cosmosdb.request.body.size"
  def azure_cosmosdb_request_body_size do
    :"azure.cosmosdb.request.body.size"
  end

  @doc """
  Cosmos DB sub status code.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [1000, 1002]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_cosmosdb_response_sub_status_code()
      :"azure.cosmosdb.response.sub_status_code"

  ### Erlang

  ```erlang
  ?AZURE_COSMOSDB_RESPONSE_SUB_STATUS_CODE.
  'azure.cosmosdb.response.sub_status_code'
  ```

  <!-- tabs-close -->
  """
  @spec azure_cosmosdb_response_sub_status_code :: :"azure.cosmosdb.response.sub_status_code"
  def azure_cosmosdb_response_sub_status_code do
    :"azure.cosmosdb.response.sub_status_code"
  end

  @doc """
  [Azure Resource Provider Namespace](https://learn.microsoft.com/azure/azure-resource-manager/management/azure-services-resource-providers) as recognized by the client.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Microsoft.Storage", "Microsoft.KeyVault", "Microsoft.ServiceBus"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_resource_provider_namespace()
      :"azure.resource_provider.namespace"

  ### Erlang

  ```erlang
  ?AZURE_RESOURCE_PROVIDER_NAMESPACE.
  'azure.resource_provider.namespace'
  ```

  <!-- tabs-close -->
  """
  @spec azure_resource_provider_namespace :: :"azure.resource_provider.namespace"
  def azure_resource_provider_namespace do
    :"azure.resource_provider.namespace"
  end

  @doc """
  The unique identifier of the service request. It's generated by the Azure service and returned with the response.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["00000000-0000-0000-0000-000000000000"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AzureAttributes.azure_service_request_id()
      :"azure.service.request.id"

  ### Erlang

  ```erlang
  ?AZURE_SERVICE_REQUEST_ID.
  'azure.service.request.id'
  ```

  <!-- tabs-close -->
  """
  @spec azure_service_request_id :: :"azure.service.request.id"
  def azure_service_request_id do
    :"azure.service.request.id"
  end
end
