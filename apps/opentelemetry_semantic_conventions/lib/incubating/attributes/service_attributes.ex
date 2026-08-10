defmodule OpenTelemetry.SemConv.Incubating.ServiceAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Service attributes.
  """
  defdelegate service_instance_id(), to: OpenTelemetry.SemConv.ServiceAttributes

  defdelegate service_name(), to: OpenTelemetry.SemConv.ServiceAttributes

  defdelegate service_namespace(), to: OpenTelemetry.SemConv.ServiceAttributes

  defdelegate service_version(), to: OpenTelemetry.SemConv.ServiceAttributes

  @typedoc """
  The operational criticality of the service.


  ### Enum Values
  * `:critical` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Service is business-critical; downtime directly impacts revenue, user experience, or core functionality.

  * `:high` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Service is important but has degradation tolerance or fallback mechanisms.

  * `:medium` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Service provides supplementary functionality; degradation has limited user impact.

  * `:low` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Service is non-essential to core operations; used for background tasks or internal tools.

  """
  @type service_criticality_values() :: %{
          :critical => :critical,
          :high => :high,
          :medium => :medium,
          :low => :low
        }
  @doc """
  The operational criticality of the service.


  ### Notes

  Application developers are encouraged to set `service.criticality` to express the operational importance of their services. Telemetry consumers **MAY** use this attribute to optimize telemetry collection or improve user experience.

  ### Examples

  ```
  ["critical", "high", "medium", "low"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ServiceAttributes.service_criticality()
      :"service.criticality"

      iex> OpenTelemetry.SemConv.Incubating.ServiceAttributes.service_criticality_values().critical
      :critical

      iex> %{OpenTelemetry.SemConv.Incubating.ServiceAttributes.service_criticality() => OpenTelemetry.SemConv.Incubating.ServiceAttributes.service_criticality_values().critical}
      %{:"service.criticality" => :critical}

  ### Erlang

  ```erlang
  ?SERVICE_CRITICALITY.
  'service.criticality'

  ?SERVICE_CRITICALITY_VALUES_CRITICAL.
  'critical'

  \#{?SERVICE_CRITICALITY => ?SERVICE_CRITICALITY_VALUES_CRITICAL}.
  \#{'service.criticality' => 'critical'}
  ```

  <!-- tabs-close -->
  """
  @spec service_criticality :: :"service.criticality"
  def service_criticality do
    :"service.criticality"
  end

  @spec service_criticality_values() :: service_criticality_values()
  def service_criticality_values() do
    %{
      :critical => :critical,
      :high => :high,
      :medium => :medium,
      :low => :low
    }
  end

  @doc """
  Logical name of the service on the other side of the connection. **SHOULD** be equal to the actual [`service.name`](/docs/resource/README.md#service) resource attribute of the remote service if any.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["shoppingcart"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ServiceAttributes.service_peer_name()
      :"service.peer.name"

  ### Erlang

  ```erlang
  ?SERVICE_PEER_NAME.
  'service.peer.name'
  ```

  <!-- tabs-close -->
  """
  @spec service_peer_name :: :"service.peer.name"
  def service_peer_name do
    :"service.peer.name"
  end

  @doc """
  Logical namespace of the service on the other side of the connection. **SHOULD** be equal to the actual [`service.namespace`](/docs/resource/README.md#service) resource attribute of the remote service if any.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Shop"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ServiceAttributes.service_peer_namespace()
      :"service.peer.namespace"

  ### Erlang

  ```erlang
  ?SERVICE_PEER_NAMESPACE.
  'service.peer.namespace'
  ```

  <!-- tabs-close -->
  """
  @spec service_peer_namespace :: :"service.peer.namespace"
  def service_peer_namespace do
    :"service.peer.namespace"
  end
end
