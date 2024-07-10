defmodule OpenTelemetry.SemConv.ServiceAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Service attributes.
  """

  @doc """
  Logical name of the service.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  MUST be the same for all instances of horizontally scaled services. If the value was not specified, SDKs **MUST** fallback to `unknown_service:` concatenated with [`process.executable.name`](process.md), e.g. `unknown_service:bash`. If `process.executable.name` is not available, the value **MUST** be set to `unknown_service`.

  ### Examples

  ```
  ["shoppingcart"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.ServiceAttributes.service_name()
      :"service.name"

  ### Erlang

  ```erlang
  ?SERVICE_NAME.
  'service.name'
  ```

  <!-- tabs-close -->
  """
  @spec service_name :: :"service.name"
  def service_name do
    :"service.name"
  end

  @doc """
  The version string of the service API or implementation. The format is not defined by these conventions.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["2.0.0", "a01dbef8a"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.ServiceAttributes.service_version()
      :"service.version"

  ### Erlang

  ```erlang
  ?SERVICE_VERSION.
  'service.version'
  ```

  <!-- tabs-close -->
  """
  @spec service_version :: :"service.version"
  def service_version do
    :"service.version"
  end
end
