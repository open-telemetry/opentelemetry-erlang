defmodule OpenTelemetry.SemConv.Incubating.OncRpcAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Onc_Rpc attributes.
  """

  @doc """
  ONC/Sun RPC procedure name.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["OPEN", "READ", "GETATTR"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OncRpcAttributes.onc_rpc_procedure_name()
      :"onc_rpc.procedure.name"

  ### Erlang

  ```erlang
  ?ONC_RPC_PROCEDURE_NAME.
  'onc_rpc.procedure.name'
  ```

  <!-- tabs-close -->
  """
  @spec onc_rpc_procedure_name :: :"onc_rpc.procedure.name"
  def onc_rpc_procedure_name do
    :"onc_rpc.procedure.name"
  end

  @doc """
  ONC/Sun RPC procedure number.
  ### Value type

  Value must be of type `integer()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OncRpcAttributes.onc_rpc_procedure_number()
      :"onc_rpc.procedure.number"

  ### Erlang

  ```erlang
  ?ONC_RPC_PROCEDURE_NUMBER.
  'onc_rpc.procedure.number'
  ```

  <!-- tabs-close -->
  """
  @spec onc_rpc_procedure_number :: :"onc_rpc.procedure.number"
  def onc_rpc_procedure_number do
    :"onc_rpc.procedure.number"
  end

  @doc """
  ONC/Sun RPC program name.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["portmapper", "nfs"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OncRpcAttributes.onc_rpc_program_name()
      :"onc_rpc.program.name"

  ### Erlang

  ```erlang
  ?ONC_RPC_PROGRAM_NAME.
  'onc_rpc.program.name'
  ```

  <!-- tabs-close -->
  """
  @spec onc_rpc_program_name :: :"onc_rpc.program.name"
  def onc_rpc_program_name do
    :"onc_rpc.program.name"
  end

  @doc """
  ONC/Sun RPC program version.
  ### Value type

  Value must be of type `integer()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OncRpcAttributes.onc_rpc_version()
      :"onc_rpc.version"

  ### Erlang

  ```erlang
  ?ONC_RPC_VERSION.
  'onc_rpc.version'
  ```

  <!-- tabs-close -->
  """
  @spec onc_rpc_version :: :"onc_rpc.version"
  def onc_rpc_version do
    :"onc_rpc.version"
  end
end
