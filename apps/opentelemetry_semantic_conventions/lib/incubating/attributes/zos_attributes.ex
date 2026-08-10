defmodule OpenTelemetry.SemConv.Incubating.ZosAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Zos attributes.
  """

  @doc """
  The System Management Facility (SMF) Identifier uniquely identified a z/OS system within a SYSPLEX or mainframe environment and is used for system and performance analysis.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["SYS1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ZosAttributes.zos_smf_id()
      :"zos.smf.id"

  ### Erlang

  ```erlang
  ?ZOS_SMF_ID.
  'zos.smf.id'
  ```

  <!-- tabs-close -->
  """
  @spec zos_smf_id :: :"zos.smf.id"
  def zos_smf_id do
    :"zos.smf.id"
  end

  @doc """
  The name of the SYSPLEX to which the z/OS system belongs too.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["SYSPLEX1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ZosAttributes.zos_sysplex_name()
      :"zos.sysplex.name"

  ### Erlang

  ```erlang
  ?ZOS_SYSPLEX_NAME.
  'zos.sysplex.name'
  ```

  <!-- tabs-close -->
  """
  @spec zos_sysplex_name :: :"zos.sysplex.name"
  def zos_sysplex_name do
    :"zos.sysplex.name"
  end
end
