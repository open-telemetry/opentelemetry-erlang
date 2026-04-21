defmodule OpenTelemetry.SemConv.Incubating.MainframeAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Mainframe attributes.
  """

  @doc """
  Name of the logical partition that hosts a systems with a mainframe operating system.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["LPAR01"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MainframeAttributes.mainframe_lpar_name()
      :"mainframe.lpar.name"

  ### Erlang

  ```erlang
  ?MAINFRAME_LPAR_NAME.
  'mainframe.lpar.name'
  ```

  <!-- tabs-close -->
  """
  @spec mainframe_lpar_name :: :"mainframe.lpar.name"
  def mainframe_lpar_name do
    :"mainframe.lpar.name"
  end
end
