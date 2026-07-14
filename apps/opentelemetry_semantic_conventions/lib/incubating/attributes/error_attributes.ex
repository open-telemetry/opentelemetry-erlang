defmodule OpenTelemetry.SemConv.Incubating.ErrorAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Error attributes.
  """
  defdelegate error_type(), to: OpenTelemetry.SemConv.ErrorAttributes

  defdelegate error_type_values(), to: OpenTelemetry.SemConv.ErrorAttributes

  @doc """
  A message providing more detail about an error in human-readable form.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  `error.message` should provide additional context and detail about an error.
  It is **NOT** RECOMMENDED to duplicate the value of `error.type` in `error.message`.
  It is also **NOT** RECOMMENDED to duplicate the value of `exception.message` in `error.message`.

  `error.message` is **NOT** RECOMMENDED for metrics or spans due to its unbounded cardinality and overlap with span status.

  ### Examples

  ```
  ["Unexpected input type: string", "The user has exceeded their storage quota"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ErrorAttributes.error_message()
      :"error.message"

  ### Erlang

  ```erlang
  ?ERROR_MESSAGE.
  'error.message'
  ```

  <!-- tabs-close -->
  """
  @spec error_message :: :"error.message"
  def error_message do
    :"error.message"
  end
end
