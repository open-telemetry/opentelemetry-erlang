defmodule OpenTelemetry.SemConv.ErrorAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Error attributes.
  """

  @typedoc """
  Describes a class of error the operation ended with.


  ### Enum Values
  * `:other` - A fallback error value to be used when the instrumentation doesn't define a custom value.

  """
  @type error_type_values() :: %{
          :other => :_OTHER
        }
  @doc """
  Describes a class of error the operation ended with.


  ### Notes

  The `error.type` **SHOULD** be predictable, and **SHOULD** have low cardinality.

  When `error.type` is set to a type (e.g., an exception type), its
  canonical class name identifying the type within the artifact **SHOULD** be used.

  Instrumentations **SHOULD** document the list of errors they report.

  The cardinality of `error.type` within one instrumentation library **SHOULD** be low.
  Telemetry consumers that aggregate data from multiple instrumentation libraries and applications
  should be prepared for `error.type` to have high cardinality at query time when no
  additional filters are applied.

  If the operation has completed successfully, instrumentations **SHOULD** **NOT** set `error.type`.

  If a specific domain defines its own set of error identifiers (such as HTTP or gRPC status codes),
  it's RECOMMENDED to:

  * Use a domain-specific attribute
  * Set `error.type` to capture all errors, regardless of whether they are defined within the domain-specific set or not.

  ### Examples

  ```
  ["timeout", "java.net.UnknownHostException", "server_certificate_invalid", "500"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.ErrorAttributes.error_type()
      :"error.type"

      iex> OpenTelemetry.SemConv.ErrorAttributes.error_type_values().other
      :_OTHER

      iex> %{OpenTelemetry.SemConv.ErrorAttributes.error_type() => OpenTelemetry.SemConv.ErrorAttributes.error_type_values().other}
      %{:"error.type" => :_OTHER}

  ### Erlang

  ```erlang
  ?ERROR_TYPE.
  'error.type'

  ?ERROR_TYPE_VALUES_OTHER.
  '_OTHER'

  \#{?ERROR_TYPE => ?ERROR_TYPE_VALUES_OTHER}.
  \#{'error.type' => '_OTHER'}
  ```

  <!-- tabs-close -->
  """
  @spec error_type :: :"error.type"
  def error_type do
    :"error.type"
  end

  @spec error_type_values() :: error_type_values()
  def error_type_values() do
    %{
      :other => :_OTHER
    }
  end
end
