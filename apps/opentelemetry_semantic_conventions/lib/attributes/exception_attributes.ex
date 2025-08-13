defmodule OpenTelemetry.SemConv.ExceptionAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Exception attributes.
  """

  @deprecated """
  It's no longer recommended to record exceptions that are handled and do not escape the scope of a span.
  """
  @spec exception_escaped :: :"exception.escaped"
  def exception_escaped do
    :"exception.escaped"
  end

  @doc """
  The exception message.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Division by zero", "Can't convert 'int' object to str implicitly"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.ExceptionAttributes.exception_message()
      :"exception.message"

  ### Erlang

  ```erlang
  ?EXCEPTION_MESSAGE.
  'exception.message'
  ```

  <!-- tabs-close -->
  """
  @spec exception_message :: :"exception.message"
  def exception_message do
    :"exception.message"
  end

  @doc """
  A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  Exception in thread "main" java.lang.RuntimeException: Test exception\n at com.example.GenerateTrace.methodB(GenerateTrace.java:13)\n at com.example.GenerateTrace.methodA(GenerateTrace.java:9)\n at com.example.GenerateTrace.main(GenerateTrace.java:5)

  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.ExceptionAttributes.exception_stacktrace()
      :"exception.stacktrace"

  ### Erlang

  ```erlang
  ?EXCEPTION_STACKTRACE.
  'exception.stacktrace'
  ```

  <!-- tabs-close -->
  """
  @spec exception_stacktrace :: :"exception.stacktrace"
  def exception_stacktrace do
    :"exception.stacktrace"
  end

  @doc """
  The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["java.net.ConnectException", "OSError"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.ExceptionAttributes.exception_type()
      :"exception.type"

  ### Erlang

  ```erlang
  ?EXCEPTION_TYPE.
  'exception.type'
  ```

  <!-- tabs-close -->
  """
  @spec exception_type :: :"exception.type"
  def exception_type do
    :"exception.type"
  end
end
