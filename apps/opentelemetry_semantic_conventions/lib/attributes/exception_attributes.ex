defmodule OpenTelemetry.SemConv.ExceptionAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Exception attributes.
  """

  @doc """
  SHOULD be set to true if the exception event is recorded at a point where it is known that the exception is escaping the scope of the span.

  ### Value type

  Value must be of type `boolean()`.
  ### Notes

  An exception is considered to have escaped (or left) the scope of a span,
  if that span is ended while the exception is still logically "in flight".
  This may be actually "in flight" in some languages (e.g. if the exception
  is passed to a Context manager's `__exit__` method in Python) but will
  usually be caught at the point of recording the exception in most languages.

  It is usually not possible to determine at the point where an exception is thrown
  whether it will escape the scope of a span.
  However, it is trivial to know that an exception
  will escape, if one checks for an active exception just before ending the span,
  as done in the [example for recording span exceptions](https://opentelemetry.io/docs/specs/semconv/exceptions/exceptions-spans/#recording-an-exception).

  It follows that an exception may still escape the scope of the span
  even if the `exception.escaped` attribute was not set or set to false,
  since the event might have been recorded at a time where it was not
  clear whether the exception will escape.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.ExceptionAttributes.exception_escaped()
      :"exception.escaped"

  ### Erlang

  ```erlang
  ?EXCEPTION_ESCAPED.
  'exception.escaped'
  ```

  <!-- tabs-close -->
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
