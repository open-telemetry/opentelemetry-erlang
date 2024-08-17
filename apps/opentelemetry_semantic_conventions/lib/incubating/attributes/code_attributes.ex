defmodule OpenTelemetry.SemConv.Incubating.CodeAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Code attributes.
  """

  @doc """
  The column number in `code.filepath` best representing the operation. It **SHOULD** point within the code unit named in `code.function`.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  16
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CodeAttributes.code_column()
      :"code.column"

  ### Erlang

  ```erlang
  ?CODE_COLUMN.
  'code.column'
  ```

  <!-- tabs-close -->
  """
  @spec code_column :: :"code.column"
  def code_column do
    :"code.column"
  end

  @doc """
  The source code file name that identifies the code unit as uniquely as possible (preferably an absolute file path).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  /usr/local/MyApplication/content_root/app/index.php
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CodeAttributes.code_filepath()
      :"code.filepath"

  ### Erlang

  ```erlang
  ?CODE_FILEPATH.
  'code.filepath'
  ```

  <!-- tabs-close -->
  """
  @spec code_filepath :: :"code.filepath"
  def code_filepath do
    :"code.filepath"
  end

  @doc """
  The method or function name, or equivalent (usually rightmost part of the code unit's name).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  serveRequest
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CodeAttributes.code_function()
      :"code.function"

  ### Erlang

  ```erlang
  ?CODE_FUNCTION.
  'code.function'
  ```

  <!-- tabs-close -->
  """
  @spec code_function :: :"code.function"
  def code_function do
    :"code.function"
  end

  @doc """
  The line number in `code.filepath` best representing the operation. It **SHOULD** point within the code unit named in `code.function`.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  42
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CodeAttributes.code_lineno()
      :"code.lineno"

  ### Erlang

  ```erlang
  ?CODE_LINENO.
  'code.lineno'
  ```

  <!-- tabs-close -->
  """
  @spec code_lineno :: :"code.lineno"
  def code_lineno do
    :"code.lineno"
  end

  @doc """
  The "namespace" within which `code.function` is defined. Usually the qualified class or module name, such that `code.namespace` + some separator + `code.function` form a unique identifier for the code unit.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  com.example.MyHttpService
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CodeAttributes.code_namespace()
      :"code.namespace"

  ### Erlang

  ```erlang
  ?CODE_NAMESPACE.
  'code.namespace'
  ```

  <!-- tabs-close -->
  """
  @spec code_namespace :: :"code.namespace"
  def code_namespace do
    :"code.namespace"
  end

  @doc """
  A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  at com.example.GenerateTrace.methodB(GenerateTrace.java:13)\n at com.example.GenerateTrace.methodA(GenerateTrace.java:9)\n at com.example.GenerateTrace.main(GenerateTrace.java:5)
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CodeAttributes.code_stacktrace()
      :"code.stacktrace"

  ### Erlang

  ```erlang
  ?CODE_STACKTRACE.
  'code.stacktrace'
  ```

  <!-- tabs-close -->
  """
  @spec code_stacktrace :: :"code.stacktrace"
  def code_stacktrace do
    :"code.stacktrace"
  end
end
