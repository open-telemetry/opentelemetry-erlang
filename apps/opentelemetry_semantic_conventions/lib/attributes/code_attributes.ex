defmodule OpenTelemetry.SemConv.CodeAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Code attributes.
  """

  @doc """
  The column number in `code.file.path` best representing the operation. It **SHOULD** point within the code unit named in `code.function.name`. This attribute **MUST** **NOT** be used on the Profile signal since the data is already captured in 'message Line'. This constraint is imposed to prevent redundancy and maintain data integrity.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  16
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.CodeAttributes.code_column_number()
      :"code.column.number"

  ### Erlang

  ```erlang
  ?CODE_COLUMN_NUMBER.
  'code.column.number'
  ```

  <!-- tabs-close -->
  """
  @spec code_column_number :: :"code.column.number"
  def code_column_number do
    :"code.column.number"
  end

  @doc """
  The source code file name that identifies the code unit as uniquely as possible (preferably an absolute file path). This attribute **MUST** **NOT** be used on the Profile signal since the data is already captured in 'message Function'. This constraint is imposed to prevent redundancy and maintain data integrity.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  /usr/local/MyApplication/content_root/app/index.php
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.CodeAttributes.code_file_path()
      :"code.file.path"

  ### Erlang

  ```erlang
  ?CODE_FILE_PATH.
  'code.file.path'
  ```

  <!-- tabs-close -->
  """
  @spec code_file_path :: :"code.file.path"
  def code_file_path do
    :"code.file.path"
  end

  @doc """
  The method or function fully-qualified name without arguments. The value should fit the natural representation of the language runtime, which is also likely the same used within `code.stacktrace` attribute value. This attribute **MUST** **NOT** be used on the Profile signal since the data is already captured in 'message Function'. This constraint is imposed to prevent redundancy and maintain data integrity.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Values and format depends on each language runtime, thus it is impossible to provide an exhaustive list of examples.
  The values are usually the same (or prefixes of) the ones found in native stack trace representation stored in
  `code.stacktrace` without information on arguments.

  Examples:

  * Java method: `com.example.MyHttpService.serveRequest`
  * Java anonymous class method: `com.mycompany.Main$1.myMethod`
  * Java lambda method: `com.mycompany.Main$$Lambda/0x0000748ae4149c00.myMethod`
  * PHP function: `GuzzleHttp\Client::transfer`
  * Go function: `github.com/my/repo/pkg.foo.func5`
  * Elixir: `OpenTelemetry.Ctx.new`
  * Erlang: `opentelemetry_ctx:new`
  * Rust: `playground::my_module::my_cool_func`
  * C function: `fopen`

  ### Examples

  ```
  ["com.example.MyHttpService.serveRequest", "GuzzleHttp\\Client::transfer", "fopen"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.CodeAttributes.code_function_name()
      :"code.function.name"

  ### Erlang

  ```erlang
  ?CODE_FUNCTION_NAME.
  'code.function.name'
  ```

  <!-- tabs-close -->
  """
  @spec code_function_name :: :"code.function.name"
  def code_function_name do
    :"code.function.name"
  end

  @doc """
  The line number in `code.file.path` best representing the operation. It **SHOULD** point within the code unit named in `code.function.name`. This attribute **MUST** **NOT** be used on the Profile signal since the data is already captured in 'message Line'. This constraint is imposed to prevent redundancy and maintain data integrity.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  42
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.CodeAttributes.code_line_number()
      :"code.line.number"

  ### Erlang

  ```erlang
  ?CODE_LINE_NUMBER.
  'code.line.number'
  ```

  <!-- tabs-close -->
  """
  @spec code_line_number :: :"code.line.number"
  def code_line_number do
    :"code.line.number"
  end

  @doc """
  A stacktrace as a string in the natural representation for the language runtime. The representation is identical to [`exception.stacktrace`](/docs/exceptions/exceptions-spans.md#stacktrace-representation). This attribute **MUST** **NOT** be used on the Profile signal since the data is already captured in 'message Location'. This constraint is imposed to prevent redundancy and maintain data integrity.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  at com.example.GenerateTrace.methodB(GenerateTrace.java:13)\n at com.example.GenerateTrace.methodA(GenerateTrace.java:9)\n at com.example.GenerateTrace.main(GenerateTrace.java:5)

  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.CodeAttributes.code_stacktrace()
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
