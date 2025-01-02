defmodule OpenTelemetry.SemConv.Incubating.FileAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for File attributes.
  """

  @doc """
  Directory where the file is located. It should include the drive letter, when appropriate.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["/home/user", "C:\\Program Files\\MyApp"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_directory()
      :"file.directory"

  ### Erlang

  ```erlang
  ?FILE_DIRECTORY.
  'file.directory'
  ```

  <!-- tabs-close -->
  """
  @spec file_directory :: :"file.directory"
  def file_directory do
    :"file.directory"
  end

  @doc """
  File extension, excluding the leading dot.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  When the file name has multiple extensions (example.tar.gz), only the last one should be captured ("gz", not "tar.gz").

  ### Examples

  ```
  ["png", "gz"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_extension()
      :"file.extension"

  ### Erlang

  ```erlang
  ?FILE_EXTENSION.
  'file.extension'
  ```

  <!-- tabs-close -->
  """
  @spec file_extension :: :"file.extension"
  def file_extension do
    :"file.extension"
  end

  @doc """
  Name of the file including the extension, without the directory.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["example.png"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_name()
      :"file.name"

  ### Erlang

  ```erlang
  ?FILE_NAME.
  'file.name'
  ```

  <!-- tabs-close -->
  """
  @spec file_name :: :"file.name"
  def file_name do
    :"file.name"
  end

  @doc """
  Full path to the file, including the file name. It should include the drive letter, when appropriate.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["/home/alice/example.png", "C:\\Program Files\\MyApp\\myapp.exe"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_path()
      :"file.path"

  ### Erlang

  ```erlang
  ?FILE_PATH.
  'file.path'
  ```

  <!-- tabs-close -->
  """
  @spec file_path :: :"file.path"
  def file_path do
    :"file.path"
  end

  @doc """
  File size in bytes.

  ### Value type

  Value must be of type `integer()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_size()
      :"file.size"

  ### Erlang

  ```erlang
  ?FILE_SIZE.
  'file.size'
  ```

  <!-- tabs-close -->
  """
  @spec file_size :: :"file.size"
  def file_size do
    :"file.size"
  end
end
