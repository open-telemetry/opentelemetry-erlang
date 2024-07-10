defmodule OpenTelemetry.SemConv.Incubating.LogAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Log attributes.
  """

  @doc """
  The basename of the file.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["audit.log"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.LogAttributes.log_file_name()
      :"log.file.name"

  ### Erlang

  ```erlang
  ?LOG_FILE_NAME.
  'log.file.name'
  ```

  <!-- tabs-close -->
  """
  @spec log_file_name :: :"log.file.name"
  def log_file_name do
    :"log.file.name"
  end

  @doc """
  The basename of the file, with symlinks resolved.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["uuid.log"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.LogAttributes.log_file_nameresolved()
      :"log.file.name_resolved"

  ### Erlang

  ```erlang
  ?LOG_FILE_NAMERESOLVED.
  'log.file.name_resolved'
  ```

  <!-- tabs-close -->
  """
  @spec log_file_nameresolved :: :"log.file.name_resolved"
  def log_file_nameresolved do
    :"log.file.name_resolved"
  end

  @doc """
  The full path to the file.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["/var/log/mysql/audit.log"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.LogAttributes.log_file_path()
      :"log.file.path"

  ### Erlang

  ```erlang
  ?LOG_FILE_PATH.
  'log.file.path'
  ```

  <!-- tabs-close -->
  """
  @spec log_file_path :: :"log.file.path"
  def log_file_path do
    :"log.file.path"
  end

  @doc """
  The full path to the file, with symlinks resolved.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["/var/lib/docker/uuid.log"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.LogAttributes.log_file_pathresolved()
      :"log.file.path_resolved"

  ### Erlang

  ```erlang
  ?LOG_FILE_PATHRESOLVED.
  'log.file.path_resolved'
  ```

  <!-- tabs-close -->
  """
  @spec log_file_pathresolved :: :"log.file.path_resolved"
  def log_file_pathresolved do
    :"log.file.path_resolved"
  end

  @typedoc """
  The stream associated with the log. See below for a list of well-known values.


  ### Enum Values
  * `:stdout` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Logs from stdout stream
  * `:stderr` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Events from stderr stream
  """
  @type log_iostream() :: %{
          :stdout => :stdout,
          :stderr => :stderr
        }
  @doc """
  The stream associated with the log. See below for a list of well-known values.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.LogAttributes.log_iostream().stdout
      :stdout
      
      iex> OpenTelemetry.SemConv.Incubating.LogAttributes.log_iostream(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'log_iostream.stdout'.
  stdout

  ?log_iostream.(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec log_iostream() :: log_iostream()
  def log_iostream() do
    %{
      :stdout => :stdout,
      :stderr => :stderr
    }
  end

  @spec log_iostream(atom() | String.t()) :: atom() | String.t()
  def log_iostream(custom_value) do
    custom_value
  end

  @doc """
  A unique identifier for the Log Record.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  If an id is provided, other log records with the same id will be considered duplicates and can be removed safely. This means, that two distinguishable log records **MUST** have different values.
  The id **MAY** be an [Universally Unique Lexicographically Sortable Identifier (ULID)](https://github.com/ulid/spec), but other identifiers (e.g. UUID) may be used as needed.

  ### Examples

  ```
  ["01ARZ3NDEKTSV4RRFFQ69G5FAV"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.LogAttributes.log_record_uid()
      :"log.record.uid"

  ### Erlang

  ```erlang
  ?LOG_RECORD_UID.
  'log.record.uid'
  ```

  <!-- tabs-close -->
  """
  @spec log_record_uid :: :"log.record.uid"
  def log_record_uid do
    :"log.record.uid"
  end
end
