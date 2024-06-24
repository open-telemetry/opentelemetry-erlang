defmodule OpenTelemetry.SemanticConventions.LogAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Log attributes.
  """

  @doc """
  The basename of the file.



  ### Example
      iex> OpenTelemetry.SemanticConventions.LogAttributes.log_file_name()
      :"log.file.name"
  """
  @spec log_file_name :: :"log.file.name"
  def log_file_name do
    :"log.file.name"
  end

  @doc """
  The basename of the file, with symlinks resolved.



  ### Example
      iex> OpenTelemetry.SemanticConventions.LogAttributes.log_file_nameresolved()
      :"log.file.name_resolved"
  """
  @spec log_file_nameresolved :: :"log.file.name_resolved"
  def log_file_nameresolved do
    :"log.file.name_resolved"
  end

  @doc """
  The full path to the file.



  ### Example
      iex> OpenTelemetry.SemanticConventions.LogAttributes.log_file_path()
      :"log.file.path"
  """
  @spec log_file_path :: :"log.file.path"
  def log_file_path do
    :"log.file.path"
  end

  @doc """
  The full path to the file, with symlinks resolved.



  ### Example
      iex> OpenTelemetry.SemanticConventions.LogAttributes.log_file_pathresolved()
      :"log.file.path_resolved"
  """
  @spec log_file_pathresolved :: :"log.file.path_resolved"
  def log_file_pathresolved do
    :"log.file.path_resolved"
  end

  @typedoc """
  The stream associated with the log. See below for a list of well-known values.


  ### Options
  * `:stdout` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Logs from stdout stream
  * `:stderr` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Events from stderr stream

  """
  @type log_iostream() :: :stdout | :stderr | atom()

  @doc """
  The stream associated with the log. See below for a list of well-known values.



  ### Example
      iex> OpenTelemetry.SemanticConventions.LogAttributes.log_iostream(:stdout)
      :stdout
      
      iex> OpenTelemetry.SemanticConventions.LogAttributes.log_iostream(:custom_value)
      :custom_value
  """
  @spec log_iostream(log_iostream()) :: :stdout | :stderr | atom()
  def log_iostream(option) do
    case option do
      :stdout -> :stdout
      :stderr -> :stderr
      _ -> option
    end
  end

  @doc """
  A unique identifier for the Log Record.

  ### Notes

  If an id is provided, other log records with the same id will be considered duplicates and can be removed safely. This means, that two distinguishable log records **MUST** have different values.
  The id **MAY** be an [Universally Unique Lexicographically Sortable Identifier (ULID)](https://github.com/ulid/spec), but other identifiers (e.g. UUID) may be used as needed.


  ### Example
      iex> OpenTelemetry.SemanticConventions.LogAttributes.log_record_uid()
      :"log.record.uid"
  """
  @spec log_record_uid :: :"log.record.uid"
  def log_record_uid do
    :"log.record.uid"
  end
end
