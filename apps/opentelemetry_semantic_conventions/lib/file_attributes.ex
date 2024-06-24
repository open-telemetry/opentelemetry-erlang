defmodule OpenTelemetry.SemanticConventions.FileAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for File attributes.
  """

  @doc """
  Directory where the file is located. It should include the drive letter, when appropriate.



  ### Example
      iex> OpenTelemetry.SemanticConventions.FileAttributes.file_directory()
      :"file.directory"
  """
  @spec file_directory :: :"file.directory"
  def file_directory do
    :"file.directory"
  end

  @doc """
  File extension, excluding the leading dot.

  ### Notes

  When the file name has multiple extensions (example.tar.gz), only the last one should be captured ("gz", not "tar.gz").


  ### Example
      iex> OpenTelemetry.SemanticConventions.FileAttributes.file_extension()
      :"file.extension"
  """
  @spec file_extension :: :"file.extension"
  def file_extension do
    :"file.extension"
  end

  @doc """
  Name of the file including the extension, without the directory.



  ### Example
      iex> OpenTelemetry.SemanticConventions.FileAttributes.file_name()
      :"file.name"
  """
  @spec file_name :: :"file.name"
  def file_name do
    :"file.name"
  end

  @doc """
  Full path to the file, including the file name. It should include the drive letter, when appropriate.



  ### Example
      iex> OpenTelemetry.SemanticConventions.FileAttributes.file_path()
      :"file.path"
  """
  @spec file_path :: :"file.path"
  def file_path do
    :"file.path"
  end

  @doc """
  File size in bytes.



  ### Example
      iex> OpenTelemetry.SemanticConventions.FileAttributes.file_size()
      :"file.size"
  """
  @spec file_size :: :"file.size"
  def file_size do
    :"file.size"
  end
end
