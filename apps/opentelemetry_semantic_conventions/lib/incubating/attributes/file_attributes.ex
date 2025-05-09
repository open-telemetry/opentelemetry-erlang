defmodule OpenTelemetry.SemConv.Incubating.FileAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for File attributes.
  """

  @doc """
  Time when the file was last accessed, in ISO 8601 format.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This attribute might not be supported by some file systems — NFS, FAT32, in embedded OS, etc.

  ### Examples

  ```
  ["2021-01-01T12:00:00Z"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_accessed()
      :"file.accessed"

  ### Erlang

  ```erlang
  ?FILE_ACCESSED.
  'file.accessed'
  ```

  <!-- tabs-close -->
  """
  @spec file_accessed :: :"file.accessed"
  def file_accessed do
    :"file.accessed"
  end

  @doc """
  Array of file attributes.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  Attributes names depend on the OS or file system. Here’s a non-exhaustive list of values expected for this attribute: `archive`, `compressed`, `directory`, `encrypted`, `execute`, `hidden`, `immutable`, `journaled`, `read`, `readonly`, `symbolic link`, `system`, `temporary`, `write`.

  ### Examples

  ```
  [["readonly", "hidden"]]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_attributes()
      :"file.attributes"

  ### Erlang

  ```erlang
  ?FILE_ATTRIBUTES.
  'file.attributes'
  ```

  <!-- tabs-close -->
  """
  @spec file_attributes :: :"file.attributes"
  def file_attributes do
    :"file.attributes"
  end

  @doc """
  Time when the file attributes or metadata was last changed, in ISO 8601 format.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  `file.changed` captures the time when any of the file's properties or attributes (including the content) are changed, while `file.modified` captures the timestamp when the file content is modified.

  ### Examples

  ```
  ["2021-01-01T12:00:00Z"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_changed()
      :"file.changed"

  ### Erlang

  ```erlang
  ?FILE_CHANGED.
  'file.changed'
  ```

  <!-- tabs-close -->
  """
  @spec file_changed :: :"file.changed"
  def file_changed do
    :"file.changed"
  end

  @doc """
  Time when the file was created, in ISO 8601 format.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This attribute might not be supported by some file systems — NFS, FAT32, in embedded OS, etc.

  ### Examples

  ```
  ["2021-01-01T12:00:00Z"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_created()
      :"file.created"

  ### Erlang

  ```erlang
  ?FILE_CREATED.
  'file.created'
  ```

  <!-- tabs-close -->
  """
  @spec file_created :: :"file.created"
  def file_created do
    :"file.created"
  end

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
  Name of the fork. A fork is additional data associated with a filesystem object.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  On Linux, a resource fork is used to store additional data with a filesystem object. A file always has at least one fork for the data portion, and additional forks may exist.
  On NTFS, this is analogous to an Alternate Data Stream (ADS), and the default data stream for a file is just called $DATA. Zone.Identifier is commonly used by Windows to track contents downloaded from the Internet. An ADS is typically of the form: C:\path\to\filename.extension:some_fork_name, and some_fork_name is the value that should populate `fork_name`. `filename.extension` should populate `file.name`, and `extension` should populate `file.extension`. The full path, `file.path`, will include the fork name.

  ### Examples

  ```
  ["Zone.Identifer"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_fork_name()
      :"file.fork_name"

  ### Erlang

  ```erlang
  ?FILE_FORK_NAME.
  'file.fork_name'
  ```

  <!-- tabs-close -->
  """
  @spec file_fork_name :: :"file.fork_name"
  def file_fork_name do
    :"file.fork_name"
  end

  @doc """
  Primary Group ID (GID) of the file.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1000"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_group_id()
      :"file.group.id"

  ### Erlang

  ```erlang
  ?FILE_GROUP_ID.
  'file.group.id'
  ```

  <!-- tabs-close -->
  """
  @spec file_group_id :: :"file.group.id"
  def file_group_id do
    :"file.group.id"
  end

  @doc """
  Primary group name of the file.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["users"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_group_name()
      :"file.group.name"

  ### Erlang

  ```erlang
  ?FILE_GROUP_NAME.
  'file.group.name'
  ```

  <!-- tabs-close -->
  """
  @spec file_group_name :: :"file.group.name"
  def file_group_name do
    :"file.group.name"
  end

  @doc """
  Inode representing the file in the filesystem.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["256383"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_inode()
      :"file.inode"

  ### Erlang

  ```erlang
  ?FILE_INODE.
  'file.inode'
  ```

  <!-- tabs-close -->
  """
  @spec file_inode :: :"file.inode"
  def file_inode do
    :"file.inode"
  end

  @doc """
  Mode of the file in octal representation.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["0640"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_mode()
      :"file.mode"

  ### Erlang

  ```erlang
  ?FILE_MODE.
  'file.mode'
  ```

  <!-- tabs-close -->
  """
  @spec file_mode :: :"file.mode"
  def file_mode do
    :"file.mode"
  end

  @doc """
  Time when the file content was last modified, in ISO 8601 format.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["2021-01-01T12:00:00Z"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_modified()
      :"file.modified"

  ### Erlang

  ```erlang
  ?FILE_MODIFIED.
  'file.modified'
  ```

  <!-- tabs-close -->
  """
  @spec file_modified :: :"file.modified"
  def file_modified do
    :"file.modified"
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
  The user ID (UID) or security identifier (SID) of the file owner.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1000"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_owner_id()
      :"file.owner.id"

  ### Erlang

  ```erlang
  ?FILE_OWNER_ID.
  'file.owner.id'
  ```

  <!-- tabs-close -->
  """
  @spec file_owner_id :: :"file.owner.id"
  def file_owner_id do
    :"file.owner.id"
  end

  @doc """
  Username of the file owner.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["root"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_owner_name()
      :"file.owner.name"

  ### Erlang

  ```erlang
  ?FILE_OWNER_NAME.
  'file.owner.name'
  ```

  <!-- tabs-close -->
  """
  @spec file_owner_name :: :"file.owner.name"
  def file_owner_name do
    :"file.owner.name"
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

  @doc """
  Path to the target of a symbolic link.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This attribute is only applicable to symbolic links.

  ### Examples

  ```
  ["/usr/bin/python3"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FileAttributes.file_symbolic_link_target_path()
      :"file.symbolic_link.target_path"

  ### Erlang

  ```erlang
  ?FILE_SYMBOLIC_LINK_TARGET_PATH.
  'file.symbolic_link.target_path'
  ```

  <!-- tabs-close -->
  """
  @spec file_symbolic_link_target_path :: :"file.symbolic_link.target_path"
  def file_symbolic_link_target_path do
    :"file.symbolic_link.target_path"
  end
end
