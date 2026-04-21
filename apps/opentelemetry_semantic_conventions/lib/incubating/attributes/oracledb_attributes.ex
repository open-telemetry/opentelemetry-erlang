defmodule OpenTelemetry.SemConv.Incubating.OracledbAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Oracledb attributes.
  """

  @doc """
  The database domain associated with the connection.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This attribute **SHOULD** be set to the value of the `DB_DOMAIN` initialization parameter,
  as exposed in `v$parameter`. `DB_DOMAIN` defines the domain portion of the global
  database name and **SHOULD** be configured when a database is, or may become, part of a
  distributed environment. Its value consists of one or more valid identifiers
  (alphanumeric ASCII characters) separated by periods.

  ### Examples

  ```
  ["example.com", "corp.internal", "prod.db.local"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OracledbAttributes.oracle_db_domain()
      :"oracle.db.domain"

  ### Erlang

  ```erlang
  ?ORACLE_DB_DOMAIN.
  'oracle.db.domain'
  ```

  <!-- tabs-close -->
  """
  @spec oracle_db_domain :: :"oracle.db.domain"
  def oracle_db_domain do
    :"oracle.db.domain"
  end

  @doc """
  The instance name associated with the connection in an Oracle Real Application Clusters environment.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  There can be multiple instances associated with a single database service. It indicates the
  unique instance name to which the connection is currently bound. For non-RAC databases, this value
  defaults to the `oracle.db.name`.

  ### Examples

  ```
  ["ORCL1", "ORCL2", "ORCL3"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OracledbAttributes.oracle_db_instance_name()
      :"oracle.db.instance.name"

  ### Erlang

  ```erlang
  ?ORACLE_DB_INSTANCE_NAME.
  'oracle.db.instance.name'
  ```

  <!-- tabs-close -->
  """
  @spec oracle_db_instance_name :: :"oracle.db.instance.name"
  def oracle_db_instance_name do
    :"oracle.db.instance.name"
  end

  @doc """
  The database name associated with the connection.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This attribute **SHOULD** be set to the value of the parameter `DB_NAME` exposed in `v$parameter`.

  ### Examples

  ```
  ["ORCL1", "FREE"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OracledbAttributes.oracle_db_name()
      :"oracle.db.name"

  ### Erlang

  ```erlang
  ?ORACLE_DB_NAME.
  'oracle.db.name'
  ```

  <!-- tabs-close -->
  """
  @spec oracle_db_name :: :"oracle.db.name"
  def oracle_db_name do
    :"oracle.db.name"
  end

  @doc """
  The pluggable database (PDB) name associated with the connection.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This attribute **SHOULD** reflect the PDB that the session is currently connected to.
  If instrumentation cannot reliably obtain the active PDB name for each operation
  without issuing an additional query (such as `SELECT SYS_CONTEXT`), it is
  RECOMMENDED to fall back to the PDB name specified at connection establishment.

  ### Examples

  ```
  ["PDB1", "FREEPDB"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OracledbAttributes.oracle_db_pdb()
      :"oracle.db.pdb"

  ### Erlang

  ```erlang
  ?ORACLE_DB_PDB.
  'oracle.db.pdb'
  ```

  <!-- tabs-close -->
  """
  @spec oracle_db_pdb :: :"oracle.db.pdb"
  def oracle_db_pdb do
    :"oracle.db.pdb"
  end

  @doc """
  The service name currently associated with the database connection.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The effective service name for a connection can change during its lifetime,
  for example after executing sql, `ALTER SESSION`. If an instrumentation cannot reliably
  obtain the current service name for each operation without issuing an additional
  query (such as `SELECT SYS_CONTEXT`), it is RECOMMENDED to fall back to the
  service name originally provided at connection establishment.

  ### Examples

  ```
  ["order-processing-service", "db_low.adb.oraclecloud.com", "db_high.adb.oraclecloud.com"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OracledbAttributes.oracle_db_service()
      :"oracle.db.service"

  ### Erlang

  ```erlang
  ?ORACLE_DB_SERVICE.
  'oracle.db.service'
  ```

  <!-- tabs-close -->
  """
  @spec oracle_db_service :: :"oracle.db.service"
  def oracle_db_service do
    :"oracle.db.service"
  end
end
