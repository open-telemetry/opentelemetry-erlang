defmodule OpenTelemetry.SemConv.Incubating.CassandraAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Cassandra attributes.
  """

  @typedoc """
  The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html).


  ### Enum Values
  * `:all` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:each_quorum` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:quorum` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:local_quorum` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:one` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:two` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:three` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:local_one` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:any` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:serial` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:local_serial` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type cassandra_consistency_level_values() :: %{
          :all => :all,
          :each_quorum => :each_quorum,
          :quorum => :quorum,
          :local_quorum => :local_quorum,
          :one => :one,
          :two => :two,
          :three => :three,
          :local_one => :local_one,
          :any => :any,
          :serial => :serial,
          :local_serial => :local_serial
        }
  @doc """
  The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html).



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CassandraAttributes.cassandra_consistency_level()
      :"cassandra.consistency.level"

      iex> OpenTelemetry.SemConv.Incubating.CassandraAttributes.cassandra_consistency_level_values().all
      :all

      iex> %{OpenTelemetry.SemConv.Incubating.CassandraAttributes.cassandra_consistency_level() => OpenTelemetry.SemConv.Incubating.CassandraAttributes.cassandra_consistency_level_values().all}
      %{:"cassandra.consistency.level" => :all}

  ### Erlang

  ```erlang
  ?CASSANDRA_CONSISTENCY_LEVEL.
  'cassandra.consistency.level'

  ?CASSANDRA_CONSISTENCY_LEVEL_VALUES_ALL.
  'all'

  \#{?CASSANDRA_CONSISTENCY_LEVEL => ?CASSANDRA_CONSISTENCY_LEVEL_VALUES_ALL}.
  \#{'cassandra.consistency.level' => 'all'}
  ```

  <!-- tabs-close -->
  """
  @spec cassandra_consistency_level :: :"cassandra.consistency.level"
  def cassandra_consistency_level do
    :"cassandra.consistency.level"
  end

  @spec cassandra_consistency_level_values() :: cassandra_consistency_level_values()
  def cassandra_consistency_level_values() do
    %{
      :all => :all,
      :each_quorum => :each_quorum,
      :quorum => :quorum,
      :local_quorum => :local_quorum,
      :one => :one,
      :two => :two,
      :three => :three,
      :local_one => :local_one,
      :any => :any,
      :serial => :serial,
      :local_serial => :local_serial
    }
  end

  @doc """
  The data center of the coordinating node for a query.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  us-west-2
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CassandraAttributes.cassandra_coordinator_dc()
      :"cassandra.coordinator.dc"

  ### Erlang

  ```erlang
  ?CASSANDRA_COORDINATOR_DC.
  'cassandra.coordinator.dc'
  ```

  <!-- tabs-close -->
  """
  @spec cassandra_coordinator_dc :: :"cassandra.coordinator.dc"
  def cassandra_coordinator_dc do
    :"cassandra.coordinator.dc"
  end

  @doc """
  The ID of the coordinating node for a query.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  be13faa2-8574-4d71-926d-27f16cf8a7af
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CassandraAttributes.cassandra_coordinator_id()
      :"cassandra.coordinator.id"

  ### Erlang

  ```erlang
  ?CASSANDRA_COORDINATOR_ID.
  'cassandra.coordinator.id'
  ```

  <!-- tabs-close -->
  """
  @spec cassandra_coordinator_id :: :"cassandra.coordinator.id"
  def cassandra_coordinator_id do
    :"cassandra.coordinator.id"
  end

  @doc """
  The fetch size used for paging, i.e. how many rows will be returned at once.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [5000]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CassandraAttributes.cassandra_page_size()
      :"cassandra.page.size"

  ### Erlang

  ```erlang
  ?CASSANDRA_PAGE_SIZE.
  'cassandra.page.size'
  ```

  <!-- tabs-close -->
  """
  @spec cassandra_page_size :: :"cassandra.page.size"
  def cassandra_page_size do
    :"cassandra.page.size"
  end

  @doc """
  Whether or not the query is idempotent.

  ### Value type

  Value must be of type `boolean()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CassandraAttributes.cassandra_query_idempotent()
      :"cassandra.query.idempotent"

  ### Erlang

  ```erlang
  ?CASSANDRA_QUERY_IDEMPOTENT.
  'cassandra.query.idempotent'
  ```

  <!-- tabs-close -->
  """
  @spec cassandra_query_idempotent :: :"cassandra.query.idempotent"
  def cassandra_query_idempotent do
    :"cassandra.query.idempotent"
  end

  @doc """
  The number of times a query was speculatively executed. Not set or `0` if the query was not executed speculatively.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [0, 2]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CassandraAttributes.cassandra_speculative_execution_count()
      :"cassandra.speculative_execution.count"

  ### Erlang

  ```erlang
  ?CASSANDRA_SPECULATIVE_EXECUTION_COUNT.
  'cassandra.speculative_execution.count'
  ```

  <!-- tabs-close -->
  """
  @spec cassandra_speculative_execution_count :: :"cassandra.speculative_execution.count"
  def cassandra_speculative_execution_count do
    :"cassandra.speculative_execution.count"
  end
end
