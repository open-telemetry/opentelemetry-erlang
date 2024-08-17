defmodule OpenTelemetry.SemConv.Incubating.GraphqlAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Graphql attributes.
  """

  @doc """
  The GraphQL document being executed.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The value may be sanitized to exclude sensitive information.
  ### Examples

  ```
  query findBookById { bookById(id: ?) { name } }
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GraphqlAttributes.graphql_document()
      :"graphql.document"

  ### Erlang

  ```erlang
  ?GRAPHQL_DOCUMENT.
  'graphql.document'
  ```

  <!-- tabs-close -->
  """
  @spec graphql_document :: :"graphql.document"
  def graphql_document do
    :"graphql.document"
  end

  @doc """
  The name of the operation being executed.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  findBookById
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GraphqlAttributes.graphql_operation_name()
      :"graphql.operation.name"

  ### Erlang

  ```erlang
  ?GRAPHQL_OPERATION_NAME.
  'graphql.operation.name'
  ```

  <!-- tabs-close -->
  """
  @spec graphql_operation_name :: :"graphql.operation.name"
  def graphql_operation_name do
    :"graphql.operation.name"
  end

  @typedoc """
  The type of the operation being executed.

  ### Enum Values
  * `:query` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - GraphQL query
  * `:mutation` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - GraphQL mutation
  * `:subscription` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - GraphQL subscription
  """
  @type graphql_operation_type_values() :: %{
          :query => :query,
          :mutation => :mutation,
          :subscription => :subscription
        }
  @doc """
  The type of the operation being executed.

  ### Examples

  ```
  ["query", "mutation", "subscription"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GraphqlAttributes.graphql_operation_type()
      :"graphql.operation.type"

      iex> OpenTelemetry.SemConv.Incubating.GraphqlAttributes.graphql_operation_type_values().query
      :query

      iex> %{OpenTelemetry.SemConv.Incubating.GraphqlAttributes.graphql_operation_type() => OpenTelemetry.SemConv.Incubating.GraphqlAttributes.graphql_operation_type_values().query}
      %{:"graphql.operation.type" => :query}

  ### Erlang

  ```erlang
  ?GRAPHQL_OPERATION_TYPE.
  'graphql.operation.type'

  ?GRAPHQL_OPERATION_TYPE_VALUES_QUERY.
  'query'

  \#{?GRAPHQL_OPERATION_TYPE => ?GRAPHQL_OPERATION_TYPE_VALUES_QUERY}.
  \#{'graphql.operation.type' => 'query'}
  ```

  <!-- tabs-close -->
  """
  @spec graphql_operation_type :: :"graphql.operation.type"
  def graphql_operation_type do
    :"graphql.operation.type"
  end

  @spec graphql_operation_type_values() :: graphql_operation_type_values()
  def graphql_operation_type_values() do
    %{
      :query => :query,
      :mutation => :mutation,
      :subscription => :subscription
    }
  end
end
