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
  @type graphql_operation_type() :: %{
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

      iex> OpenTelemetry.SemConv.Incubating.GraphqlAttributes.graphql_operation_type().query
      :query
      
      iex> OpenTelemetry.SemConv.Incubating.GraphqlAttributes.graphql_operation_type(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'graphql_operation_type.query'.
  query

  ?graphql_operation_type(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec graphql_operation_type() :: graphql_operation_type()
  def graphql_operation_type() do
    %{
      :query => :query,
      :mutation => :mutation,
      :subscription => :subscription
    }
  end

  @spec graphql_operation_type(atom() | String.t()) :: atom() | String.t()
  def graphql_operation_type(custom_value) do
    custom_value
  end
end
