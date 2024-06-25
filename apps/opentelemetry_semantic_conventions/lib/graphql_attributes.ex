defmodule OpenTelemetry.SemanticConventions.GraphqlAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Graphql attributes.
  """

  @doc """
  The GraphQL document being executed.
  ### Notes

  The value may be sanitized to exclude sensitive information.

  ### Example
      iex> OpenTelemetry.SemanticConventions.GraphqlAttributes.graphql_document()
      :"graphql.document"
  """
  @spec graphql_document :: :"graphql.document"
  def graphql_document do
    :"graphql.document"
  end

  @doc """
  The name of the operation being executed.


  ### Example
      iex> OpenTelemetry.SemanticConventions.GraphqlAttributes.graphql_operation_name()
      :"graphql.operation.name"
  """
  @spec graphql_operation_name :: :"graphql.operation.name"
  def graphql_operation_name do
    :"graphql.operation.name"
  end

  @typedoc """
  The type of the operation being executed.

  ### Enum Values
  * `:query` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - GraphQL query
  * `:mutation` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - GraphQL mutation
  * `:subscription` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - GraphQL subscription
  """
  @type graphql_operation_type() :: %{
          :query => :query,
          :mutation => :mutation,
          :subscription => :subscription
        }
  @doc """
  The type of the operation being executed.


  ### Example
      iex> OpenTelemetry.SemanticConventions.GraphqlAttributes.graphql_operation_type().query
      :query
      
      iex> OpenTelemetry.SemanticConventions.GraphqlAttributes.graphql_operation_type(:custom_value)
      :custom_value
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
