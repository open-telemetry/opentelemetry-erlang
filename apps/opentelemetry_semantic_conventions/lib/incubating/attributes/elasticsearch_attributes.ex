defmodule OpenTelemetry.SemConv.Incubating.ElasticsearchAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Elasticsearch attributes.
  """

  @doc """
  Represents the human-readable identifier of the node/instance to which a request was routed.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["instance-0000000001"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ElasticsearchAttributes.elasticsearch_node_name()
      :"elasticsearch.node.name"

  ### Erlang

  ```erlang
  ?ELASTICSEARCH_NODE_NAME.
  'elasticsearch.node.name'
  ```

  <!-- tabs-close -->
  """
  @spec elasticsearch_node_name :: :"elasticsearch.node.name"
  def elasticsearch_node_name do
    :"elasticsearch.node.name"
  end
end
