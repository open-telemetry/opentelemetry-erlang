defmodule OpenTelemetry.SemConv.Incubating.McpAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Mcp attributes.
  """

  @typedoc """
  The name of the request or notification method.

  ### Enum Values
  * `:notifications_cancelled` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Notification cancelling a previously-issued request.

  * `:initialize` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to initialize the MCP client.

  * `:notifications_initialized` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Notification indicating that the MCP client has been initialized.

  * `:notifications_progress` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Notification indicating the progress for a long-running operation.

  * `:ping` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to check that the other party is still alive.

  * `:resources_list` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to list resources available on server.

  * `:resources_templates_list` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to list resource templates available on server.

  * `:resources_read` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to read a resource.

  * `:notifications_resources_list_changed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Notification indicating that the list of resources has changed.

  * `:resources_subscribe` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to subscribe to a resource.

  * `:resources_unsubscribe` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to unsubscribe from resource updates.

  * `:notifications_resources_updated` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Notification indicating that a resource has been updated.

  * `:prompts_list` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to list prompts available on server.

  * `:prompts_get` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to get a prompt.

  * `:notifications_prompts_list_changed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Notification indicating that the list of prompts has changed.

  * `:tools_list` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to list tools available on server.

  * `:tools_call` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to call a tool.

  * `:notifications_tools_list_changed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Notification indicating that the list of tools has changed.

  * `:logging_set_level` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to set the logging level.

  * `:notifications_message` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Notification indicating that a message has been received.

  * `:sampling_create_message` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to create a sampling message.

  * `:completion_complete` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to complete a prompt.

  * `:roots_list` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request to list roots available on server.

  * `:notifications_roots_list_changed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Notification indicating that the list of roots has changed.

  * `:elicitation_create` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Request from the server to elicit additional information from the user via the client

  """
  @type mcp_method_name_values() :: %{
          :notifications_cancelled => :"notifications/cancelled",
          :initialize => :initialize,
          :notifications_initialized => :"notifications/initialized",
          :notifications_progress => :"notifications/progress",
          :ping => :ping,
          :resources_list => :"resources/list",
          :resources_templates_list => :"resources/templates/list",
          :resources_read => :"resources/read",
          :notifications_resources_list_changed => :"notifications/resources/list_changed",
          :resources_subscribe => :"resources/subscribe",
          :resources_unsubscribe => :"resources/unsubscribe",
          :notifications_resources_updated => :"notifications/resources/updated",
          :prompts_list => :"prompts/list",
          :prompts_get => :"prompts/get",
          :notifications_prompts_list_changed => :"notifications/prompts/list_changed",
          :tools_list => :"tools/list",
          :tools_call => :"tools/call",
          :notifications_tools_list_changed => :"notifications/tools/list_changed",
          :logging_set_level => :"logging/setLevel",
          :notifications_message => :"notifications/message",
          :sampling_create_message => :"sampling/createMessage",
          :completion_complete => :"completion/complete",
          :roots_list => :"roots/list",
          :notifications_roots_list_changed => :"notifications/roots/list_changed",
          :elicitation_create => :"elicitation/create"
        }
  @doc """
  The name of the request or notification method.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.McpAttributes.mcp_method_name()
      :"mcp.method.name"

      iex> OpenTelemetry.SemConv.Incubating.McpAttributes.mcp_method_name_values().notifications_cancelled
      :"notifications/cancelled"

      iex> %{OpenTelemetry.SemConv.Incubating.McpAttributes.mcp_method_name() => OpenTelemetry.SemConv.Incubating.McpAttributes.mcp_method_name_values().notifications_cancelled}
      %{:"mcp.method.name" => :"notifications/cancelled"}

  ### Erlang

  ```erlang
  ?MCP_METHOD_NAME.
  'mcp.method.name'

  ?MCP_METHOD_NAME_VALUES_NOTIFICATIONS_CANCELLED.
  'notifications/cancelled'

  \#{?MCP_METHOD_NAME => ?MCP_METHOD_NAME_VALUES_NOTIFICATIONS_CANCELLED}.
  \#{'mcp.method.name' => 'notifications/cancelled'}
  ```

  <!-- tabs-close -->
  """
  @spec mcp_method_name :: :"mcp.method.name"
  def mcp_method_name do
    :"mcp.method.name"
  end

  @spec mcp_method_name_values() :: mcp_method_name_values()
  def mcp_method_name_values() do
    %{
      :notifications_cancelled => :"notifications/cancelled",
      :initialize => :initialize,
      :notifications_initialized => :"notifications/initialized",
      :notifications_progress => :"notifications/progress",
      :ping => :ping,
      :resources_list => :"resources/list",
      :resources_templates_list => :"resources/templates/list",
      :resources_read => :"resources/read",
      :notifications_resources_list_changed => :"notifications/resources/list_changed",
      :resources_subscribe => :"resources/subscribe",
      :resources_unsubscribe => :"resources/unsubscribe",
      :notifications_resources_updated => :"notifications/resources/updated",
      :prompts_list => :"prompts/list",
      :prompts_get => :"prompts/get",
      :notifications_prompts_list_changed => :"notifications/prompts/list_changed",
      :tools_list => :"tools/list",
      :tools_call => :"tools/call",
      :notifications_tools_list_changed => :"notifications/tools/list_changed",
      :logging_set_level => :"logging/setLevel",
      :notifications_message => :"notifications/message",
      :sampling_create_message => :"sampling/createMessage",
      :completion_complete => :"completion/complete",
      :roots_list => :"roots/list",
      :notifications_roots_list_changed => :"notifications/roots/list_changed",
      :elicitation_create => :"elicitation/create"
    }
  end

  @doc """
  The [version](https://modelcontextprotocol.io/specification/versioning) of the Model Context Protocol used.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["2025-06-18"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.McpAttributes.mcp_protocol_version()
      :"mcp.protocol.version"

  ### Erlang

  ```erlang
  ?MCP_PROTOCOL_VERSION.
  'mcp.protocol.version'
  ```

  <!-- tabs-close -->
  """
  @spec mcp_protocol_version :: :"mcp.protocol.version"
  def mcp_protocol_version do
    :"mcp.protocol.version"
  end

  @doc """
  The value of the resource uri.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This is a URI of the resource provided in the following requests or notifications: `resources/read`, `resources/subscribe`, `resources/unsubscribe`, or `notifications/resources/updated`.

  ### Examples

  ```
  ["postgres://database/customers/schema", "file:///home/user/documents/report.pdf"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.McpAttributes.mcp_resource_uri()
      :"mcp.resource.uri"

  ### Erlang

  ```erlang
  ?MCP_RESOURCE_URI.
  'mcp.resource.uri'
  ```

  <!-- tabs-close -->
  """
  @spec mcp_resource_uri :: :"mcp.resource.uri"
  def mcp_resource_uri do
    :"mcp.resource.uri"
  end

  @doc """
  Identifies [MCP session](https://modelcontextprotocol.io/specification/2025-06-18/basic/transports#session-management).
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["191c4850af6c49e08843a3f6c80e5046"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.McpAttributes.mcp_session_id()
      :"mcp.session.id"

  ### Erlang

  ```erlang
  ?MCP_SESSION_ID.
  'mcp.session.id'
  ```

  <!-- tabs-close -->
  """
  @spec mcp_session_id :: :"mcp.session.id"
  def mcp_session_id do
    :"mcp.session.id"
  end
end
