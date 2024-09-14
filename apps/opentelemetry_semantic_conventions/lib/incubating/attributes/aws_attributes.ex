defmodule OpenTelemetry.SemConv.Incubating.AWSAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for AWS attributes.
  """

  @doc """
  The JSON-serialized value of each item in the `AttributeDefinitions` request field.
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["{ \"AttributeName\": \"string\", \"AttributeType\": \"string\" }"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_attribute_definitions()
      :"aws.dynamodb.attribute_definitions"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_ATTRIBUTE_DEFINITIONS.
  'aws.dynamodb.attribute_definitions'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_attribute_definitions :: :"aws.dynamodb.attribute_definitions"
  def aws_dynamodb_attribute_definitions do
    :"aws.dynamodb.attribute_definitions"
  end

  @doc """
  The value of the `AttributesToGet` request parameter.
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["lives", "id"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_attributes_to_get()
      :"aws.dynamodb.attributes_to_get"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_ATTRIBUTES_TO_GET.
  'aws.dynamodb.attributes_to_get'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_attributes_to_get :: :"aws.dynamodb.attributes_to_get"
  def aws_dynamodb_attributes_to_get do
    :"aws.dynamodb.attributes_to_get"
  end

  @doc """
  The value of the `ConsistentRead` request parameter.
  ### Value type

  Value must be of type `boolean()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_consistent_read()
      :"aws.dynamodb.consistent_read"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_CONSISTENT_READ.
  'aws.dynamodb.consistent_read'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_consistent_read :: :"aws.dynamodb.consistent_read"
  def aws_dynamodb_consistent_read do
    :"aws.dynamodb.consistent_read"
  end

  @doc """
  The JSON-serialized value of each item in the `ConsumedCapacity` response field.
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["{ \"CapacityUnits\": number, \"GlobalSecondaryIndexes\": { \"string\" : { \"CapacityUnits\": number, \"ReadCapacityUnits\": number, \"WriteCapacityUnits\": number } }, \"LocalSecondaryIndexes\": { \"string\" : { \"CapacityUnits\": number, \"ReadCapacityUnits\": number, \"WriteCapacityUnits\": number } }, \"ReadCapacityUnits\": number, \"Table\": { \"CapacityUnits\": number, \"ReadCapacityUnits\": number, \"WriteCapacityUnits\": number }, \"TableName\": \"string\", \"WriteCapacityUnits\": number }"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_consumed_capacity()
      :"aws.dynamodb.consumed_capacity"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_CONSUMED_CAPACITY.
  'aws.dynamodb.consumed_capacity'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_consumed_capacity :: :"aws.dynamodb.consumed_capacity"
  def aws_dynamodb_consumed_capacity do
    :"aws.dynamodb.consumed_capacity"
  end

  @doc """
  The value of the `Count` response parameter.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [10]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_count()
      :"aws.dynamodb.count"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_COUNT.
  'aws.dynamodb.count'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_count :: :"aws.dynamodb.count"
  def aws_dynamodb_count do
    :"aws.dynamodb.count"
  end

  @doc """
  The value of the `ExclusiveStartTableName` request parameter.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Users", "CatsTable"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_exclusive_start_table()
      :"aws.dynamodb.exclusive_start_table"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_EXCLUSIVE_START_TABLE.
  'aws.dynamodb.exclusive_start_table'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_exclusive_start_table :: :"aws.dynamodb.exclusive_start_table"
  def aws_dynamodb_exclusive_start_table do
    :"aws.dynamodb.exclusive_start_table"
  end

  @doc """
  The JSON-serialized value of each item in the `GlobalSecondaryIndexUpdates` request field.
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["{ \"Create\": { \"IndexName\": \"string\", \"KeySchema\": [ { \"AttributeName\": \"string\", \"KeyType\": \"string\" } ], \"Projection\": { \"NonKeyAttributes\": [ \"string\" ], \"ProjectionType\": \"string\" }, \"ProvisionedThroughput\": { \"ReadCapacityUnits\": number, \"WriteCapacityUnits\": number } }"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_global_secondary_index_updates()
      :"aws.dynamodb.global_secondary_index_updates"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_GLOBAL_SECONDARY_INDEX_UPDATES.
  'aws.dynamodb.global_secondary_index_updates'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_global_secondary_index_updates ::
          :"aws.dynamodb.global_secondary_index_updates"
  def aws_dynamodb_global_secondary_index_updates do
    :"aws.dynamodb.global_secondary_index_updates"
  end

  @doc """
  The JSON-serialized value of each item of the `GlobalSecondaryIndexes` request field
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["{ \"IndexName\": \"string\", \"KeySchema\": [ { \"AttributeName\": \"string\", \"KeyType\": \"string\" } ], \"Projection\": { \"NonKeyAttributes\": [ \"string\" ], \"ProjectionType\": \"string\" }, \"ProvisionedThroughput\": { \"ReadCapacityUnits\": number, \"WriteCapacityUnits\": number } }"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_global_secondary_indexes()
      :"aws.dynamodb.global_secondary_indexes"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_GLOBAL_SECONDARY_INDEXES.
  'aws.dynamodb.global_secondary_indexes'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_global_secondary_indexes :: :"aws.dynamodb.global_secondary_indexes"
  def aws_dynamodb_global_secondary_indexes do
    :"aws.dynamodb.global_secondary_indexes"
  end

  @doc """
  The value of the `IndexName` request parameter.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["name_to_group"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_index_name()
      :"aws.dynamodb.index_name"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_INDEX_NAME.
  'aws.dynamodb.index_name'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_index_name :: :"aws.dynamodb.index_name"
  def aws_dynamodb_index_name do
    :"aws.dynamodb.index_name"
  end

  @doc """
  The JSON-serialized value of the `ItemCollectionMetrics` response field.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["{ \"string\" : [ { \"ItemCollectionKey\": { \"string\" : { \"B\": blob, \"BOOL\": boolean, \"BS\": [ blob ], \"L\": [ \"AttributeValue\" ], \"M\": { \"string\" : \"AttributeValue\" }, \"N\": \"string\", \"NS\": [ \"string\" ], \"NULL\": boolean, \"S\": \"string\", \"SS\": [ \"string\" ] } }, \"SizeEstimateRangeGB\": [ number ] } ] }"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_item_collection_metrics()
      :"aws.dynamodb.item_collection_metrics"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_ITEM_COLLECTION_METRICS.
  'aws.dynamodb.item_collection_metrics'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_item_collection_metrics :: :"aws.dynamodb.item_collection_metrics"
  def aws_dynamodb_item_collection_metrics do
    :"aws.dynamodb.item_collection_metrics"
  end

  @doc """
  The value of the `Limit` request parameter.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [10]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_limit()
      :"aws.dynamodb.limit"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_LIMIT.
  'aws.dynamodb.limit'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_limit :: :"aws.dynamodb.limit"
  def aws_dynamodb_limit do
    :"aws.dynamodb.limit"
  end

  @doc """
  The JSON-serialized value of each item of the `LocalSecondaryIndexes` request field.
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["{ \"IndexArn\": \"string\", \"IndexName\": \"string\", \"IndexSizeBytes\": number, \"ItemCount\": number, \"KeySchema\": [ { \"AttributeName\": \"string\", \"KeyType\": \"string\" } ], \"Projection\": { \"NonKeyAttributes\": [ \"string\" ], \"ProjectionType\": \"string\" } }"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_local_secondary_indexes()
      :"aws.dynamodb.local_secondary_indexes"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_LOCAL_SECONDARY_INDEXES.
  'aws.dynamodb.local_secondary_indexes'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_local_secondary_indexes :: :"aws.dynamodb.local_secondary_indexes"
  def aws_dynamodb_local_secondary_indexes do
    :"aws.dynamodb.local_secondary_indexes"
  end

  @doc """
  The value of the `ProjectionExpression` request parameter.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Title", "Title, Price, Color", "Title, Description, RelatedItems, ProductReviews"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_projection()
      :"aws.dynamodb.projection"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_PROJECTION.
  'aws.dynamodb.projection'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_projection :: :"aws.dynamodb.projection"
  def aws_dynamodb_projection do
    :"aws.dynamodb.projection"
  end

  @doc """
  The value of the `ProvisionedThroughput.ReadCapacityUnits` request parameter.
  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [1.0, 2.0]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_provisioned_read_capacity()
      :"aws.dynamodb.provisioned_read_capacity"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_PROVISIONED_READ_CAPACITY.
  'aws.dynamodb.provisioned_read_capacity'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_provisioned_read_capacity :: :"aws.dynamodb.provisioned_read_capacity"
  def aws_dynamodb_provisioned_read_capacity do
    :"aws.dynamodb.provisioned_read_capacity"
  end

  @doc """
  The value of the `ProvisionedThroughput.WriteCapacityUnits` request parameter.
  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [1.0, 2.0]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_provisioned_write_capacity()
      :"aws.dynamodb.provisioned_write_capacity"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_PROVISIONED_WRITE_CAPACITY.
  'aws.dynamodb.provisioned_write_capacity'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_provisioned_write_capacity :: :"aws.dynamodb.provisioned_write_capacity"
  def aws_dynamodb_provisioned_write_capacity do
    :"aws.dynamodb.provisioned_write_capacity"
  end

  @doc """
  The value of the `ScanIndexForward` request parameter.
  ### Value type

  Value must be of type `boolean()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_scan_forward()
      :"aws.dynamodb.scan_forward"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_SCAN_FORWARD.
  'aws.dynamodb.scan_forward'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_scan_forward :: :"aws.dynamodb.scan_forward"
  def aws_dynamodb_scan_forward do
    :"aws.dynamodb.scan_forward"
  end

  @doc """
  The value of the `ScannedCount` response parameter.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [50]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_scanned_count()
      :"aws.dynamodb.scanned_count"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_SCANNED_COUNT.
  'aws.dynamodb.scanned_count'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_scanned_count :: :"aws.dynamodb.scanned_count"
  def aws_dynamodb_scanned_count do
    :"aws.dynamodb.scanned_count"
  end

  @doc """
  The value of the `Segment` request parameter.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [10]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_segment()
      :"aws.dynamodb.segment"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_SEGMENT.
  'aws.dynamodb.segment'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_segment :: :"aws.dynamodb.segment"
  def aws_dynamodb_segment do
    :"aws.dynamodb.segment"
  end

  @doc """
  The value of the `Select` request parameter.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["ALL_ATTRIBUTES", "COUNT"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_select()
      :"aws.dynamodb.select"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_SELECT.
  'aws.dynamodb.select'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_select :: :"aws.dynamodb.select"
  def aws_dynamodb_select do
    :"aws.dynamodb.select"
  end

  @doc """
  The number of items in the `TableNames` response parameter.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [20]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_table_count()
      :"aws.dynamodb.table_count"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_TABLE_COUNT.
  'aws.dynamodb.table_count'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_table_count :: :"aws.dynamodb.table_count"
  def aws_dynamodb_table_count do
    :"aws.dynamodb.table_count"
  end

  @doc """
  The keys in the `RequestItems` object field.
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["Users", "Cats"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_table_names()
      :"aws.dynamodb.table_names"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_TABLE_NAMES.
  'aws.dynamodb.table_names'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_table_names :: :"aws.dynamodb.table_names"
  def aws_dynamodb_table_names do
    :"aws.dynamodb.table_names"
  end

  @doc """
  The value of the `TotalSegments` request parameter.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [100]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_dynamodb_total_segments()
      :"aws.dynamodb.total_segments"

  ### Erlang

  ```erlang
  ?AWS_DYNAMODB_TOTAL_SEGMENTS.
  'aws.dynamodb.total_segments'
  ```

  <!-- tabs-close -->
  """
  @spec aws_dynamodb_total_segments :: :"aws.dynamodb.total_segments"
  def aws_dynamodb_total_segments do
    :"aws.dynamodb.total_segments"
  end

  @doc """
  The ARN of an [ECS cluster](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/clusters.html).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["arn:aws:ecs:us-west-2:123456789123:cluster/my-cluster"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_ecs_cluster_arn()
      :"aws.ecs.cluster.arn"

  ### Erlang

  ```erlang
  ?AWS_ECS_CLUSTER_ARN.
  'aws.ecs.cluster.arn'
  ```

  <!-- tabs-close -->
  """
  @spec aws_ecs_cluster_arn :: :"aws.ecs.cluster.arn"
  def aws_ecs_cluster_arn do
    :"aws.ecs.cluster.arn"
  end

  @doc """
  The Amazon Resource Name (ARN) of an [ECS container instance](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ECS_instances.html).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["arn:aws:ecs:us-west-1:123456789123:container/32624152-9086-4f0e-acae-1a75b14fe4d9"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_ecs_container_arn()
      :"aws.ecs.container.arn"

  ### Erlang

  ```erlang
  ?AWS_ECS_CONTAINER_ARN.
  'aws.ecs.container.arn'
  ```

  <!-- tabs-close -->
  """
  @spec aws_ecs_container_arn :: :"aws.ecs.container.arn"
  def aws_ecs_container_arn do
    :"aws.ecs.container.arn"
  end

  @typedoc """
  The [launch type](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html) for an ECS task.


  ### Enum Values
  * `:ec2` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:fargate` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type aws_ecs_launchtype_values() :: %{
          :ec2 => :ec2,
          :fargate => :fargate
        }
  @doc """
  The [launch type](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html) for an ECS task.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_ecs_launchtype()
      :"aws.ecs.launchtype"

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_ecs_launchtype_values().ec2
      :ec2

      iex> %{OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_ecs_launchtype() => OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_ecs_launchtype_values().ec2}
      %{:"aws.ecs.launchtype" => :ec2}

  ### Erlang

  ```erlang
  ?AWS_ECS_LAUNCHTYPE.
  'aws.ecs.launchtype'

  ?AWS_ECS_LAUNCHTYPE_VALUES_EC2.
  'ec2'

  \#{?AWS_ECS_LAUNCHTYPE => ?AWS_ECS_LAUNCHTYPE_VALUES_EC2}.
  \#{'aws.ecs.launchtype' => 'ec2'}
  ```

  <!-- tabs-close -->
  """
  @spec aws_ecs_launchtype :: :"aws.ecs.launchtype"
  def aws_ecs_launchtype do
    :"aws.ecs.launchtype"
  end

  @spec aws_ecs_launchtype_values() :: aws_ecs_launchtype_values()
  def aws_ecs_launchtype_values() do
    %{
      :ec2 => :ec2,
      :fargate => :fargate
    }
  end

  @doc """
  The ARN of a running [ECS task](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["arn:aws:ecs:us-west-1:123456789123:task/10838bed-421f-43ef-870a-f43feacbbb5b", "arn:aws:ecs:us-west-1:123456789123:task/my-cluster/task-id/23ebb8ac-c18f-46c6-8bbe-d55d0e37cfbd"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_ecs_task_arn()
      :"aws.ecs.task.arn"

  ### Erlang

  ```erlang
  ?AWS_ECS_TASK_ARN.
  'aws.ecs.task.arn'
  ```

  <!-- tabs-close -->
  """
  @spec aws_ecs_task_arn :: :"aws.ecs.task.arn"
  def aws_ecs_task_arn do
    :"aws.ecs.task.arn"
  end

  @doc """
  The family name of the [ECS task definition](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html) used to create the ECS task.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry-family"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_ecs_task_family()
      :"aws.ecs.task.family"

  ### Erlang

  ```erlang
  ?AWS_ECS_TASK_FAMILY.
  'aws.ecs.task.family'
  ```

  <!-- tabs-close -->
  """
  @spec aws_ecs_task_family :: :"aws.ecs.task.family"
  def aws_ecs_task_family do
    :"aws.ecs.task.family"
  end

  @doc """
  The ID of a running ECS task. The ID **MUST** be extracted from `task.arn`.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["10838bed-421f-43ef-870a-f43feacbbb5b", "23ebb8ac-c18f-46c6-8bbe-d55d0e37cfbd"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_ecs_task_id()
      :"aws.ecs.task.id"

  ### Erlang

  ```erlang
  ?AWS_ECS_TASK_ID.
  'aws.ecs.task.id'
  ```

  <!-- tabs-close -->
  """
  @spec aws_ecs_task_id :: :"aws.ecs.task.id"
  def aws_ecs_task_id do
    :"aws.ecs.task.id"
  end

  @doc """
  The revision for the task definition used to create the ECS task.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["8", "26"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_ecs_task_revision()
      :"aws.ecs.task.revision"

  ### Erlang

  ```erlang
  ?AWS_ECS_TASK_REVISION.
  'aws.ecs.task.revision'
  ```

  <!-- tabs-close -->
  """
  @spec aws_ecs_task_revision :: :"aws.ecs.task.revision"
  def aws_ecs_task_revision do
    :"aws.ecs.task.revision"
  end

  @doc """
  The ARN of an EKS cluster.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["arn:aws:ecs:us-west-2:123456789123:cluster/my-cluster"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_eks_cluster_arn()
      :"aws.eks.cluster.arn"

  ### Erlang

  ```erlang
  ?AWS_EKS_CLUSTER_ARN.
  'aws.eks.cluster.arn'
  ```

  <!-- tabs-close -->
  """
  @spec aws_eks_cluster_arn :: :"aws.eks.cluster.arn"
  def aws_eks_cluster_arn do
    :"aws.eks.cluster.arn"
  end

  @doc """
  The full invoked ARN as provided on the `Context` passed to the function (`Lambda-Runtime-Invoked-Function-Arn` header on the `/runtime/invocation/next` applicable).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This may be different from `cloud.resource_id` if an alias is involved.
  ### Examples

  ```
  ["arn:aws:lambda:us-east-1:123456:function:myfunction:myalias"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_lambda_invoked_arn()
      :"aws.lambda.invoked_arn"

  ### Erlang

  ```erlang
  ?AWS_LAMBDA_INVOKED_ARN.
  'aws.lambda.invoked_arn'
  ```

  <!-- tabs-close -->
  """
  @spec aws_lambda_invoked_arn :: :"aws.lambda.invoked_arn"
  def aws_lambda_invoked_arn do
    :"aws.lambda.invoked_arn"
  end

  @doc """
  The Amazon Resource Name(s) (ARN) of the AWS log group(s).

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  See the [log group ARN format documentation](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format).

  ### Examples

  ```
  ["arn:aws:logs:us-west-1:123456789012:log-group:/aws/my/group:*"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_log_group_arns()
      :"aws.log.group.arns"

  ### Erlang

  ```erlang
  ?AWS_LOG_GROUP_ARNS.
  'aws.log.group.arns'
  ```

  <!-- tabs-close -->
  """
  @spec aws_log_group_arns :: :"aws.log.group.arns"
  def aws_log_group_arns do
    :"aws.log.group.arns"
  end

  @doc """
  The name(s) of the AWS log group(s) an application is writing to.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  Multiple log groups must be supported for cases like multi-container applications, where a single application has sidecar containers, and each write to their own log group.

  ### Examples

  ```
  ["/aws/lambda/my-function", "opentelemetry-service"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_log_group_names()
      :"aws.log.group.names"

  ### Erlang

  ```erlang
  ?AWS_LOG_GROUP_NAMES.
  'aws.log.group.names'
  ```

  <!-- tabs-close -->
  """
  @spec aws_log_group_names :: :"aws.log.group.names"
  def aws_log_group_names do
    :"aws.log.group.names"
  end

  @doc """
  The ARN(s) of the AWS log stream(s).

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  See the [log stream ARN format documentation](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format). One log group can contain several log streams, so these ARNs necessarily identify both a log group and a log stream.

  ### Examples

  ```
  ["arn:aws:logs:us-west-1:123456789012:log-group:/aws/my/group:log-stream:logs/main/10838bed-421f-43ef-870a-f43feacbbb5b"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_log_stream_arns()
      :"aws.log.stream.arns"

  ### Erlang

  ```erlang
  ?AWS_LOG_STREAM_ARNS.
  'aws.log.stream.arns'
  ```

  <!-- tabs-close -->
  """
  @spec aws_log_stream_arns :: :"aws.log.stream.arns"
  def aws_log_stream_arns do
    :"aws.log.stream.arns"
  end

  @doc """
  The name(s) of the AWS log stream(s) an application is writing to.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["logs/main/10838bed-421f-43ef-870a-f43feacbbb5b"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_log_stream_names()
      :"aws.log.stream.names"

  ### Erlang

  ```erlang
  ?AWS_LOG_STREAM_NAMES.
  'aws.log.stream.names'
  ```

  <!-- tabs-close -->
  """
  @spec aws_log_stream_names :: :"aws.log.stream.names"
  def aws_log_stream_names do
    :"aws.log.stream.names"
  end

  @doc """
  The AWS request ID as returned in the response headers `x-amz-request-id` or `x-amz-requestid`.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["79b9da39-b7ae-508a-a6bc-864b2829c622", "C9ER4AJX75574TDJ"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_request_id()
      :"aws.request_id"

  ### Erlang

  ```erlang
  ?AWS_REQUEST_ID.
  'aws.request_id'
  ```

  <!-- tabs-close -->
  """
  @spec aws_request_id :: :"aws.request_id"
  def aws_request_id do
    :"aws.request_id"
  end

  @doc """
  The S3 bucket name the request refers to. Corresponds to the `--bucket` parameter of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) operations.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `bucket` attribute is applicable to all S3 operations that reference a bucket, i.e. that require the bucket name as a mandatory parameter.
  This applies to almost all S3 operations except `list-buckets`.

  ### Examples

  ```
  ["some-bucket-name"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_s3_bucket()
      :"aws.s3.bucket"

  ### Erlang

  ```erlang
  ?AWS_S3_BUCKET.
  'aws.s3.bucket'
  ```

  <!-- tabs-close -->
  """
  @spec aws_s3_bucket :: :"aws.s3.bucket"
  def aws_s3_bucket do
    :"aws.s3.bucket"
  end

  @doc """
  The source object (in the form `bucket`/`key`) for the copy operation.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `copy_source` attribute applies to S3 copy operations and corresponds to the `--copy-source` parameter
  of the [copy-object operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html).
  This applies in particular to the following operations:

  - [copy-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html)
  - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)

  ### Examples

  ```
  ["someFile.yml"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_s3_copy_source()
      :"aws.s3.copy_source"

  ### Erlang

  ```erlang
  ?AWS_S3_COPY_SOURCE.
  'aws.s3.copy_source'
  ```

  <!-- tabs-close -->
  """
  @spec aws_s3_copy_source :: :"aws.s3.copy_source"
  def aws_s3_copy_source do
    :"aws.s3.copy_source"
  end

  @doc """
  The delete request container that specifies the objects to be deleted.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `delete` attribute is only applicable to the [delete-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-object.html) operation.
  The `delete` attribute corresponds to the `--delete` parameter of the
  [delete-objects operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-objects.html).

  ### Examples

  ```
  ["Objects=[{Key=string,VersionId=string},{Key=string,VersionId=string}],Quiet=boolean"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_s3_delete()
      :"aws.s3.delete"

  ### Erlang

  ```erlang
  ?AWS_S3_DELETE.
  'aws.s3.delete'
  ```

  <!-- tabs-close -->
  """
  @spec aws_s3_delete :: :"aws.s3.delete"
  def aws_s3_delete do
    :"aws.s3.delete"
  end

  @doc """
  The S3 object key the request refers to. Corresponds to the `--key` parameter of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) operations.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `key` attribute is applicable to all object-related S3 operations, i.e. that require the object key as a mandatory parameter.
  This applies in particular to the following operations:

  - [copy-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html)
  - [delete-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-object.html)
  - [get-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/get-object.html)
  - [head-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/head-object.html)
  - [put-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/put-object.html)
  - [restore-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/restore-object.html)
  - [select-object-content](https://docs.aws.amazon.com/cli/latest/reference/s3api/select-object-content.html)
  - [abort-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/abort-multipart-upload.html)
  - [complete-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/complete-multipart-upload.html)
  - [create-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/create-multipart-upload.html)
  - [list-parts](https://docs.aws.amazon.com/cli/latest/reference/s3api/list-parts.html)
  - [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
  - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)

  ### Examples

  ```
  ["someFile.yml"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_s3_key()
      :"aws.s3.key"

  ### Erlang

  ```erlang
  ?AWS_S3_KEY.
  'aws.s3.key'
  ```

  <!-- tabs-close -->
  """
  @spec aws_s3_key :: :"aws.s3.key"
  def aws_s3_key do
    :"aws.s3.key"
  end

  @doc """
  The part number of the part being uploaded in a multipart-upload operation. This is a positive integer between 1 and 10,000.
  ### Value type

  Value must be of type `integer()`.
  ### Notes

  The `part_number` attribute is only applicable to the [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
  and [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html) operations.
  The `part_number` attribute corresponds to the `--part-number` parameter of the
  [upload-part operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html).

  ### Examples

  ```
  [3456]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_s3_part_number()
      :"aws.s3.part_number"

  ### Erlang

  ```erlang
  ?AWS_S3_PART_NUMBER.
  'aws.s3.part_number'
  ```

  <!-- tabs-close -->
  """
  @spec aws_s3_part_number :: :"aws.s3.part_number"
  def aws_s3_part_number do
    :"aws.s3.part_number"
  end

  @doc """
  Upload ID that identifies the multipart upload.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `upload_id` attribute applies to S3 multipart-upload operations and corresponds to the `--upload-id` parameter
  of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) multipart operations.
  This applies in particular to the following operations:

  - [abort-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/abort-multipart-upload.html)
  - [complete-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/complete-multipart-upload.html)
  - [list-parts](https://docs.aws.amazon.com/cli/latest/reference/s3api/list-parts.html)
  - [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
  - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)

  ### Examples

  ```
  ["dfRtDYWFbkRONycy.Yxwh66Yjlx.cph0gtNBtJ"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AWSAttributes.aws_s3_upload_id()
      :"aws.s3.upload_id"

  ### Erlang

  ```erlang
  ?AWS_S3_UPLOAD_ID.
  'aws.s3.upload_id'
  ```

  <!-- tabs-close -->
  """
  @spec aws_s3_upload_id :: :"aws.s3.upload_id"
  def aws_s3_upload_id do
    :"aws.s3.upload_id"
  end
end
