defmodule OpenTelemetry.SemanticConventions.AwsAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Aws attributes.
  """

  @doc """
  The JSON-serialized value of each item in the `AttributeDefinitions` request field.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_attributedefinitions()
      :"aws.dynamodb.attribute_definitions"
  """
  @spec aws_dynamodb_attributedefinitions :: :"aws.dynamodb.attribute_definitions"
  def aws_dynamodb_attributedefinitions do
    :"aws.dynamodb.attribute_definitions"
  end

  @doc """
  The value of the `AttributesToGet` request parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_attributestoget()
      :"aws.dynamodb.attributes_to_get"
  """
  @spec aws_dynamodb_attributestoget :: :"aws.dynamodb.attributes_to_get"
  def aws_dynamodb_attributestoget do
    :"aws.dynamodb.attributes_to_get"
  end

  @doc """
  The value of the `ConsistentRead` request parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_consistentread()
      :"aws.dynamodb.consistent_read"
  """
  @spec aws_dynamodb_consistentread :: :"aws.dynamodb.consistent_read"
  def aws_dynamodb_consistentread do
    :"aws.dynamodb.consistent_read"
  end

  @doc """
  The JSON-serialized value of each item in the `ConsumedCapacity` response field.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_consumedcapacity()
      :"aws.dynamodb.consumed_capacity"
  """
  @spec aws_dynamodb_consumedcapacity :: :"aws.dynamodb.consumed_capacity"
  def aws_dynamodb_consumedcapacity do
    :"aws.dynamodb.consumed_capacity"
  end

  @doc """
  The value of the `Count` response parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_count()
      :"aws.dynamodb.count"
  """
  @spec aws_dynamodb_count :: :"aws.dynamodb.count"
  def aws_dynamodb_count do
    :"aws.dynamodb.count"
  end

  @doc """
  The value of the `ExclusiveStartTableName` request parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_exclusivestarttable()
      :"aws.dynamodb.exclusive_start_table"
  """
  @spec aws_dynamodb_exclusivestarttable :: :"aws.dynamodb.exclusive_start_table"
  def aws_dynamodb_exclusivestarttable do
    :"aws.dynamodb.exclusive_start_table"
  end

  @doc """
  The JSON-serialized value of each item in the `GlobalSecondaryIndexUpdates` request field.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_globalsecondaryindexupdates()
      :"aws.dynamodb.global_secondary_index_updates"
  """
  @spec aws_dynamodb_globalsecondaryindexupdates :: :"aws.dynamodb.global_secondary_index_updates"
  def aws_dynamodb_globalsecondaryindexupdates do
    :"aws.dynamodb.global_secondary_index_updates"
  end

  @doc """
  The JSON-serialized value of each item of the `GlobalSecondaryIndexes` request field


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_globalsecondaryindexes()
      :"aws.dynamodb.global_secondary_indexes"
  """
  @spec aws_dynamodb_globalsecondaryindexes :: :"aws.dynamodb.global_secondary_indexes"
  def aws_dynamodb_globalsecondaryindexes do
    :"aws.dynamodb.global_secondary_indexes"
  end

  @doc """
  The value of the `IndexName` request parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_indexname()
      :"aws.dynamodb.index_name"
  """
  @spec aws_dynamodb_indexname :: :"aws.dynamodb.index_name"
  def aws_dynamodb_indexname do
    :"aws.dynamodb.index_name"
  end

  @doc """
  The JSON-serialized value of the `ItemCollectionMetrics` response field.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_itemcollectionmetrics()
      :"aws.dynamodb.item_collection_metrics"
  """
  @spec aws_dynamodb_itemcollectionmetrics :: :"aws.dynamodb.item_collection_metrics"
  def aws_dynamodb_itemcollectionmetrics do
    :"aws.dynamodb.item_collection_metrics"
  end

  @doc """
  The value of the `Limit` request parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_limit()
      :"aws.dynamodb.limit"
  """
  @spec aws_dynamodb_limit :: :"aws.dynamodb.limit"
  def aws_dynamodb_limit do
    :"aws.dynamodb.limit"
  end

  @doc """
  The JSON-serialized value of each item of the `LocalSecondaryIndexes` request field.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_localsecondaryindexes()
      :"aws.dynamodb.local_secondary_indexes"
  """
  @spec aws_dynamodb_localsecondaryindexes :: :"aws.dynamodb.local_secondary_indexes"
  def aws_dynamodb_localsecondaryindexes do
    :"aws.dynamodb.local_secondary_indexes"
  end

  @doc """
  The value of the `ProjectionExpression` request parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_projection()
      :"aws.dynamodb.projection"
  """
  @spec aws_dynamodb_projection :: :"aws.dynamodb.projection"
  def aws_dynamodb_projection do
    :"aws.dynamodb.projection"
  end

  @doc """
  The value of the `ProvisionedThroughput.ReadCapacityUnits` request parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_provisionedreadcapacity()
      :"aws.dynamodb.provisioned_read_capacity"
  """
  @spec aws_dynamodb_provisionedreadcapacity :: :"aws.dynamodb.provisioned_read_capacity"
  def aws_dynamodb_provisionedreadcapacity do
    :"aws.dynamodb.provisioned_read_capacity"
  end

  @doc """
  The value of the `ProvisionedThroughput.WriteCapacityUnits` request parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_provisionedwritecapacity()
      :"aws.dynamodb.provisioned_write_capacity"
  """
  @spec aws_dynamodb_provisionedwritecapacity :: :"aws.dynamodb.provisioned_write_capacity"
  def aws_dynamodb_provisionedwritecapacity do
    :"aws.dynamodb.provisioned_write_capacity"
  end

  @doc """
  The value of the `ScanIndexForward` request parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_scanforward()
      :"aws.dynamodb.scan_forward"
  """
  @spec aws_dynamodb_scanforward :: :"aws.dynamodb.scan_forward"
  def aws_dynamodb_scanforward do
    :"aws.dynamodb.scan_forward"
  end

  @doc """
  The value of the `ScannedCount` response parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_scannedcount()
      :"aws.dynamodb.scanned_count"
  """
  @spec aws_dynamodb_scannedcount :: :"aws.dynamodb.scanned_count"
  def aws_dynamodb_scannedcount do
    :"aws.dynamodb.scanned_count"
  end

  @doc """
  The value of the `Segment` request parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_segment()
      :"aws.dynamodb.segment"
  """
  @spec aws_dynamodb_segment :: :"aws.dynamodb.segment"
  def aws_dynamodb_segment do
    :"aws.dynamodb.segment"
  end

  @doc """
  The value of the `Select` request parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_select()
      :"aws.dynamodb.select"
  """
  @spec aws_dynamodb_select :: :"aws.dynamodb.select"
  def aws_dynamodb_select do
    :"aws.dynamodb.select"
  end

  @doc """
  The number of items in the `TableNames` response parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_tablecount()
      :"aws.dynamodb.table_count"
  """
  @spec aws_dynamodb_tablecount :: :"aws.dynamodb.table_count"
  def aws_dynamodb_tablecount do
    :"aws.dynamodb.table_count"
  end

  @doc """
  The keys in the `RequestItems` object field.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_tablenames()
      :"aws.dynamodb.table_names"
  """
  @spec aws_dynamodb_tablenames :: :"aws.dynamodb.table_names"
  def aws_dynamodb_tablenames do
    :"aws.dynamodb.table_names"
  end

  @doc """
  The value of the `TotalSegments` request parameter.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_dynamodb_totalsegments()
      :"aws.dynamodb.total_segments"
  """
  @spec aws_dynamodb_totalsegments :: :"aws.dynamodb.total_segments"
  def aws_dynamodb_totalsegments do
    :"aws.dynamodb.total_segments"
  end

  @doc """
  The ARN of an [ECS cluster](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/clusters.html).



  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_ecs_cluster_arn()
      :"aws.ecs.cluster.arn"
  """
  @spec aws_ecs_cluster_arn :: :"aws.ecs.cluster.arn"
  def aws_ecs_cluster_arn do
    :"aws.ecs.cluster.arn"
  end

  @doc """
  The Amazon Resource Name (ARN) of an [ECS container instance](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ECS_instances.html).



  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_ecs_container_arn()
      :"aws.ecs.container.arn"
  """
  @spec aws_ecs_container_arn :: :"aws.ecs.container.arn"
  def aws_ecs_container_arn do
    :"aws.ecs.container.arn"
  end

  @typedoc """
  The [launch type](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html) for an ECS task.


  ### Options
  * `:ec2` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:fargate` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type aws_ecs_launchtype() :: :ec2 | :fargate | atom()

  @doc """
  The [launch type](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html) for an ECS task.



  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_ecs_launchtype(:ec2)
      :ec2
      
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_ecs_launchtype(:custom_value)
      :custom_value
  """
  @spec aws_ecs_launchtype(aws_ecs_launchtype()) :: :ec2 | :fargate | atom()
  def aws_ecs_launchtype(option) do
    case option do
      :ec2 -> :ec2
      :fargate -> :fargate
      _ -> option
    end
  end

  @doc """
  The ARN of a running [ECS task](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids).



  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_ecs_task_arn()
      :"aws.ecs.task.arn"
  """
  @spec aws_ecs_task_arn :: :"aws.ecs.task.arn"
  def aws_ecs_task_arn do
    :"aws.ecs.task.arn"
  end

  @doc """
  The family name of the [ECS task definition](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html) used to create the ECS task.



  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_ecs_task_family()
      :"aws.ecs.task.family"
  """
  @spec aws_ecs_task_family :: :"aws.ecs.task.family"
  def aws_ecs_task_family do
    :"aws.ecs.task.family"
  end

  @doc """
  The ID of a running ECS task. The ID **MUST** be extracted from `task.arn`.



  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_ecs_task_id()
      :"aws.ecs.task.id"
  """
  @spec aws_ecs_task_id :: :"aws.ecs.task.id"
  def aws_ecs_task_id do
    :"aws.ecs.task.id"
  end

  @doc """
  The revision for the task definition used to create the ECS task.



  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_ecs_task_revision()
      :"aws.ecs.task.revision"
  """
  @spec aws_ecs_task_revision :: :"aws.ecs.task.revision"
  def aws_ecs_task_revision do
    :"aws.ecs.task.revision"
  end

  @doc """
  The ARN of an EKS cluster.



  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_eks_cluster_arn()
      :"aws.eks.cluster.arn"
  """
  @spec aws_eks_cluster_arn :: :"aws.eks.cluster.arn"
  def aws_eks_cluster_arn do
    :"aws.eks.cluster.arn"
  end

  @doc """
  The full invoked ARN as provided on the `Context` passed to the function (`Lambda-Runtime-Invoked-Function-Arn` header on the `/runtime/invocation/next` applicable).

  ### Notes

  This may be different from `cloud.resource_id` if an alias is involved.

  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_lambda_invokedarn()
      :"aws.lambda.invoked_arn"
  """
  @spec aws_lambda_invokedarn :: :"aws.lambda.invoked_arn"
  def aws_lambda_invokedarn do
    :"aws.lambda.invoked_arn"
  end

  @doc """
  The Amazon Resource Name(s) (ARN) of the AWS log group(s).

  ### Notes

  See the [log group ARN format documentation](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format).


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_log_group_arns()
      :"aws.log.group.arns"
  """
  @spec aws_log_group_arns :: :"aws.log.group.arns"
  def aws_log_group_arns do
    :"aws.log.group.arns"
  end

  @doc """
  The name(s) of the AWS log group(s) an application is writing to.

  ### Notes

  Multiple log groups must be supported for cases like multi-container applications, where a single application has sidecar containers, and each write to their own log group.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_log_group_names()
      :"aws.log.group.names"
  """
  @spec aws_log_group_names :: :"aws.log.group.names"
  def aws_log_group_names do
    :"aws.log.group.names"
  end

  @doc """
  The ARN(s) of the AWS log stream(s).

  ### Notes

  See the [log stream ARN format documentation](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format). One log group can contain several log streams, so these ARNs necessarily identify both a log group and a log stream.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_log_stream_arns()
      :"aws.log.stream.arns"
  """
  @spec aws_log_stream_arns :: :"aws.log.stream.arns"
  def aws_log_stream_arns do
    :"aws.log.stream.arns"
  end

  @doc """
  The name(s) of the AWS log stream(s) an application is writing to.



  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_log_stream_names()
      :"aws.log.stream.names"
  """
  @spec aws_log_stream_names :: :"aws.log.stream.names"
  def aws_log_stream_names do
    :"aws.log.stream.names"
  end

  @doc """
  The AWS request ID as returned in the response headers `x-amz-request-id` or `x-amz-requestid`.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_requestid()
      :"aws.request_id"
  """
  @spec aws_requestid :: :"aws.request_id"
  def aws_requestid do
    :"aws.request_id"
  end

  @doc """
  The S3 bucket name the request refers to. Corresponds to the `--bucket` parameter of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) operations.
  ### Notes

  The `bucket` attribute is applicable to all S3 operations that reference a bucket, i.e. that require the bucket name as a mandatory parameter.
  This applies to almost all S3 operations except `list-buckets`.


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_s_3_bucket()
      :"aws.s3.bucket"
  """
  @spec aws_s_3_bucket :: :"aws.s3.bucket"
  def aws_s_3_bucket do
    :"aws.s3.bucket"
  end

  @doc """
  The source object (in the form `bucket`/`key`) for the copy operation.
  ### Notes

  The `copy_source` attribute applies to S3 copy operations and corresponds to the `--copy-source` parameter
  of the [copy-object operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html).
  This applies in particular to the following operations:

  - [copy-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html)
  - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_s_3_copysource()
      :"aws.s3.copy_source"
  """
  @spec aws_s_3_copysource :: :"aws.s3.copy_source"
  def aws_s_3_copysource do
    :"aws.s3.copy_source"
  end

  @doc """
  The delete request container that specifies the objects to be deleted.
  ### Notes

  The `delete` attribute is only applicable to the [delete-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-object.html) operation.
  The `delete` attribute corresponds to the `--delete` parameter of the
  [delete-objects operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-objects.html).


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_s_3_delete()
      :"aws.s3.delete"
  """
  @spec aws_s_3_delete :: :"aws.s3.delete"
  def aws_s_3_delete do
    :"aws.s3.delete"
  end

  @doc """
  The S3 object key the request refers to. Corresponds to the `--key` parameter of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) operations.
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


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_s_3_key()
      :"aws.s3.key"
  """
  @spec aws_s_3_key :: :"aws.s3.key"
  def aws_s_3_key do
    :"aws.s3.key"
  end

  @doc """
  The part number of the part being uploaded in a multipart-upload operation. This is a positive integer between 1 and 10,000.
  ### Notes

  The `part_number` attribute is only applicable to the [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
  and [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html) operations.
  The `part_number` attribute corresponds to the `--part-number` parameter of the
  [upload-part operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html).


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_s_3_partnumber()
      :"aws.s3.part_number"
  """
  @spec aws_s_3_partnumber :: :"aws.s3.part_number"
  def aws_s_3_partnumber do
    :"aws.s3.part_number"
  end

  @doc """
  Upload ID that identifies the multipart upload.
  ### Notes

  The `upload_id` attribute applies to S3 multipart-upload operations and corresponds to the `--upload-id` parameter
  of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) multipart operations.
  This applies in particular to the following operations:

  - [abort-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/abort-multipart-upload.html)
  - [complete-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/complete-multipart-upload.html)
  - [list-parts](https://docs.aws.amazon.com/cli/latest/reference/s3api/list-parts.html)
  - [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
  - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)


  ### Example
      iex> OpenTelemetry.SemanticConventions.AwsAttributes.aws_s_3_uploadid()
      :"aws.s3.upload_id"
  """
  @spec aws_s_3_uploadid :: :"aws.s3.upload_id"
  def aws_s_3_uploadid do
    :"aws.s3.upload_id"
  end
end
