
%%%------------------------------------------------------------------------
%% Copyright The OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%-------------------------------------------------------------------------

%% The JSON-serialized value of each item in the `AttributeDefinitions` request field.
-define(AWS_DYNAMODB_ATTRIBUTE_DEFINITIONS, 'aws.dynamodb.attribute_definitions').


%% The value of the `AttributesToGet` request parameter.
-define(AWS_DYNAMODB_ATTRIBUTES_TO_GET, 'aws.dynamodb.attributes_to_get').


%% The value of the `ConsistentRead` request parameter.
-define(AWS_DYNAMODB_CONSISTENT_READ, 'aws.dynamodb.consistent_read').


%% The JSON-serialized value of each item in the `ConsumedCapacity` response field.
-define(AWS_DYNAMODB_CONSUMED_CAPACITY, 'aws.dynamodb.consumed_capacity').


%% The value of the `Count` response parameter.
-define(AWS_DYNAMODB_COUNT, 'aws.dynamodb.count').


%% The value of the `ExclusiveStartTableName` request parameter.
-define(AWS_DYNAMODB_EXCLUSIVE_START_TABLE, 'aws.dynamodb.exclusive_start_table').


%% The JSON-serialized value of each item in the `GlobalSecondaryIndexUpdates` request field.
-define(AWS_DYNAMODB_GLOBAL_SECONDARY_INDEX_UPDATES, 'aws.dynamodb.global_secondary_index_updates').


%% The JSON-serialized value of each item of the `GlobalSecondaryIndexes` request field
-define(AWS_DYNAMODB_GLOBAL_SECONDARY_INDEXES, 'aws.dynamodb.global_secondary_indexes').


%% The value of the `IndexName` request parameter.
-define(AWS_DYNAMODB_INDEX_NAME, 'aws.dynamodb.index_name').


%% The JSON-serialized value of the `ItemCollectionMetrics` response field.
-define(AWS_DYNAMODB_ITEM_COLLECTION_METRICS, 'aws.dynamodb.item_collection_metrics').


%% The value of the `Limit` request parameter.
-define(AWS_DYNAMODB_LIMIT, 'aws.dynamodb.limit').


%% The JSON-serialized value of each item of the `LocalSecondaryIndexes` request field.
-define(AWS_DYNAMODB_LOCAL_SECONDARY_INDEXES, 'aws.dynamodb.local_secondary_indexes').


%% The value of the `ProjectionExpression` request parameter.
-define(AWS_DYNAMODB_PROJECTION, 'aws.dynamodb.projection').


%% The value of the `ProvisionedThroughput.ReadCapacityUnits` request parameter.
-define(AWS_DYNAMODB_PROVISIONED_READ_CAPACITY, 'aws.dynamodb.provisioned_read_capacity').


%% The value of the `ProvisionedThroughput.WriteCapacityUnits` request parameter.
-define(AWS_DYNAMODB_PROVISIONED_WRITE_CAPACITY, 'aws.dynamodb.provisioned_write_capacity').


%% The value of the `ScanIndexForward` request parameter.
-define(AWS_DYNAMODB_SCAN_FORWARD, 'aws.dynamodb.scan_forward').


%% The value of the `ScannedCount` response parameter.
-define(AWS_DYNAMODB_SCANNED_COUNT, 'aws.dynamodb.scanned_count').


%% The value of the `Segment` request parameter.
-define(AWS_DYNAMODB_SEGMENT, 'aws.dynamodb.segment').


%% The value of the `Select` request parameter.
-define(AWS_DYNAMODB_SELECT, 'aws.dynamodb.select').


%% The number of items in the `TableNames` response parameter.
-define(AWS_DYNAMODB_TABLE_COUNT, 'aws.dynamodb.table_count').


%% The keys in the `RequestItems` object field.
-define(AWS_DYNAMODB_TABLE_NAMES, 'aws.dynamodb.table_names').


%% The value of the `TotalSegments` request parameter.
-define(AWS_DYNAMODB_TOTAL_SEGMENTS, 'aws.dynamodb.total_segments').


%% The ARN of an [ECS cluster](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/clusters.html).
%%  
-define(AWS_ECS_CLUSTER_ARN, 'aws.ecs.cluster.arn').


%% The Amazon Resource Name (ARN) of an [ECS container instance](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ECS_instances.html).
%%  
-define(AWS_ECS_CONTAINER_ARN, 'aws.ecs.container.arn').


%% The [launch type](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html) for an ECS task.
%%  
-define(AWS_ECS_LAUNCHTYPE, 'aws.ecs.launchtype').

-define(AWS_ECS_LAUNCHTYPE_VALUES_EC2, 'ec2').

-define(AWS_ECS_LAUNCHTYPE_VALUES_FARGATE, 'fargate').



%% The ARN of a running [ECS task](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids).
%%  
-define(AWS_ECS_TASK_ARN, 'aws.ecs.task.arn').


%% The family name of the [ECS task definition](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html) used to create the ECS task.
%%  
-define(AWS_ECS_TASK_FAMILY, 'aws.ecs.task.family').


%% The ID of a running ECS task. The ID MUST be extracted from `task.arn`.
%%  
-define(AWS_ECS_TASK_ID, 'aws.ecs.task.id').


%% The revision for the task definition used to create the ECS task.
%%  
-define(AWS_ECS_TASK_REVISION, 'aws.ecs.task.revision').


%% The ARN of an EKS cluster.
%%  
-define(AWS_EKS_CLUSTER_ARN, 'aws.eks.cluster.arn').


%% The full invoked ARN as provided on the `Context` passed to the function (`Lambda-Runtime-Invoked-Function-Arn` header on the `/runtime/invocation/next` applicable).
%%  
-define(AWS_LAMBDA_INVOKED_ARN, 'aws.lambda.invoked_arn').


%% The Amazon Resource Name(s) (ARN) of the AWS log group(s).
%%  
-define(AWS_LOG_GROUP_ARNS, 'aws.log.group.arns').


%% The name(s) of the AWS log group(s) an application is writing to.
%%  
-define(AWS_LOG_GROUP_NAMES, 'aws.log.group.names').


%% The ARN(s) of the AWS log stream(s).
%%  
-define(AWS_LOG_STREAM_ARNS, 'aws.log.stream.arns').


%% The name(s) of the AWS log stream(s) an application is writing to.
%%  
-define(AWS_LOG_STREAM_NAMES, 'aws.log.stream.names').


%% The AWS request ID as returned in the response headers `x-amz-request-id` or `x-amz-requestid`.
-define(AWS_REQUEST_ID, 'aws.request_id').


%% The S3 bucket name the request refers to. Corresponds to the `--bucket` parameter of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) operations.
-define(AWS_S3_BUCKET, 'aws.s3.bucket').


%% The source object (in the form `bucket`/`key`) for the copy operation.
-define(AWS_S3_COPY_SOURCE, 'aws.s3.copy_source').


%% The delete request container that specifies the objects to be deleted.
-define(AWS_S3_DELETE, 'aws.s3.delete').


%% The S3 object key the request refers to. Corresponds to the `--key` parameter of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) operations.
-define(AWS_S3_KEY, 'aws.s3.key').


%% The part number of the part being uploaded in a multipart-upload operation. This is a positive integer between 1 and 10,000.
-define(AWS_S3_PART_NUMBER, 'aws.s3.part_number').


%% Upload ID that identifies the multipart upload.
-define(AWS_S3_UPLOAD_ID, 'aws.s3.upload_id').
