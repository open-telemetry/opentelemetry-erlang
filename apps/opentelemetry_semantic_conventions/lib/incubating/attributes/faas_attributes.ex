defmodule OpenTelemetry.SemConv.Incubating.FAASAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for FAAS attributes.
  """

  @doc """
  A boolean that is true if the serverless function is executed for the first time (aka cold-start).

  ### Value type

  Value must be of type `boolean()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_coldstart()
      :"faas.coldstart"

  ### Erlang

  ```erlang
  ?FAAS_COLDSTART.
  'faas.coldstart'
  ```

  <!-- tabs-close -->
  """
  @spec faas_coldstart :: :"faas.coldstart"
  def faas_coldstart do
    :"faas.coldstart"
  end

  @doc """
  A string containing the schedule period as [Cron Expression](https://docs.oracle.com/cd/E12058_01/doc/doc.1014/e12030/cron_expressions.htm).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  0/5 * * * ? *
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_cron()
      :"faas.cron"

  ### Erlang

  ```erlang
  ?FAAS_CRON.
  'faas.cron'
  ```

  <!-- tabs-close -->
  """
  @spec faas_cron :: :"faas.cron"
  def faas_cron do
    :"faas.cron"
  end

  @doc """
  The name of the source on which the triggering operation was performed. For example, in Cloud Storage or S3 corresponds to the bucket name, and in Cosmos DB to the database name.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["myBucketName", "myDbName"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_document_collection()
      :"faas.document.collection"

  ### Erlang

  ```erlang
  ?FAAS_DOCUMENT_COLLECTION.
  'faas.document.collection'
  ```

  <!-- tabs-close -->
  """
  @spec faas_document_collection :: :"faas.document.collection"
  def faas_document_collection do
    :"faas.document.collection"
  end

  @doc """
  The document name/table subjected to the operation. For example, in Cloud Storage or S3 is the name of the file, and in Cosmos DB the table name.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["myFile.txt", "myTableName"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_document_name()
      :"faas.document.name"

  ### Erlang

  ```erlang
  ?FAAS_DOCUMENT_NAME.
  'faas.document.name'
  ```

  <!-- tabs-close -->
  """
  @spec faas_document_name :: :"faas.document.name"
  def faas_document_name do
    :"faas.document.name"
  end

  @typedoc """
  Describes the type of the operation that was performed on the data.

  ### Enum Values
  * `:insert` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - When a new object is created.
  * `:edit` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - When an object is modified.
  * `:delete` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - When an object is deleted.
  """
  @type faas_document_operation_values() :: %{
          :insert => :insert,
          :edit => :edit,
          :delete => :delete
        }
  @doc """
  Describes the type of the operation that was performed on the data.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_document_operation()
      :"faas.document.operation"

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_document_operation_values().insert
      :insert

      iex> %{OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_document_operation() => OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_document_operation_values().insert}
      %{:"faas.document.operation" => :insert}

  ### Erlang

  ```erlang
  ?FAAS_DOCUMENT_OPERATION.
  'faas.document.operation'

  ?FAAS_DOCUMENT_OPERATION_VALUES_INSERT.
  'insert'

  \#{?FAAS_DOCUMENT_OPERATION => ?FAAS_DOCUMENT_OPERATION_VALUES_INSERT}.
  \#{'faas.document.operation' => 'insert'}
  ```

  <!-- tabs-close -->
  """
  @spec faas_document_operation :: :"faas.document.operation"
  def faas_document_operation do
    :"faas.document.operation"
  end

  @spec faas_document_operation_values() :: faas_document_operation_values()
  def faas_document_operation_values() do
    %{
      :insert => :insert,
      :edit => :edit,
      :delete => :delete
    }
  end

  @doc """
  A string containing the time when the data was accessed in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  2020-01-23T13:47:06Z
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_document_time()
      :"faas.document.time"

  ### Erlang

  ```erlang
  ?FAAS_DOCUMENT_TIME.
  'faas.document.time'
  ```

  <!-- tabs-close -->
  """
  @spec faas_document_time :: :"faas.document.time"
  def faas_document_time do
    :"faas.document.time"
  end

  @doc """
  The execution environment ID as a string, that will be potentially reused for other invocations to the same function/function version.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  * **AWS Lambda:** Use the (full) log stream name.

  ### Examples

  ```
  ["2021/06/28/[$LATEST]2f399eb14537447da05ab2a2e39309de"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_instance()
      :"faas.instance"

  ### Erlang

  ```erlang
  ?FAAS_INSTANCE.
  'faas.instance'
  ```

  <!-- tabs-close -->
  """
  @spec faas_instance :: :"faas.instance"
  def faas_instance do
    :"faas.instance"
  end

  @doc """
  The invocation ID of the current function invocation.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  af9d5aa4-a685-4c5f-a22b-444f80b3cc28
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_invocation_id()
      :"faas.invocation_id"

  ### Erlang

  ```erlang
  ?FAAS_INVOCATION_ID.
  'faas.invocation_id'
  ```

  <!-- tabs-close -->
  """
  @spec faas_invocation_id :: :"faas.invocation_id"
  def faas_invocation_id do
    :"faas.invocation_id"
  end

  @doc """
  The name of the invoked function.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  SHOULD be equal to the `faas.name` resource attribute of the invoked function.

  ### Examples

  ```
  my-function
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_invoked_name()
      :"faas.invoked_name"

  ### Erlang

  ```erlang
  ?FAAS_INVOKED_NAME.
  'faas.invoked_name'
  ```

  <!-- tabs-close -->
  """
  @spec faas_invoked_name :: :"faas.invoked_name"
  def faas_invoked_name do
    :"faas.invoked_name"
  end

  @typedoc """
  The cloud provider of the invoked function.


  ### Enum Values
  * `:alibaba_cloud` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Alibaba Cloud
  * `:aws` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Amazon Web Services
  * `:azure` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Microsoft Azure
  * `:gcp` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Google Cloud Platform
  * `:tencent_cloud` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Tencent Cloud
  """
  @type faas_invoked_provider_values() :: %{
          :alibaba_cloud => :alibaba_cloud,
          :aws => :aws,
          :azure => :azure,
          :gcp => :gcp,
          :tencent_cloud => :tencent_cloud
        }
  @doc """
  The cloud provider of the invoked function.


  ### Notes

  SHOULD be equal to the `cloud.provider` resource attribute of the invoked function.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_invoked_provider()
      :"faas.invoked_provider"

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_invoked_provider_values().alibaba_cloud
      :alibaba_cloud

      iex> %{OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_invoked_provider() => OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_invoked_provider_values().alibaba_cloud}
      %{:"faas.invoked_provider" => :alibaba_cloud}

  ### Erlang

  ```erlang
  ?FAAS_INVOKED_PROVIDER.
  'faas.invoked_provider'

  ?FAAS_INVOKED_PROVIDER_VALUES_ALIBABA_CLOUD.
  'alibaba_cloud'

  \#{?FAAS_INVOKED_PROVIDER => ?FAAS_INVOKED_PROVIDER_VALUES_ALIBABA_CLOUD}.
  \#{'faas.invoked_provider' => 'alibaba_cloud'}
  ```

  <!-- tabs-close -->
  """
  @spec faas_invoked_provider :: :"faas.invoked_provider"
  def faas_invoked_provider do
    :"faas.invoked_provider"
  end

  @spec faas_invoked_provider_values() :: faas_invoked_provider_values()
  def faas_invoked_provider_values() do
    %{
      :alibaba_cloud => :alibaba_cloud,
      :aws => :aws,
      :azure => :azure,
      :gcp => :gcp,
      :tencent_cloud => :tencent_cloud
    }
  end

  @doc """
  The cloud region of the invoked function.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  SHOULD be equal to the `cloud.region` resource attribute of the invoked function.

  ### Examples

  ```
  eu-central-1
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_invoked_region()
      :"faas.invoked_region"

  ### Erlang

  ```erlang
  ?FAAS_INVOKED_REGION.
  'faas.invoked_region'
  ```

  <!-- tabs-close -->
  """
  @spec faas_invoked_region :: :"faas.invoked_region"
  def faas_invoked_region do
    :"faas.invoked_region"
  end

  @doc """
  The amount of memory available to the serverless function converted to Bytes.

  ### Value type

  Value must be of type `integer()`.
  ### Notes

  It's recommended to set this attribute since e.g. too little memory can easily stop a Java AWS Lambda function from working correctly. On AWS Lambda, the environment variable `AWS_LAMBDA_FUNCTION_MEMORY_SIZE` provides this information (which must be multiplied by 1,048,576).

  ### Examples

  ```
  134217728
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_max_memory()
      :"faas.max_memory"

  ### Erlang

  ```erlang
  ?FAAS_MAX_MEMORY.
  'faas.max_memory'
  ```

  <!-- tabs-close -->
  """
  @spec faas_max_memory :: :"faas.max_memory"
  def faas_max_memory do
    :"faas.max_memory"
  end

  @doc """
  The name of the single function that this runtime instance executes.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This is the name of the function as configured/deployed on the FaaS
  platform and is usually different from the name of the callback
  function (which may be stored in the
  [`code.namespace`/`code.function`](/docs/general/attributes.md#source-code-attributes)
  span attributes).

  For some cloud providers, the above definition is ambiguous. The following
  definition of function name **MUST** be used for this attribute
  (and consequently the span name) for the listed cloud providers/products:

  * **Azure:**  The full name `<FUNCAPP>/<FUNC>`, i.e., function app name
    followed by a forward slash followed by the function name (this form
    can also be seen in the resource JSON for the function).
    This means that a span attribute **MUST** be used, as an Azure function
    app can host multiple functions that would usually share
    a TracerProvider (see also the `cloud.resource_id` attribute).

  ### Examples

  ```
  ["my-function", "myazurefunctionapp/some-function-name"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_name()
      :"faas.name"

  ### Erlang

  ```erlang
  ?FAAS_NAME.
  'faas.name'
  ```

  <!-- tabs-close -->
  """
  @spec faas_name :: :"faas.name"
  def faas_name do
    :"faas.name"
  end

  @doc """
  A string containing the function invocation time in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  2020-01-23T13:47:06Z
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_time()
      :"faas.time"

  ### Erlang

  ```erlang
  ?FAAS_TIME.
  'faas.time'
  ```

  <!-- tabs-close -->
  """
  @spec faas_time :: :"faas.time"
  def faas_time do
    :"faas.time"
  end

  @typedoc """
  Type of the trigger which caused this function invocation.


  ### Enum Values
  * `:datasource` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - A response to some data source operation such as a database or filesystem read/write
  * `:http` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - To provide an answer to an inbound HTTP request
  * `:pubsub` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - A function is set to be executed when messages are sent to a messaging system
  * `:timer` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - A function is scheduled to be executed regularly
  * `:other` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - If none of the others apply
  """
  @type faas_trigger_values() :: %{
          :datasource => :datasource,
          :http => :http,
          :pubsub => :pubsub,
          :timer => :timer,
          :other => :other
        }
  @doc """
  Type of the trigger which caused this function invocation.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_trigger()
      :"faas.trigger"

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_trigger_values().datasource
      :datasource

      iex> %{OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_trigger() => OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_trigger_values().datasource}
      %{:"faas.trigger" => :datasource}

  ### Erlang

  ```erlang
  ?FAAS_TRIGGER.
  'faas.trigger'

  ?FAAS_TRIGGER_VALUES_DATASOURCE.
  'datasource'

  \#{?FAAS_TRIGGER => ?FAAS_TRIGGER_VALUES_DATASOURCE}.
  \#{'faas.trigger' => 'datasource'}
  ```

  <!-- tabs-close -->
  """
  @spec faas_trigger :: :"faas.trigger"
  def faas_trigger do
    :"faas.trigger"
  end

  @spec faas_trigger_values() :: faas_trigger_values()
  def faas_trigger_values() do
    %{
      :datasource => :datasource,
      :http => :http,
      :pubsub => :pubsub,
      :timer => :timer,
      :other => :other
    }
  end

  @doc """
  The immutable version of the function being executed.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Depending on the cloud provider and platform, use:

  * **AWS Lambda:** The [function version](https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html)
    (an integer represented as a decimal string).
  * **Google Cloud Run (Services):** The [revision](https://cloud.google.com/run/docs/managing/revisions)
    (i.e., the function name plus the revision suffix).
  * **Google Cloud Functions:** The value of the
    [`K_REVISION` environment variable](https://cloud.google.com/functions/docs/env-var#runtime_environment_variables_set_automatically).
  * **Azure Functions:** Not applicable. Do not set this attribute.

  ### Examples

  ```
  ["26", "pinkfroid-00002"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FAASAttributes.faas_version()
      :"faas.version"

  ### Erlang

  ```erlang
  ?FAAS_VERSION.
  'faas.version'
  ```

  <!-- tabs-close -->
  """
  @spec faas_version :: :"faas.version"
  def faas_version do
    :"faas.version"
  end
end
