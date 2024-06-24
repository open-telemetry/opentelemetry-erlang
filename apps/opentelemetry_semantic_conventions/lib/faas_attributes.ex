defmodule OpenTelemetry.SemanticConventions.FaasAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Faas attributes.
  """

  @doc """
  A boolean that is true if the serverless function is executed for the first time (aka cold-start).



  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_coldstart()
      :"faas.coldstart"
  """
  @spec faas_coldstart :: :"faas.coldstart"
  def faas_coldstart do
    :"faas.coldstart"
  end

  @doc """
  A string containing the schedule period as [Cron Expression](https://docs.oracle.com/cd/E12058_01/doc/doc.1014/e12030/cron_expressions.htm).



  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_cron()
      :"faas.cron"
  """
  @spec faas_cron :: :"faas.cron"
  def faas_cron do
    :"faas.cron"
  end

  @doc """
  The name of the source on which the triggering operation was performed. For example, in Cloud Storage or S3 corresponds to the bucket name, and in Cosmos DB to the database name.



  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_document_collection()
      :"faas.document.collection"
  """
  @spec faas_document_collection :: :"faas.document.collection"
  def faas_document_collection do
    :"faas.document.collection"
  end

  @doc """
  The document name/table subjected to the operation. For example, in Cloud Storage or S3 is the name of the file, and in Cosmos DB the table name.



  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_document_name()
      :"faas.document.name"
  """
  @spec faas_document_name :: :"faas.document.name"
  def faas_document_name do
    :"faas.document.name"
  end

  @typedoc """
  Describes the type of the operation that was performed on the data.

  ### Options
  * `:insert` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When a new object is created.
  * `:edit` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When an object is modified.
  * `:delete` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When an object is deleted.

  """
  @type faas_document_operation() :: :insert | :edit | :delete | atom()

  @doc """
  Describes the type of the operation that was performed on the data.


  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_document_operation(:insert)
      :insert
      
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_document_operation(:custom_value)
      :custom_value
  """
  @spec faas_document_operation(faas_document_operation()) :: :insert | :edit | :delete | atom()
  def faas_document_operation(option) do
    :"faas.document.operation"

    case option do
      :insert -> :insert
      :edit -> :edit
      :delete -> :delete
      _ -> option
    end
  end

  @doc """
  A string containing the time when the data was accessed in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime).



  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_document_time()
      :"faas.document.time"
  """
  @spec faas_document_time :: :"faas.document.time"
  def faas_document_time do
    :"faas.document.time"
  end

  @doc """
  The execution environment ID as a string, that will be potentially reused for other invocations to the same function/function version.

  ### Notes

  * **AWS Lambda:** Use the (full) log stream name.


  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_instance()
      :"faas.instance"
  """
  @spec faas_instance :: :"faas.instance"
  def faas_instance do
    :"faas.instance"
  end

  @doc """
  The invocation ID of the current function invocation.



  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_invocationid()
      :"faas.invocation_id"
  """
  @spec faas_invocationid :: :"faas.invocation_id"
  def faas_invocationid do
    :"faas.invocation_id"
  end

  @doc """
  The name of the invoked function.

  ### Notes

  SHOULD be equal to the `faas.name` resource attribute of the invoked function.


  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_invokedname()
      :"faas.invoked_name"
  """
  @spec faas_invokedname :: :"faas.invoked_name"
  def faas_invokedname do
    :"faas.invoked_name"
  end

  @typedoc """
  The cloud provider of the invoked function.


  ### Options
  * `:alibaba_cloud` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Alibaba Cloud
  * `:aws` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Amazon Web Services
  * `:azure` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Microsoft Azure
  * `:gcp` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Google Cloud Platform
  * `:tencent_cloud` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Tencent Cloud

  """
  @type faas_invokedprovider() :: :alibaba_cloud | :aws | :azure | :gcp | :tencent_cloud | atom()

  @doc """
  The cloud provider of the invoked function.

  ### Notes

  SHOULD be equal to the `cloud.provider` resource attribute of the invoked function.


  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_invokedprovider(:alibaba_cloud)
      :alibaba_cloud
      
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_invokedprovider(:custom_value)
      :custom_value
  """
  @spec faas_invokedprovider(faas_invokedprovider()) ::
          :alibaba_cloud | :aws | :azure | :gcp | :tencent_cloud | atom()
  def faas_invokedprovider(option) do
    :"faas.invoked_provider"

    case option do
      :alibaba_cloud -> :alibaba_cloud
      :aws -> :aws
      :azure -> :azure
      :gcp -> :gcp
      :tencent_cloud -> :tencent_cloud
      _ -> option
    end
  end

  @doc """
  The cloud region of the invoked function.

  ### Notes

  SHOULD be equal to the `cloud.region` resource attribute of the invoked function.


  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_invokedregion()
      :"faas.invoked_region"
  """
  @spec faas_invokedregion :: :"faas.invoked_region"
  def faas_invokedregion do
    :"faas.invoked_region"
  end

  @doc """
  The amount of memory available to the serverless function converted to Bytes.

  ### Notes

  It's recommended to set this attribute since e.g. too little memory can easily stop a Java AWS Lambda function from working correctly. On AWS Lambda, the environment variable `AWS_LAMBDA_FUNCTION_MEMORY_SIZE` provides this information (which must be multiplied by 1,048,576).


  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_maxmemory()
      :"faas.max_memory"
  """
  @spec faas_maxmemory :: :"faas.max_memory"
  def faas_maxmemory do
    :"faas.max_memory"
  end

  @doc """
  The name of the single function that this runtime instance executes.

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


  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_name()
      :"faas.name"
  """
  @spec faas_name :: :"faas.name"
  def faas_name do
    :"faas.name"
  end

  @doc """
  A string containing the function invocation time in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime).



  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_time()
      :"faas.time"
  """
  @spec faas_time :: :"faas.time"
  def faas_time do
    :"faas.time"
  end

  @typedoc """
  Type of the trigger which caused this function invocation.


  ### Options
  * `:datasource` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - A response to some data source operation such as a database or filesystem read/write
  * `:http` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - To provide an answer to an inbound HTTP request
  * `:pubsub` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - A function is set to be executed when messages are sent to a messaging system
  * `:timer` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - A function is scheduled to be executed regularly
  * `:other` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - If none of the others apply

  """
  @type faas_trigger() :: :datasource | :http | :pubsub | :timer | :other | atom()

  @doc """
  Type of the trigger which caused this function invocation.



  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_trigger(:datasource)
      :datasource
      
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_trigger(:custom_value)
      :custom_value
  """
  @spec faas_trigger(faas_trigger()) :: :datasource | :http | :pubsub | :timer | :other | atom()
  def faas_trigger(option) do
    :"faas.trigger"

    case option do
      :datasource -> :datasource
      :http -> :http
      :pubsub -> :pubsub
      :timer -> :timer
      :other -> :other
      _ -> option
    end
  end

  @doc """
  The immutable version of the function being executed.
  ### Notes

  Depending on the cloud provider and platform, use:

  * **AWS Lambda:** The [function version](https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html)
    (an integer represented as a decimal string).
  * **Google Cloud Run (Services):** The [revision](https://cloud.google.com/run/docs/managing/revisions)
    (i.e., the function name plus the revision suffix).
  * **Google Cloud Functions:** The value of the
    [`K_REVISION` environment variable](https://cloud.google.com/functions/docs/env-var#runtime_environment_variables_set_automatically).
  * **Azure Functions:** Not applicable. Do not set this attribute.


  ### Example
      iex> OpenTelemetry.SemanticConventions.FaasAttributes.faas_version()
      :"faas.version"
  """
  @spec faas_version :: :"faas.version"
  def faas_version do
    :"faas.version"
  end
end
