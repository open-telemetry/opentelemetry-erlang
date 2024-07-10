

%% A boolean that is true if the serverless function is executed for the first time (aka cold-start).
%%  
-define(FAAS_COLDSTART, 'faas.coldstart').


%% A string containing the schedule period as [Cron Expression](https://docs.oracle.com/cd/E12058_01/doc/doc.1014/e12030/cron_expressions.htm).
%%  
-define(FAAS_CRON, 'faas.cron').


%% The name of the source on which the triggering operation was performed. For example, in Cloud Storage or S3 corresponds to the bucket name, and in Cosmos DB to the database name.
%%  
-define(FAAS_DOCUMENT_COLLECTION, 'faas.document.collection').


%% The document name/table subjected to the operation. For example, in Cloud Storage or S3 is the name of the file, and in Cosmos DB the table name.
%%  
-define(FAAS_DOCUMENT_NAME, 'faas.document.name').


%% Describes the type of the operation that was performed on the data.

-define('faas_document_operation.insert', 'insert').

-define('faas_document_operation.edit', 'edit').

-define('faas_document_operation.delete', 'delete').

-define(faas_document_operation.(Custom), Custom).


%% A string containing the time when the data was accessed in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime).
%%  
-define(FAAS_DOCUMENT_TIME, 'faas.document.time').


%% The execution environment ID as a string, that will be potentially reused for other invocations to the same function/function version.
%%  
-define(FAAS_INSTANCE, 'faas.instance').


%% The invocation ID of the current function invocation.
%%  
-define(FAAS_INVOCATIONID, 'faas.invocation_id').


%% The name of the invoked function.
%%  
-define(FAAS_INVOKEDNAME, 'faas.invoked_name').


%% The cloud provider of the invoked function.
%%  

-define('faas_invokedprovider.alibaba_cloud', 'alibaba_cloud').

-define('faas_invokedprovider.aws', 'aws').

-define('faas_invokedprovider.azure', 'azure').

-define('faas_invokedprovider.gcp', 'gcp').

-define('faas_invokedprovider.tencent_cloud', 'tencent_cloud').

-define(faas_invokedprovider.(Custom), Custom).


%% The cloud region of the invoked function.
%%  
-define(FAAS_INVOKEDREGION, 'faas.invoked_region').


%% The amount of memory available to the serverless function converted to Bytes.
%%  
-define(FAAS_MAXMEMORY, 'faas.max_memory').


%% The name of the single function that this runtime instance executes.
%%  
-define(FAAS_NAME, 'faas.name').


%% A string containing the function invocation time in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime).
%%  
-define(FAAS_TIME, 'faas.time').


%% Type of the trigger which caused this function invocation.
%%  

-define('faas_trigger.datasource', 'datasource').

-define('faas_trigger.http', 'http').

-define('faas_trigger.pubsub', 'pubsub').

-define('faas_trigger.timer', 'timer').

-define('faas_trigger.other', 'other').

-define(faas_trigger.(Custom), Custom).


%% The immutable version of the function being executed.
-define(FAAS_VERSION, 'faas.version').
