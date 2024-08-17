
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
-define(FAAS_DOCUMENT_OPERATION, 'faas.document.operation').

-define(FAAS_DOCUMENT_OPERATION_VALUES_INSERT, 'insert').

-define(FAAS_DOCUMENT_OPERATION_VALUES_EDIT, 'edit').

-define(FAAS_DOCUMENT_OPERATION_VALUES_DELETE, 'delete').



%% A string containing the time when the data was accessed in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime).
%%  
-define(FAAS_DOCUMENT_TIME, 'faas.document.time').


%% The execution environment ID as a string, that will be potentially reused for other invocations to the same function/function version.
%%  
-define(FAAS_INSTANCE, 'faas.instance').


%% The invocation ID of the current function invocation.
%%  
-define(FAAS_INVOCATION_ID, 'faas.invocation_id').


%% The name of the invoked function.
%%  
-define(FAAS_INVOKED_NAME, 'faas.invoked_name').


%% The cloud provider of the invoked function.
%%  
-define(FAAS_INVOKED_PROVIDER, 'faas.invoked_provider').

-define(FAAS_INVOKED_PROVIDER_VALUES_ALIBABA_CLOUD, 'alibaba_cloud').

-define(FAAS_INVOKED_PROVIDER_VALUES_AWS, 'aws').

-define(FAAS_INVOKED_PROVIDER_VALUES_AZURE, 'azure').

-define(FAAS_INVOKED_PROVIDER_VALUES_GCP, 'gcp').

-define(FAAS_INVOKED_PROVIDER_VALUES_TENCENT_CLOUD, 'tencent_cloud').



%% The cloud region of the invoked function.
%%  
-define(FAAS_INVOKED_REGION, 'faas.invoked_region').


%% The amount of memory available to the serverless function converted to Bytes.
%%  
-define(FAAS_MAX_MEMORY, 'faas.max_memory').


%% The name of the single function that this runtime instance executes.
%%  
-define(FAAS_NAME, 'faas.name').


%% A string containing the function invocation time in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime).
%%  
-define(FAAS_TIME, 'faas.time').


%% Type of the trigger which caused this function invocation.
%%  
-define(FAAS_TRIGGER, 'faas.trigger').

-define(FAAS_TRIGGER_VALUES_DATASOURCE, 'datasource').

-define(FAAS_TRIGGER_VALUES_HTTP, 'http').

-define(FAAS_TRIGGER_VALUES_PUBSUB, 'pubsub').

-define(FAAS_TRIGGER_VALUES_TIMER, 'timer').

-define(FAAS_TRIGGER_VALUES_OTHER, 'other').



%% The immutable version of the function being executed.
-define(FAAS_VERSION, 'faas.version').
