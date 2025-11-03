
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/cicd_attributes.hrl").


%% The kind of action a pipeline run is performing.
%%  
-define(CICD_PIPELINE_ACTION_NAME, 'cicd.pipeline.action.name').

-define(CICD_PIPELINE_ACTION_NAME_VALUES_BUILD, 'BUILD').

-define(CICD_PIPELINE_ACTION_NAME_VALUES_RUN, 'RUN').

-define(CICD_PIPELINE_ACTION_NAME_VALUES_SYNC, 'SYNC').



%% The human readable name of the pipeline within a CI/CD system.
%%  
-define(CICD_PIPELINE_NAME, 'cicd.pipeline.name').


%% The result of a pipeline run.
%%  
-define(CICD_PIPELINE_RESULT, 'cicd.pipeline.result').

-define(CICD_PIPELINE_RESULT_VALUES_SUCCESS, 'success').

-define(CICD_PIPELINE_RESULT_VALUES_FAILURE, 'failure').

-define(CICD_PIPELINE_RESULT_VALUES_ERROR, 'error').

-define(CICD_PIPELINE_RESULT_VALUES_TIMEOUT, 'timeout').

-define(CICD_PIPELINE_RESULT_VALUES_CANCELLATION, 'cancellation').

-define(CICD_PIPELINE_RESULT_VALUES_SKIP, 'skip').



%% The unique identifier of a pipeline run within a CI/CD system.
%%  
-define(CICD_PIPELINE_RUN_ID, 'cicd.pipeline.run.id').


%% The pipeline run goes through these states during its lifecycle.
%%  
-define(CICD_PIPELINE_RUN_STATE, 'cicd.pipeline.run.state').

-define(CICD_PIPELINE_RUN_STATE_VALUES_PENDING, 'pending').

-define(CICD_PIPELINE_RUN_STATE_VALUES_EXECUTING, 'executing').

-define(CICD_PIPELINE_RUN_STATE_VALUES_FINALIZING, 'finalizing').



%% The [URL](https://wikipedia.org/wiki/URL) of the pipeline run, providing the complete address in order to locate and identify the pipeline run.
%%  
-define(CICD_PIPELINE_RUN_URL_FULL, 'cicd.pipeline.run.url.full').


%% The human readable name of a task within a pipeline. Task here most closely aligns with a [computing process](https://wikipedia.org/wiki/Pipeline_(computing)) in a pipeline. Other terms for tasks include commands, steps, and procedures.
%%  
-define(CICD_PIPELINE_TASK_NAME, 'cicd.pipeline.task.name').


%% The unique identifier of a task run within a pipeline.
%%  
-define(CICD_PIPELINE_TASK_RUN_ID, 'cicd.pipeline.task.run.id').


%% The result of a task run.
%%  
-define(CICD_PIPELINE_TASK_RUN_RESULT, 'cicd.pipeline.task.run.result').

-define(CICD_PIPELINE_TASK_RUN_RESULT_VALUES_SUCCESS, 'success').

-define(CICD_PIPELINE_TASK_RUN_RESULT_VALUES_FAILURE, 'failure').

-define(CICD_PIPELINE_TASK_RUN_RESULT_VALUES_ERROR, 'error').

-define(CICD_PIPELINE_TASK_RUN_RESULT_VALUES_TIMEOUT, 'timeout').

-define(CICD_PIPELINE_TASK_RUN_RESULT_VALUES_CANCELLATION, 'cancellation').

-define(CICD_PIPELINE_TASK_RUN_RESULT_VALUES_SKIP, 'skip').



%% The [URL](https://wikipedia.org/wiki/URL) of the pipeline task run, providing the complete address in order to locate and identify the pipeline task run.
%%  
-define(CICD_PIPELINE_TASK_RUN_URL_FULL, 'cicd.pipeline.task.run.url.full').


%% The type of the task within a pipeline.
%%  
-define(CICD_PIPELINE_TASK_TYPE, 'cicd.pipeline.task.type').

-define(CICD_PIPELINE_TASK_TYPE_VALUES_BUILD, 'build').

-define(CICD_PIPELINE_TASK_TYPE_VALUES_TEST, 'test').

-define(CICD_PIPELINE_TASK_TYPE_VALUES_DEPLOY, 'deploy').



%% The name of a component of the CICD system.
-define(CICD_SYSTEM_COMPONENT, 'cicd.system.component').


%% The unique identifier of a worker within a CICD system.
-define(CICD_WORKER_ID, 'cicd.worker.id').


%% The name of a worker within a CICD system.
-define(CICD_WORKER_NAME, 'cicd.worker.name').


%% The state of a CICD worker / agent.
%%  
-define(CICD_WORKER_STATE, 'cicd.worker.state').

-define(CICD_WORKER_STATE_VALUES_AVAILABLE, 'available').

-define(CICD_WORKER_STATE_VALUES_BUSY, 'busy').

-define(CICD_WORKER_STATE_VALUES_OFFLINE, 'offline').



%% The [URL](https://wikipedia.org/wiki/URL) of the worker, providing the complete address in order to locate and identify the worker.
-define(CICD_WORKER_URL_FULL, 'cicd.worker.url.full').
