
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

%% The human readable name of the pipeline within a CI/CD system.
%%  
-define(CICD_PIPELINE_NAME, 'cicd.pipeline.name').


%% The unique identifier of a pipeline run within a CI/CD system.
%%  
-define(CICD_PIPELINE_RUN_ID, 'cicd.pipeline.run.id').


%% The human readable name of a task within a pipeline. Task here most closely aligns with a [computing process](https://en.wikipedia.org/wiki/Pipeline_(computing)) in a pipeline. Other terms for tasks include commands, steps, and procedures.
%%  
-define(CICD_PIPELINE_TASK_NAME, 'cicd.pipeline.task.name').


%% The unique identifier of a task run within a pipeline.
%%  
-define(CICD_PIPELINE_TASK_RUN_ID, 'cicd.pipeline.task.run.id').


%% The [URL](https://en.wikipedia.org/wiki/URL) of the pipeline run providing the complete address in order to locate and identify the pipeline run.
%%  
-define(CICD_PIPELINE_TASK_RUN_URL_FULL, 'cicd.pipeline.task.run.url.full').


%% The type of the task within a pipeline.
%%  
-define(CICD_PIPELINE_TASK_TYPE, 'cicd.pipeline.task.type').

-define(CICD_PIPELINE_TASK_TYPE_VALUES_BUILD, 'build').

-define(CICD_PIPELINE_TASK_TYPE_VALUES_TEST, 'test').

-define(CICD_PIPELINE_TASK_TYPE_VALUES_DEPLOY, 'deploy').

