
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

%% The number of pipeline runs currently active in the system by state.
-define(CICD_PIPELINE_RUN_ACTIVE, 'cicd.pipeline.run.active').


%% Duration of a pipeline run grouped by pipeline, state and result.
-define(CICD_PIPELINE_RUN_DURATION, 'cicd.pipeline.run.duration').


%% The number of errors encountered in pipeline runs (eg. compile, test failures).
-define(CICD_PIPELINE_RUN_ERRORS, 'cicd.pipeline.run.errors').


%% The number of errors in a component of the CICD system (eg. controller, scheduler, agent).
-define(CICD_SYSTEM_ERRORS, 'cicd.system.errors').


%% The number of workers on the CICD system by state.
-define(CICD_WORKER_COUNT, 'cicd.worker.count').
