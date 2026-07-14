
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

%% The number of changes (pull requests/merge requests/changelists) in a repository, categorized by their state (e.g. open or merged)
-define(VCS_CHANGE_COUNT, 'vcs.change.count').


%% The time duration a change (pull request/merge request/changelist) has been in a given state.
-define(VCS_CHANGE_DURATION, 'vcs.change.duration').


%% The amount of time since its creation it took a change (pull request/merge request/changelist) to get the first approval.
-define(VCS_CHANGE_TIME_TO_APPROVAL, 'vcs.change.time_to_approval').


%% The amount of time since its creation it took a change (pull request/merge request/changelist) to get merged into the target(base) ref.
-define(VCS_CHANGE_TIME_TO_MERGE, 'vcs.change.time_to_merge').


%% The number of unique contributors to a repository
-define(VCS_CONTRIBUTOR_COUNT, 'vcs.contributor.count').


%% The number of refs of type branch or tag in a repository.
-define(VCS_REF_COUNT, 'vcs.ref.count').


%% The number of lines added/removed in a ref (branch) relative to the ref from the `vcs.ref.base.name` attribute.
-define(VCS_REF_LINES_DELTA, 'vcs.ref.lines_delta').


%% The number of revisions (commits) a ref (branch) is ahead/behind the branch from the `vcs.ref.base.name` attribute
-define(VCS_REF_REVISIONS_DELTA, 'vcs.ref.revisions_delta').


%% Time a ref (branch) created from the default branch (trunk) has existed. The `ref.type` attribute will always be `branch`
-define(VCS_REF_TIME, 'vcs.ref.time').


%% The number of repositories in an organization.
-define(VCS_REPOSITORY_COUNT, 'vcs.repository.count').
