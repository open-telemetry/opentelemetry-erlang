
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/security_rule_attributes.hrl").


%% A categorization value keyword used by the entity using the rule for detection of this event
%%  
-define(SECURITY_RULE_CATEGORY, 'security_rule.category').


%% The description of the rule generating the event.
%%  
-define(SECURITY_RULE_DESCRIPTION, 'security_rule.description').


%% Name of the license under which the rule used to generate this event is made available.
%%  
-define(SECURITY_RULE_LICENSE, 'security_rule.license').


%% The name of the rule or signature generating the event.
%%  
-define(SECURITY_RULE_NAME, 'security_rule.name').


%% Reference URL to additional information about the rule used to generate this event.
%%  
-define(SECURITY_RULE_REFERENCE, 'security_rule.reference').


%% Name of the ruleset, policy, group, or parent category in which the rule used to generate this event is a member.
%%  
-define(SECURITY_RULE_RULESET_NAME, 'security_rule.ruleset.name').


%% A rule ID that is unique within the scope of a set or group of agents, observers, or other entities using the rule for detection of this event.
%%  
-define(SECURITY_RULE_UUID, 'security_rule.uuid').


%% The version / revision of the rule being used for analysis.
%%  
-define(SECURITY_RULE_VERSION, 'security_rule.version').
