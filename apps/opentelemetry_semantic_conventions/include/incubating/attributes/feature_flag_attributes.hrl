
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/feature_flag_attributes.hrl").


%% The unique identifier for the flag evaluation context. For example, the targeting key.
%%  
-define(FEATURE_FLAG_CONTEXT_ID, 'feature_flag.context.id').

%% @deprecated Replaced by `error.message`.
%% Deprecated, use `error.message` instead.
-define(FEATURE_FLAG_EVALUATION_ERROR_MESSAGE, 'feature_flag.evaluation.error.message').

%% @deprecated Replaced by `feature_flag.result.reason`.
%% Deprecated, use `feature_flag.result.reason` instead.
-define(FEATURE_FLAG_EVALUATION_REASON, 'feature_flag.evaluation.reason').

-define(FEATURE_FLAG_EVALUATION_REASON_VALUES_STATIC, 'static').

-define(FEATURE_FLAG_EVALUATION_REASON_VALUES_DEFAULT, 'default').

-define(FEATURE_FLAG_EVALUATION_REASON_VALUES_TARGETING_MATCH, 'targeting_match').

-define(FEATURE_FLAG_EVALUATION_REASON_VALUES_SPLIT, 'split').

-define(FEATURE_FLAG_EVALUATION_REASON_VALUES_CACHED, 'cached').

-define(FEATURE_FLAG_EVALUATION_REASON_VALUES_DISABLED, 'disabled').

-define(FEATURE_FLAG_EVALUATION_REASON_VALUES_UNKNOWN, 'unknown').

-define(FEATURE_FLAG_EVALUATION_REASON_VALUES_STALE, 'stale').

-define(FEATURE_FLAG_EVALUATION_REASON_VALUES_ERROR, 'error').



%% The lookup key of the feature flag.
-define(FEATURE_FLAG_KEY, 'feature_flag.key').


%% Identifies the feature flag provider.
-define(FEATURE_FLAG_PROVIDER_NAME, 'feature_flag.provider.name').


%% The reason code which shows how a feature flag value was determined.
%%  
-define(FEATURE_FLAG_RESULT_REASON, 'feature_flag.result.reason').

-define(FEATURE_FLAG_RESULT_REASON_VALUES_STATIC, 'static').

-define(FEATURE_FLAG_RESULT_REASON_VALUES_DEFAULT, 'default').

-define(FEATURE_FLAG_RESULT_REASON_VALUES_TARGETING_MATCH, 'targeting_match').

-define(FEATURE_FLAG_RESULT_REASON_VALUES_SPLIT, 'split').

-define(FEATURE_FLAG_RESULT_REASON_VALUES_CACHED, 'cached').

-define(FEATURE_FLAG_RESULT_REASON_VALUES_DISABLED, 'disabled').

-define(FEATURE_FLAG_RESULT_REASON_VALUES_UNKNOWN, 'unknown').

-define(FEATURE_FLAG_RESULT_REASON_VALUES_STALE, 'stale').

-define(FEATURE_FLAG_RESULT_REASON_VALUES_ERROR, 'error').



%% A semantic identifier for an evaluated flag value.
%%  
-define(FEATURE_FLAG_RESULT_VARIANT, 'feature_flag.result.variant').


%% The identifier of the [flag set](https://openfeature.dev/specification/glossary/#flag-set) to which the feature flag belongs.
%%  
-define(FEATURE_FLAG_SET_ID, 'feature_flag.set.id').

%% @deprecated Replaced by `feature_flag.result.variant`.
%% Deprecated, use `feature_flag.result.variant` instead.
-define(FEATURE_FLAG_VARIANT, 'feature_flag.variant').


%% The version of the ruleset used during the evaluation. This may be any stable value which uniquely identifies the ruleset.
%%  
-define(FEATURE_FLAG_VERSION, 'feature_flag.version').
