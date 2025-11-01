
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/graphql_attributes.hrl").


%% The GraphQL document being executed.
-define(GRAPHQL_DOCUMENT, 'graphql.document').


%% The name of the operation being executed.
-define(GRAPHQL_OPERATION_NAME, 'graphql.operation.name').


%% The type of the operation being executed.
-define(GRAPHQL_OPERATION_TYPE, 'graphql.operation.type').

-define(GRAPHQL_OPERATION_TYPE_VALUES_QUERY, 'query').

-define(GRAPHQL_OPERATION_TYPE_VALUES_MUTATION, 'mutation').

-define(GRAPHQL_OPERATION_TYPE_VALUES_SUBSCRIPTION, 'subscription').

