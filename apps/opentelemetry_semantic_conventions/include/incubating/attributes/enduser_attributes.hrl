
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/enduser_attributes.hrl").


%% Unique identifier of an end user in the system. It maybe a username, email address, or other identifier.
-define(ENDUSER_ID, 'enduser.id').


%% Pseudonymous identifier of an end user. This identifier should be a random value that is not directly linked or associated with the end user's actual identity.
%%  
-define(ENDUSER_PSEUDO_ID, 'enduser.pseudo.id').

%% @deprecated Replaced by `user.roles` attribute.
%% Deprecated, use `user.roles` instead.
-define(ENDUSER_ROLE, 'enduser.role').

%% @deprecated Removed.
%% Deprecated, no replacement at this time.
-define(ENDUSER_SCOPE, 'enduser.scope').
