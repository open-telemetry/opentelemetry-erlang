
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/code_attributes.hrl").

%% @deprecated Replaced by `code.column.number`
%% Deprecated, use `code.column.number`
%%  
-define(CODE_COLUMN, 'code.column').

%% @deprecated Replaced by `code.file.path`
%% Deprecated, use `code.file.path` instead
%%  
-define(CODE_FILEPATH, 'code.filepath').

%% @deprecated Replaced by `code.function.name`
%% Deprecated, use `code.function.name` instead
%%  
-define(CODE_FUNCTION, 'code.function').

%% @deprecated Replaced by `code.line.number`
%% Deprecated, use `code.line.number` instead
%%  
-define(CODE_LINENO, 'code.lineno').

%% @deprecated Value should be included in `code.function.name` which is expected to be a fully-qualified name.
%% Deprecated, namespace is now included into `code.function.name`
%%  
-define(CODE_NAMESPACE, 'code.namespace').
