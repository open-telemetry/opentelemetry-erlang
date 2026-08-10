
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

%% The total number of objects collected inside a generation since interpreter start.
-define(CPYTHON_GC_COLLECTED_OBJECTS, 'cpython.gc.collected_objects').


%% The number of times a generation was collected since interpreter start.
-define(CPYTHON_GC_COLLECTIONS, 'cpython.gc.collections').


%% The total number of objects which were found to be uncollectable inside a generation since interpreter start.
-define(CPYTHON_GC_UNCOLLECTABLE_OBJECTS, 'cpython.gc.uncollectable_objects').
