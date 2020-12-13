%%%------------------------------------------------------------------------
%% Copyright 2020, OpenTelemetry Authors
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
%%
%% @doc Resource detectors are responsible for reading in attributes about
%% the runtime environment of a node (such as an environment variable or
%% some metadata endpoint provided by a cloud host) and returning a
%% `otel_resource:t()' made from those attributes.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_resource_detector).

-callback get_resource(term()) -> otel_resource:t().
