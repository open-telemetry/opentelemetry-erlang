
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

%% The fully qualified human readable name of the [test case](https://en.wikipedia.org/wiki/Test_case).
%%  
-define(TEST_CASE_NAME, 'test.case.name').


%% The status of the actual test case result from test execution.
%%  
-define(TEST_CASE_RESULT_STATUS, 'test.case.result.status').

-define(TEST_CASE_RESULT_STATUS_VALUES_PASS, 'pass').

-define(TEST_CASE_RESULT_STATUS_VALUES_FAIL, 'fail').



%% The human readable name of a [test suite](https://en.wikipedia.org/wiki/Test_suite).
%%  
-define(TEST_SUITE_NAME, 'test.suite.name').


%% The status of the test suite run.
%%  
-define(TEST_SUITE_RUN_STATUS, 'test.suite.run.status').

-define(TEST_SUITE_RUN_STATUS_VALUES_SUCCESS, 'success').

-define(TEST_SUITE_RUN_STATUS_VALUES_FAILURE, 'failure').

-define(TEST_SUITE_RUN_STATUS_VALUES_SKIPPED, 'skipped').

-define(TEST_SUITE_RUN_STATUS_VALUES_ABORTED, 'aborted').

-define(TEST_SUITE_RUN_STATUS_VALUES_TIMED_OUT, 'timed_out').

-define(TEST_SUITE_RUN_STATUS_VALUES_IN_PROGRESS, 'in_progress').

