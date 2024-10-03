defmodule OpenTelemetry.SemConv.Incubating.TestAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Test attributes.
  """

  @doc """
  The fully qualified human readable name of the [test case](https://en.wikipedia.org/wiki/Test_case).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["org.example.TestCase1.test1", "example/tests/TestCase1.test1", "ExampleTestCase1_test1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TestAttributes.test_case_name()
      :"test.case.name"

  ### Erlang

  ```erlang
  ?TEST_CASE_NAME.
  'test.case.name'
  ```

  <!-- tabs-close -->
  """
  @spec test_case_name :: :"test.case.name"
  def test_case_name do
    :"test.case.name"
  end

  @typedoc """
  The status of the actual test case result from test execution.


  ### Enum Values
  * `:pass` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - pass
  * `:fail` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - fail
  """
  @type test_case_result_status_values() :: %{
          :pass => :pass,
          :fail => :fail
        }
  @doc """
  The status of the actual test case result from test execution.


  ### Examples

  ```
  ["pass", "fail"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TestAttributes.test_case_result_status()
      :"test.case.result.status"

      iex> OpenTelemetry.SemConv.Incubating.TestAttributes.test_case_result_status_values().pass
      :pass

      iex> %{OpenTelemetry.SemConv.Incubating.TestAttributes.test_case_result_status() => OpenTelemetry.SemConv.Incubating.TestAttributes.test_case_result_status_values().pass}
      %{:"test.case.result.status" => :pass}

  ### Erlang

  ```erlang
  ?TEST_CASE_RESULT_STATUS.
  'test.case.result.status'

  ?TEST_CASE_RESULT_STATUS_VALUES_PASS.
  'pass'

  \#{?TEST_CASE_RESULT_STATUS => ?TEST_CASE_RESULT_STATUS_VALUES_PASS}.
  \#{'test.case.result.status' => 'pass'}
  ```

  <!-- tabs-close -->
  """
  @spec test_case_result_status :: :"test.case.result.status"
  def test_case_result_status do
    :"test.case.result.status"
  end

  @spec test_case_result_status_values() :: test_case_result_status_values()
  def test_case_result_status_values() do
    %{
      :pass => :pass,
      :fail => :fail
    }
  end

  @doc """
  The human readable name of a [test suite](https://en.wikipedia.org/wiki/Test_suite).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["TestSuite1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TestAttributes.test_suite_name()
      :"test.suite.name"

  ### Erlang

  ```erlang
  ?TEST_SUITE_NAME.
  'test.suite.name'
  ```

  <!-- tabs-close -->
  """
  @spec test_suite_name :: :"test.suite.name"
  def test_suite_name do
    :"test.suite.name"
  end

  @typedoc """
  The status of the test suite run.


  ### Enum Values
  * `:success` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - success
  * `:failure` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - failure
  * `:skipped` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - skipped
  * `:aborted` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - aborted
  * `:timed_out` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - timed_out
  * `:in_progress` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - in_progress
  """
  @type test_suite_run_status_values() :: %{
          :success => :success,
          :failure => :failure,
          :skipped => :skipped,
          :aborted => :aborted,
          :timed_out => :timed_out,
          :in_progress => :in_progress
        }
  @doc """
  The status of the test suite run.


  ### Examples

  ```
  ["success", "failure", "skipped", "aborted", "timed_out", "in_progress"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TestAttributes.test_suite_run_status()
      :"test.suite.run.status"

      iex> OpenTelemetry.SemConv.Incubating.TestAttributes.test_suite_run_status_values().success
      :success

      iex> %{OpenTelemetry.SemConv.Incubating.TestAttributes.test_suite_run_status() => OpenTelemetry.SemConv.Incubating.TestAttributes.test_suite_run_status_values().success}
      %{:"test.suite.run.status" => :success}

  ### Erlang

  ```erlang
  ?TEST_SUITE_RUN_STATUS.
  'test.suite.run.status'

  ?TEST_SUITE_RUN_STATUS_VALUES_SUCCESS.
  'success'

  \#{?TEST_SUITE_RUN_STATUS => ?TEST_SUITE_RUN_STATUS_VALUES_SUCCESS}.
  \#{'test.suite.run.status' => 'success'}
  ```

  <!-- tabs-close -->
  """
  @spec test_suite_run_status :: :"test.suite.run.status"
  def test_suite_run_status do
    :"test.suite.run.status"
  end

  @spec test_suite_run_status_values() :: test_suite_run_status_values()
  def test_suite_run_status_values() do
    %{
      :success => :success,
      :failure => :failure,
      :skipped => :skipped,
      :aborted => :aborted,
      :timed_out => :timed_out,
      :in_progress => :in_progress
    }
  end
end
