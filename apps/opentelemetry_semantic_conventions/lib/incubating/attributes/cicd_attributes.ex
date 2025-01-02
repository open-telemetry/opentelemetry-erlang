defmodule OpenTelemetry.SemConv.Incubating.CicdAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Cicd attributes.
  """

  @doc """
  The human readable name of the pipeline within a CI/CD system.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Build and Test", "Lint", "Deploy Go Project", "deploy_to_environment"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CicdAttributes.cicd_pipeline_name()
      :"cicd.pipeline.name"

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_NAME.
  'cicd.pipeline.name'
  ```

  <!-- tabs-close -->
  """
  @spec cicd_pipeline_name :: :"cicd.pipeline.name"
  def cicd_pipeline_name do
    :"cicd.pipeline.name"
  end

  @doc """
  The unique identifier of a pipeline run within a CI/CD system.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["120912"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CicdAttributes.cicd_pipeline_run_id()
      :"cicd.pipeline.run.id"

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_RUN_ID.
  'cicd.pipeline.run.id'
  ```

  <!-- tabs-close -->
  """
  @spec cicd_pipeline_run_id :: :"cicd.pipeline.run.id"
  def cicd_pipeline_run_id do
    :"cicd.pipeline.run.id"
  end

  @doc """
  The human readable name of a task within a pipeline. Task here most closely aligns with a [computing process](https://en.wikipedia.org/wiki/Pipeline_(computing)) in a pipeline. Other terms for tasks include commands, steps, and procedures.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Run GoLang Linter", "Go Build", "go-test", "deploy_binary"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CicdAttributes.cicd_pipeline_task_name()
      :"cicd.pipeline.task.name"

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_TASK_NAME.
  'cicd.pipeline.task.name'
  ```

  <!-- tabs-close -->
  """
  @spec cicd_pipeline_task_name :: :"cicd.pipeline.task.name"
  def cicd_pipeline_task_name do
    :"cicd.pipeline.task.name"
  end

  @doc """
  The unique identifier of a task run within a pipeline.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["12097"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CicdAttributes.cicd_pipeline_task_run_id()
      :"cicd.pipeline.task.run.id"

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_TASK_RUN_ID.
  'cicd.pipeline.task.run.id'
  ```

  <!-- tabs-close -->
  """
  @spec cicd_pipeline_task_run_id :: :"cicd.pipeline.task.run.id"
  def cicd_pipeline_task_run_id do
    :"cicd.pipeline.task.run.id"
  end

  @doc """
  The [URL](https://en.wikipedia.org/wiki/URL) of the pipeline run providing the complete address in order to locate and identify the pipeline run.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["https://github.com/open-telemetry/semantic-conventions/actions/runs/9753949763/job/26920038674?pr=1075"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CicdAttributes.cicd_pipeline_task_run_url_full()
      :"cicd.pipeline.task.run.url.full"

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_TASK_RUN_URL_FULL.
  'cicd.pipeline.task.run.url.full'
  ```

  <!-- tabs-close -->
  """
  @spec cicd_pipeline_task_run_url_full :: :"cicd.pipeline.task.run.url.full"
  def cicd_pipeline_task_run_url_full do
    :"cicd.pipeline.task.run.url.full"
  end

  @typedoc """
  The type of the task within a pipeline.


  ### Enum Values
  * `:build` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - build
  * `:test` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - test
  * `:deploy` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - deploy
  """
  @type cicd_pipeline_task_type_values() :: %{
          :build => :build,
          :test => :test,
          :deploy => :deploy
        }
  @doc """
  The type of the task within a pipeline.


  ### Examples

  ```
  ["build", "test", "deploy"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CicdAttributes.cicd_pipeline_task_type()
      :"cicd.pipeline.task.type"

      iex> OpenTelemetry.SemConv.Incubating.CicdAttributes.cicd_pipeline_task_type_values().build
      :build

      iex> %{OpenTelemetry.SemConv.Incubating.CicdAttributes.cicd_pipeline_task_type() => OpenTelemetry.SemConv.Incubating.CicdAttributes.cicd_pipeline_task_type_values().build}
      %{:"cicd.pipeline.task.type" => :build}

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_TASK_TYPE.
  'cicd.pipeline.task.type'

  ?CICD_PIPELINE_TASK_TYPE_VALUES_BUILD.
  'build'

  \#{?CICD_PIPELINE_TASK_TYPE => ?CICD_PIPELINE_TASK_TYPE_VALUES_BUILD}.
  \#{'cicd.pipeline.task.type' => 'build'}
  ```

  <!-- tabs-close -->
  """
  @spec cicd_pipeline_task_type :: :"cicd.pipeline.task.type"
  def cicd_pipeline_task_type do
    :"cicd.pipeline.task.type"
  end

  @spec cicd_pipeline_task_type_values() :: cicd_pipeline_task_type_values()
  def cicd_pipeline_task_type_values() do
    %{
      :build => :build,
      :test => :test,
      :deploy => :deploy
    }
  end
end
