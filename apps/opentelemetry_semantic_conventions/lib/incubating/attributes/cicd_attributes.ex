defmodule OpenTelemetry.SemConv.Incubating.CICDAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for CICD attributes.
  """

  @typedoc """
  The kind of action a pipeline run is performing.


  ### Enum Values
  * `:build` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The pipeline run is executing a build.
  * `:run` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The pipeline run is executing.
  * `:sync` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The pipeline run is executing a sync.
  """
  @type cicd_pipeline_action_name_values() :: %{
          :build => :BUILD,
          :run => :RUN,
          :sync => :SYNC
        }
  @doc """
  The kind of action a pipeline run is performing.


  ### Examples

  ```
  ["BUILD", "RUN", "SYNC"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_action_name()
      :"cicd.pipeline.action.name"

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_action_name_values().build
      :BUILD

      iex> %{OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_action_name() => OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_action_name_values().build}
      %{:"cicd.pipeline.action.name" => :BUILD}

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_ACTION_NAME.
  'cicd.pipeline.action.name'

  ?CICD_PIPELINE_ACTION_NAME_VALUES_BUILD.
  'BUILD'

  \#{?CICD_PIPELINE_ACTION_NAME => ?CICD_PIPELINE_ACTION_NAME_VALUES_BUILD}.
  \#{'cicd.pipeline.action.name' => 'BUILD'}
  ```

  <!-- tabs-close -->
  """
  @spec cicd_pipeline_action_name :: :"cicd.pipeline.action.name"
  def cicd_pipeline_action_name do
    :"cicd.pipeline.action.name"
  end

  @spec cicd_pipeline_action_name_values() :: cicd_pipeline_action_name_values()
  def cicd_pipeline_action_name_values() do
    %{
      :build => :BUILD,
      :run => :RUN,
      :sync => :SYNC
    }
  end

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

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_name()
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

  @typedoc """
  The result of a pipeline run.


  ### Enum Values
  * `:success` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The pipeline run finished successfully.
  * `:failure` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The pipeline run did not finish successfully, eg. due to a compile error or a failing test. Such failures are usually detected by non-zero exit codes of the tools executed in the pipeline run.
  * `:error` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The pipeline run failed due to an error in the CICD system, eg. due to the worker being killed.
  * `:timeout` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - A timeout caused the pipeline run to be interrupted.
  * `:cancellation` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The pipeline run was cancelled, eg. by a user manually cancelling the pipeline run.
  * `:skip` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The pipeline run was skipped, eg. due to a precondition not being met.
  """
  @type cicd_pipeline_result_values() :: %{
          :success => :success,
          :failure => :failure,
          :error => :error,
          :timeout => :timeout,
          :cancellation => :cancellation,
          :skip => :skip
        }
  @doc """
  The result of a pipeline run.


  ### Examples

  ```
  ["success", "failure", "timeout", "skipped"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_result()
      :"cicd.pipeline.result"

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_result_values().success
      :success

      iex> %{OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_result() => OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_result_values().success}
      %{:"cicd.pipeline.result" => :success}

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_RESULT.
  'cicd.pipeline.result'

  ?CICD_PIPELINE_RESULT_VALUES_SUCCESS.
  'success'

  \#{?CICD_PIPELINE_RESULT => ?CICD_PIPELINE_RESULT_VALUES_SUCCESS}.
  \#{'cicd.pipeline.result' => 'success'}
  ```

  <!-- tabs-close -->
  """
  @spec cicd_pipeline_result :: :"cicd.pipeline.result"
  def cicd_pipeline_result do
    :"cicd.pipeline.result"
  end

  @spec cicd_pipeline_result_values() :: cicd_pipeline_result_values()
  def cicd_pipeline_result_values() do
    %{
      :success => :success,
      :failure => :failure,
      :error => :error,
      :timeout => :timeout,
      :cancellation => :cancellation,
      :skip => :skip
    }
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

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_run_id()
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

  @typedoc """
  The pipeline run goes through these states during its lifecycle.


  ### Enum Values
  * `:pending` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The run pending state spans from the event triggering the pipeline run until the execution of the run starts (eg. time spent in a queue, provisioning agents, creating run resources).

  * `:executing` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The executing state spans the execution of any run tasks (eg. build, test).
  * `:finalizing` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The finalizing state spans from when the run has finished executing (eg. cleanup of run resources).
  """
  @type cicd_pipeline_run_state_values() :: %{
          :pending => :pending,
          :executing => :executing,
          :finalizing => :finalizing
        }
  @doc """
  The pipeline run goes through these states during its lifecycle.


  ### Examples

  ```
  ["pending", "executing", "finalizing"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_run_state()
      :"cicd.pipeline.run.state"

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_run_state_values().pending
      :pending

      iex> %{OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_run_state() => OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_run_state_values().pending}
      %{:"cicd.pipeline.run.state" => :pending}

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_RUN_STATE.
  'cicd.pipeline.run.state'

  ?CICD_PIPELINE_RUN_STATE_VALUES_PENDING.
  'pending'

  \#{?CICD_PIPELINE_RUN_STATE => ?CICD_PIPELINE_RUN_STATE_VALUES_PENDING}.
  \#{'cicd.pipeline.run.state' => 'pending'}
  ```

  <!-- tabs-close -->
  """
  @spec cicd_pipeline_run_state :: :"cicd.pipeline.run.state"
  def cicd_pipeline_run_state do
    :"cicd.pipeline.run.state"
  end

  @spec cicd_pipeline_run_state_values() :: cicd_pipeline_run_state_values()
  def cicd_pipeline_run_state_values() do
    %{
      :pending => :pending,
      :executing => :executing,
      :finalizing => :finalizing
    }
  end

  @doc """
  The [URL](https://wikipedia.org/wiki/URL) of the pipeline run, providing the complete address in order to locate and identify the pipeline run.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["https://github.com/open-telemetry/semantic-conventions/actions/runs/9753949763?pr=1075"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_run_url_full()
      :"cicd.pipeline.run.url.full"

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_RUN_URL_FULL.
  'cicd.pipeline.run.url.full'
  ```

  <!-- tabs-close -->
  """
  @spec cicd_pipeline_run_url_full :: :"cicd.pipeline.run.url.full"
  def cicd_pipeline_run_url_full do
    :"cicd.pipeline.run.url.full"
  end

  @doc """
  The human readable name of a task within a pipeline. Task here most closely aligns with a [computing process](https://wikipedia.org/wiki/Pipeline_(computing)) in a pipeline. Other terms for tasks include commands, steps, and procedures.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Run GoLang Linter", "Go Build", "go-test", "deploy_binary"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_task_name()
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

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_task_run_id()
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

  @typedoc """
  The result of a task run.


  ### Enum Values
  * `:success` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The task run finished successfully.
  * `:failure` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The task run did not finish successfully, eg. due to a compile error or a failing test. Such failures are usually detected by non-zero exit codes of the tools executed in the task run.
  * `:error` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The task run failed due to an error in the CICD system, eg. due to the worker being killed.
  * `:timeout` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - A timeout caused the task run to be interrupted.
  * `:cancellation` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The task run was cancelled, eg. by a user manually cancelling the task run.
  * `:skip` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The task run was skipped, eg. due to a precondition not being met.
  """
  @type cicd_pipeline_task_run_result_values() :: %{
          :success => :success,
          :failure => :failure,
          :error => :error,
          :timeout => :timeout,
          :cancellation => :cancellation,
          :skip => :skip
        }
  @doc """
  The result of a task run.


  ### Examples

  ```
  ["success", "failure", "timeout", "skipped"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_task_run_result()
      :"cicd.pipeline.task.run.result"

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_task_run_result_values().success
      :success

      iex> %{OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_task_run_result() => OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_task_run_result_values().success}
      %{:"cicd.pipeline.task.run.result" => :success}

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_TASK_RUN_RESULT.
  'cicd.pipeline.task.run.result'

  ?CICD_PIPELINE_TASK_RUN_RESULT_VALUES_SUCCESS.
  'success'

  \#{?CICD_PIPELINE_TASK_RUN_RESULT => ?CICD_PIPELINE_TASK_RUN_RESULT_VALUES_SUCCESS}.
  \#{'cicd.pipeline.task.run.result' => 'success'}
  ```

  <!-- tabs-close -->
  """
  @spec cicd_pipeline_task_run_result :: :"cicd.pipeline.task.run.result"
  def cicd_pipeline_task_run_result do
    :"cicd.pipeline.task.run.result"
  end

  @spec cicd_pipeline_task_run_result_values() :: cicd_pipeline_task_run_result_values()
  def cicd_pipeline_task_run_result_values() do
    %{
      :success => :success,
      :failure => :failure,
      :error => :error,
      :timeout => :timeout,
      :cancellation => :cancellation,
      :skip => :skip
    }
  end

  @doc """
  The [URL](https://wikipedia.org/wiki/URL) of the pipeline task run, providing the complete address in order to locate and identify the pipeline task run.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["https://github.com/open-telemetry/semantic-conventions/actions/runs/9753949763/job/26920038674?pr=1075"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_task_run_url_full()
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

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_task_type()
      :"cicd.pipeline.task.type"

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_task_type_values().build
      :build

      iex> %{OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_task_type() => OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_pipeline_task_type_values().build}
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

  @doc """
  The name of a component of the CICD system.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["controller", "scheduler", "agent"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_system_component()
      :"cicd.system.component"

  ### Erlang

  ```erlang
  ?CICD_SYSTEM_COMPONENT.
  'cicd.system.component'
  ```

  <!-- tabs-close -->
  """
  @spec cicd_system_component :: :"cicd.system.component"
  def cicd_system_component do
    :"cicd.system.component"
  end

  @doc """
  The unique identifier of a worker within a CICD system.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["abc123", "10.0.1.2", "controller"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_worker_id()
      :"cicd.worker.id"

  ### Erlang

  ```erlang
  ?CICD_WORKER_ID.
  'cicd.worker.id'
  ```

  <!-- tabs-close -->
  """
  @spec cicd_worker_id :: :"cicd.worker.id"
  def cicd_worker_id do
    :"cicd.worker.id"
  end

  @doc """
  The name of a worker within a CICD system.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["agent-abc", "controller", "Ubuntu LTS"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_worker_name()
      :"cicd.worker.name"

  ### Erlang

  ```erlang
  ?CICD_WORKER_NAME.
  'cicd.worker.name'
  ```

  <!-- tabs-close -->
  """
  @spec cicd_worker_name :: :"cicd.worker.name"
  def cicd_worker_name do
    :"cicd.worker.name"
  end

  @typedoc """
  The state of a CICD worker / agent.


  ### Enum Values
  * `:available` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The worker is not performing work for the CICD system. It is available to the CICD system to perform work on (online / idle).
  * `:busy` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The worker is performing work for the CICD system.
  * `:offline` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The worker is not available to the CICD system (disconnected / down).
  """
  @type cicd_worker_state_values() :: %{
          :available => :available,
          :busy => :busy,
          :offline => :offline
        }
  @doc """
  The state of a CICD worker / agent.


  ### Examples

  ```
  ["idle", "busy", "down"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_worker_state()
      :"cicd.worker.state"

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_worker_state_values().available
      :available

      iex> %{OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_worker_state() => OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_worker_state_values().available}
      %{:"cicd.worker.state" => :available}

  ### Erlang

  ```erlang
  ?CICD_WORKER_STATE.
  'cicd.worker.state'

  ?CICD_WORKER_STATE_VALUES_AVAILABLE.
  'available'

  \#{?CICD_WORKER_STATE => ?CICD_WORKER_STATE_VALUES_AVAILABLE}.
  \#{'cicd.worker.state' => 'available'}
  ```

  <!-- tabs-close -->
  """
  @spec cicd_worker_state :: :"cicd.worker.state"
  def cicd_worker_state do
    :"cicd.worker.state"
  end

  @spec cicd_worker_state_values() :: cicd_worker_state_values()
  def cicd_worker_state_values() do
    %{
      :available => :available,
      :busy => :busy,
      :offline => :offline
    }
  end

  @doc """
  The [URL](https://wikipedia.org/wiki/URL) of the worker, providing the complete address in order to locate and identify the worker.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["https://cicd.example.org/worker/abc123"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CICDAttributes.cicd_worker_url_full()
      :"cicd.worker.url.full"

  ### Erlang

  ```erlang
  ?CICD_WORKER_URL_FULL.
  'cicd.worker.url.full'
  ```

  <!-- tabs-close -->
  """
  @spec cicd_worker_url_full :: :"cicd.worker.url.full"
  def cicd_worker_url_full do
    :"cicd.worker.url.full"
  end
end
