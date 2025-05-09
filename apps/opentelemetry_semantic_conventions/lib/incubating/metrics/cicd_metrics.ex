defmodule OpenTelemetry.SemConv.Incubating.Metrics.CICDMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for CICD metrics.
  """
  @doc """
  The number of pipeline runs currently active in the system by state.

  Instrument: `updowncounter`
  Unit: `{run}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.CICDMetrics.cicd_pipeline_run_active()
      :"cicd.pipeline.run.active"

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_RUN_ACTIVE.
  'cicd.pipeline.run.active'
  ```

  <!-- tabs-close -->
  """

  @spec cicd_pipeline_run_active :: :"cicd.pipeline.run.active"
  def cicd_pipeline_run_active do
    :"cicd.pipeline.run.active"
  end

  @doc """
  Duration of a pipeline run grouped by pipeline, state and result.

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.CICDMetrics.cicd_pipeline_run_duration()
      :"cicd.pipeline.run.duration"

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_RUN_DURATION.
  'cicd.pipeline.run.duration'
  ```

  <!-- tabs-close -->
  """

  @spec cicd_pipeline_run_duration :: :"cicd.pipeline.run.duration"
  def cicd_pipeline_run_duration do
    :"cicd.pipeline.run.duration"
  end

  @doc """
  The number of errors encountered in pipeline runs (eg. compile, test failures).

  Instrument: `counter`
  Unit: `{error}`
  ### Notes

  There might be errors in a pipeline run that are non fatal (eg. they are suppressed) or in a parallel stage multiple stages could have a fatal error.
  This means that this error count might not be the same as the count of metric `cicd.pipeline.run.duration` with run result `failure`.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.CICDMetrics.cicd_pipeline_run_errors()
      :"cicd.pipeline.run.errors"

  ### Erlang

  ```erlang
  ?CICD_PIPELINE_RUN_ERRORS.
  'cicd.pipeline.run.errors'
  ```

  <!-- tabs-close -->
  """

  @spec cicd_pipeline_run_errors :: :"cicd.pipeline.run.errors"
  def cicd_pipeline_run_errors do
    :"cicd.pipeline.run.errors"
  end

  @doc """
  The number of errors in a component of the CICD system (eg. controller, scheduler, agent).

  Instrument: `counter`
  Unit: `{error}`
  ### Notes

  Errors in pipeline run execution are explicitly excluded. Ie a test failure is not counted in this metric.

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.CICDMetrics.cicd_system_errors()
      :"cicd.system.errors"

  ### Erlang

  ```erlang
  ?CICD_SYSTEM_ERRORS.
  'cicd.system.errors'
  ```

  <!-- tabs-close -->
  """

  @spec cicd_system_errors :: :"cicd.system.errors"
  def cicd_system_errors do
    :"cicd.system.errors"
  end

  @doc """
  The number of workers on the CICD system by state.

  Instrument: `updowncounter`
  Unit: `{count}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.CICDMetrics.cicd_worker_count()
      :"cicd.worker.count"

  ### Erlang

  ```erlang
  ?CICD_WORKER_COUNT.
  'cicd.worker.count'
  ```

  <!-- tabs-close -->
  """

  @spec cicd_worker_count :: :"cicd.worker.count"
  def cicd_worker_count do
    :"cicd.worker.count"
  end
end
