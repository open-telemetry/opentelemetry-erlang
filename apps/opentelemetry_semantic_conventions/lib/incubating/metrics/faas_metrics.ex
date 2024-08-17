defmodule OpenTelemetry.SemConv.Incubating.Metrics.FAASMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for FAAS metrics.
  """
  @doc """
  Number of invocation cold starts

  Instrument: `counter`
  Unit: `{coldstart}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.FAASMetrics.faas_coldstarts()
      :"faas.coldstarts"

  ### Erlang

  ```erlang
  ?FAAS_COLDSTARTS.
  'faas.coldstarts'
  ```

  <!-- tabs-close -->
  """

  @spec faas_coldstarts :: :"faas.coldstarts"
  def faas_coldstarts do
    :"faas.coldstarts"
  end

  @doc """
  Distribution of CPU usage per invocation

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.FAASMetrics.faas_cpu_usage()
      :"faas.cpu_usage"

  ### Erlang

  ```erlang
  ?FAAS_CPU_USAGE.
  'faas.cpu_usage'
  ```

  <!-- tabs-close -->
  """

  @spec faas_cpu_usage :: :"faas.cpu_usage"
  def faas_cpu_usage do
    :"faas.cpu_usage"
  end

  @doc """
  Number of invocation errors

  Instrument: `counter`
  Unit: `{error}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.FAASMetrics.faas_errors()
      :"faas.errors"

  ### Erlang

  ```erlang
  ?FAAS_ERRORS.
  'faas.errors'
  ```

  <!-- tabs-close -->
  """

  @spec faas_errors :: :"faas.errors"
  def faas_errors do
    :"faas.errors"
  end

  @doc """
  Measures the duration of the function's initialization, such as a cold start

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.FAASMetrics.faas_init_duration()
      :"faas.init_duration"

  ### Erlang

  ```erlang
  ?FAAS_INIT_DURATION.
  'faas.init_duration'
  ```

  <!-- tabs-close -->
  """

  @spec faas_init_duration :: :"faas.init_duration"
  def faas_init_duration do
    :"faas.init_duration"
  end

  @doc """
  Number of successful invocations

  Instrument: `counter`
  Unit: `{invocation}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.FAASMetrics.faas_invocations()
      :"faas.invocations"

  ### Erlang

  ```erlang
  ?FAAS_INVOCATIONS.
  'faas.invocations'
  ```

  <!-- tabs-close -->
  """

  @spec faas_invocations :: :"faas.invocations"
  def faas_invocations do
    :"faas.invocations"
  end

  @doc """
  Measures the duration of the function's logic execution

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.FAASMetrics.faas_invoke_duration()
      :"faas.invoke_duration"

  ### Erlang

  ```erlang
  ?FAAS_INVOKE_DURATION.
  'faas.invoke_duration'
  ```

  <!-- tabs-close -->
  """

  @spec faas_invoke_duration :: :"faas.invoke_duration"
  def faas_invoke_duration do
    :"faas.invoke_duration"
  end

  @doc """
  Distribution of max memory usage per invocation

  Instrument: `histogram`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.FAASMetrics.faas_mem_usage()
      :"faas.mem_usage"

  ### Erlang

  ```erlang
  ?FAAS_MEM_USAGE.
  'faas.mem_usage'
  ```

  <!-- tabs-close -->
  """

  @spec faas_mem_usage :: :"faas.mem_usage"
  def faas_mem_usage do
    :"faas.mem_usage"
  end

  @doc """
  Distribution of net I/O usage per invocation

  Instrument: `histogram`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.FAASMetrics.faas_net_io()
      :"faas.net_io"

  ### Erlang

  ```erlang
  ?FAAS_NET_IO.
  'faas.net_io'
  ```

  <!-- tabs-close -->
  """

  @spec faas_net_io :: :"faas.net_io"
  def faas_net_io do
    :"faas.net_io"
  end

  @doc """
  Number of invocation timeouts

  Instrument: `counter`
  Unit: `{timeout}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.FAASMetrics.faas_timeouts()
      :"faas.timeouts"

  ### Erlang

  ```erlang
  ?FAAS_TIMEOUTS.
  'faas.timeouts'
  ```

  <!-- tabs-close -->
  """

  @spec faas_timeouts :: :"faas.timeouts"
  def faas_timeouts do
    :"faas.timeouts"
  end
end
