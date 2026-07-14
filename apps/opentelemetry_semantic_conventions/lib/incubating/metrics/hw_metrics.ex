defmodule OpenTelemetry.SemConv.Incubating.Metrics.HWMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for HW metrics.
  """
  @doc """
  Energy consumed by the component

  Instrument: `counter`
  Unit: `J`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HWMetrics.hw_energy()
      :"hw.energy"

  ### Erlang

  ```erlang
  ?HW_ENERGY.
  'hw.energy'
  ```

  <!-- tabs-close -->
  """

  @spec hw_energy :: :"hw.energy"
  def hw_energy do
    :"hw.energy"
  end

  @doc """
  Number of errors encountered by the component

  Instrument: `counter`
  Unit: `{error}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HWMetrics.hw_errors()
      :"hw.errors"

  ### Erlang

  ```erlang
  ?HW_ERRORS.
  'hw.errors'
  ```

  <!-- tabs-close -->
  """

  @spec hw_errors :: :"hw.errors"
  def hw_errors do
    :"hw.errors"
  end

  @doc """
  Ambient (external) temperature of the physical host

  Instrument: `gauge`
  Unit: `Cel`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HWMetrics.hw_host_ambient_temperature()
      :"hw.host.ambient_temperature"

  ### Erlang

  ```erlang
  ?HW_HOST_AMBIENT_TEMPERATURE.
  'hw.host.ambient_temperature'
  ```

  <!-- tabs-close -->
  """

  @spec hw_host_ambient_temperature :: :"hw.host.ambient_temperature"
  def hw_host_ambient_temperature do
    :"hw.host.ambient_temperature"
  end

  @doc """
  Total energy consumed by the entire physical host, in joules

  Instrument: `counter`
  Unit: `J`
  ### Notes

  The overall energy usage of a host **MUST** be reported using the specific `hw.host.energy` and `hw.host.power` metrics **only**, instead of the generic `hw.energy` and `hw.power` described in the previous section, to prevent summing up overlapping values.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HWMetrics.hw_host_energy()
      :"hw.host.energy"

  ### Erlang

  ```erlang
  ?HW_HOST_ENERGY.
  'hw.host.energy'
  ```

  <!-- tabs-close -->
  """

  @spec hw_host_energy :: :"hw.host.energy"
  def hw_host_energy do
    :"hw.host.energy"
  end

  @doc """
  By how many degrees Celsius the temperature of the physical host can be increased, before reaching a warning threshold on one of the internal sensors


  Instrument: `gauge`
  Unit: `Cel`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HWMetrics.hw_host_heating_margin()
      :"hw.host.heating_margin"

  ### Erlang

  ```erlang
  ?HW_HOST_HEATING_MARGIN.
  'hw.host.heating_margin'
  ```

  <!-- tabs-close -->
  """

  @spec hw_host_heating_margin :: :"hw.host.heating_margin"
  def hw_host_heating_margin do
    :"hw.host.heating_margin"
  end

  @doc """
  Instantaneous power consumed by the entire physical host in Watts (`hw.host.energy` is preferred)


  Instrument: `gauge`
  Unit: `W`
  ### Notes

  The overall energy usage of a host **MUST** be reported using the specific `hw.host.energy` and `hw.host.power` metrics **only**, instead of the generic `hw.energy` and `hw.power` described in the previous section, to prevent summing up overlapping values.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HWMetrics.hw_host_power()
      :"hw.host.power"

  ### Erlang

  ```erlang
  ?HW_HOST_POWER.
  'hw.host.power'
  ```

  <!-- tabs-close -->
  """

  @spec hw_host_power :: :"hw.host.power"
  def hw_host_power do
    :"hw.host.power"
  end

  @doc """
  Instantaneous power consumed by the component

  Instrument: `gauge`
  Unit: `W`
  ### Notes

  It is recommended to report `hw.energy` instead of `hw.power` when possible.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HWMetrics.hw_power()
      :"hw.power"

  ### Erlang

  ```erlang
  ?HW_POWER.
  'hw.power'
  ```

  <!-- tabs-close -->
  """

  @spec hw_power :: :"hw.power"
  def hw_power do
    :"hw.power"
  end

  @doc """
  Operational status: `1` (true) or `0` (false) for each of the possible states

  Instrument: `updowncounter`
  Unit: `1`
  ### Notes

  `hw.status` is currently specified as an *UpDownCounter* but would ideally be represented using a [*StateSet* as defined in OpenMetrics](https://github.com/prometheus/OpenMetrics/blob/v1.0.0/specification/OpenMetrics.md#stateset). This semantic convention will be updated once *StateSet* is specified in OpenTelemetry. This planned change is not expected to have any consequence on the way users query their timeseries backend to retrieve the values of `hw.status` over time.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HWMetrics.hw_status()
      :"hw.status"

  ### Erlang

  ```erlang
  ?HW_STATUS.
  'hw.status'
  ```

  <!-- tabs-close -->
  """

  @spec hw_status :: :"hw.status"
  def hw_status do
    :"hw.status"
  end
end
