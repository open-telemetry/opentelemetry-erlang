defmodule OpenTelemetry.SemConv.Incubating.Metrics.CpuMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Cpu metrics.
  """

  @deprecated """
  Replaced by `system.cpu.frequency`.
  """

  @spec cpu_frequency :: :"cpu.frequency"
  def cpu_frequency do
    :"cpu.frequency"
  end

  @deprecated """
  Replaced by `system.cpu.time`.
  """

  @spec cpu_time :: :"cpu.time"
  def cpu_time do
    :"cpu.time"
  end

  @deprecated """
  Replaced by `system.cpu.utilization`.
  """

  @spec cpu_utilization :: :"cpu.utilization"
  def cpu_utilization do
    :"cpu.utilization"
  end
end
