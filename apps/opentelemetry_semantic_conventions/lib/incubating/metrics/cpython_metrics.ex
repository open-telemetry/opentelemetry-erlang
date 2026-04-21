defmodule OpenTelemetry.SemConv.Incubating.Metrics.CpythonMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Cpython metrics.
  """
  @doc """
  The total number of objects collected inside a generation since interpreter start.

  Instrument: `counter`
  Unit: `{object}`
  ### Notes

  This metric reports data from [`gc.stats()`](https://docs.python.org/3/library/gc.html#gc.get_stats).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.CpythonMetrics.cpython_gc_collected_objects()
      :"cpython.gc.collected_objects"

  ### Erlang

  ```erlang
  ?CPYTHON_GC_COLLECTED_OBJECTS.
  'cpython.gc.collected_objects'
  ```

  <!-- tabs-close -->
  """

  @spec cpython_gc_collected_objects :: :"cpython.gc.collected_objects"
  def cpython_gc_collected_objects do
    :"cpython.gc.collected_objects"
  end

  @doc """
  The number of times a generation was collected since interpreter start.

  Instrument: `counter`
  Unit: `{collection}`
  ### Notes

  This metric reports data from [`gc.stats()`](https://docs.python.org/3/library/gc.html#gc.get_stats).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.CpythonMetrics.cpython_gc_collections()
      :"cpython.gc.collections"

  ### Erlang

  ```erlang
  ?CPYTHON_GC_COLLECTIONS.
  'cpython.gc.collections'
  ```

  <!-- tabs-close -->
  """

  @spec cpython_gc_collections :: :"cpython.gc.collections"
  def cpython_gc_collections do
    :"cpython.gc.collections"
  end

  @doc """
  The total number of objects which were found to be uncollectable inside a generation since interpreter start.

  Instrument: `counter`
  Unit: `{object}`
  ### Notes

  This metric reports data from [`gc.stats()`](https://docs.python.org/3/library/gc.html#gc.get_stats).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.CpythonMetrics.cpython_gc_uncollectable_objects()
      :"cpython.gc.uncollectable_objects"

  ### Erlang

  ```erlang
  ?CPYTHON_GC_UNCOLLECTABLE_OBJECTS.
  'cpython.gc.uncollectable_objects'
  ```

  <!-- tabs-close -->
  """

  @spec cpython_gc_uncollectable_objects :: :"cpython.gc.uncollectable_objects"
  def cpython_gc_uncollectable_objects do
    :"cpython.gc.uncollectable_objects"
  end
end
