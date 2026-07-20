defmodule OpenTelemetry.SemConv.Incubating.CpythonAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Cpython attributes.
  """

  @typedoc """
  Value of the garbage collector collection generation.

  ### Enum Values
  * `:generation_0` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Generation 0
  * `:generation_1` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Generation 1
  * `:generation_2` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Generation 2
  """
  @type cpython_gc_generation_values() :: %{
          :generation_0 => 0,
          :generation_1 => 1,
          :generation_2 => 2
        }
  @doc """
  Value of the garbage collector collection generation.

  ### Examples

  ```
  [0, 1, 2]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CpythonAttributes.cpython_gc_generation()
      :"cpython.gc.generation"

      iex> OpenTelemetry.SemConv.Incubating.CpythonAttributes.cpython_gc_generation_values().generation_0
      0

      iex> %{OpenTelemetry.SemConv.Incubating.CpythonAttributes.cpython_gc_generation() => OpenTelemetry.SemConv.Incubating.CpythonAttributes.cpython_gc_generation_values().generation_0}
      %{:"cpython.gc.generation" => 0}

  ### Erlang

  ```erlang
  ?CPYTHON_GC_GENERATION.
  'cpython.gc.generation'

  ?CPYTHON_GC_GENERATION_VALUES_GENERATION_0.
  '0'

  \#{?CPYTHON_GC_GENERATION => ?CPYTHON_GC_GENERATION_VALUES_GENERATION_0}.
  \#{'cpython.gc.generation' => '0'}
  ```

  <!-- tabs-close -->
  """
  @spec cpython_gc_generation :: :"cpython.gc.generation"
  def cpython_gc_generation do
    :"cpython.gc.generation"
  end

  @spec cpython_gc_generation_values() :: cpython_gc_generation_values()
  def cpython_gc_generation_values() do
    %{
      :generation_0 => 0,
      :generation_1 => 1,
      :generation_2 => 2
    }
  end
end
