defmodule OpenTelemetry.SemConv.Incubating.OpentracingAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Opentracing attributes.
  """

  @typedoc """
  Parent-child Reference type

  ### Enum Values
  * `:child_of` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The parent Span depends on the child Span in some capacity
  * `:follows_from` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The parent Span doesn't depend in any way on the result of the child Span
  """
  @type opentracing_ref_type_values() :: %{
          :child_of => :child_of,
          :follows_from => :follows_from
        }
  @doc """
  Parent-child Reference type

  ### Notes

  The causal relationship between a child Span and a parent Span.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OpentracingAttributes.opentracing_ref_type()
      :"opentracing.ref_type"

      iex> OpenTelemetry.SemConv.Incubating.OpentracingAttributes.opentracing_ref_type_values().child_of
      :child_of

      iex> %{OpenTelemetry.SemConv.Incubating.OpentracingAttributes.opentracing_ref_type() => OpenTelemetry.SemConv.Incubating.OpentracingAttributes.opentracing_ref_type_values().child_of}
      %{:"opentracing.ref_type" => :child_of}

  ### Erlang

  ```erlang
  ?OPENTRACING_REF_TYPE.
  'opentracing.ref_type'

  ?OPENTRACING_REF_TYPE_VALUES_CHILD_OF.
  'child_of'

  \#{?OPENTRACING_REF_TYPE => ?OPENTRACING_REF_TYPE_VALUES_CHILD_OF}.
  \#{'opentracing.ref_type' => 'child_of'}
  ```

  <!-- tabs-close -->
  """
  @spec opentracing_ref_type :: :"opentracing.ref_type"
  def opentracing_ref_type do
    :"opentracing.ref_type"
  end

  @spec opentracing_ref_type_values() :: opentracing_ref_type_values()
  def opentracing_ref_type_values() do
    %{
      :child_of => :child_of,
      :follows_from => :follows_from
    }
  end
end
