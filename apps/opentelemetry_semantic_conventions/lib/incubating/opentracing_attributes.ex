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
  @type opentracing_reftype_values() :: %{
          :child_of => :child_of,
          :follows_from => :follows_from
        }
  @doc """
  Parent-child Reference type

  ### Notes

  The causal relationship between a child Span and a parent Span.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OpentracingAttributes.opentracing_reftype()
      :"opentracing.ref_type"
      
      iex> OpenTelemetry.SemConv.Incubating.OpentracingAttributes.opentracing_reftype_values().child_of
      :child_of
      
      iex> %{OpenTelemetry.SemConv.Incubating.OpentracingAttributes.opentracing_reftype() => OpenTelemetry.SemConv.Incubating.OpentracingAttributes.opentracing_reftype_values().child_of}
      %{:"opentracing.ref_type" => :child_of}
      
      iex> OpenTelemetry.SemConv.Incubating.OpentracingAttributes.opentracing_reftype_values(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?OPENTRACING_REFTYPE.
  'opentracing.ref_type'

  \#{?OPENTRACING_REFTYPE => ?OPENTRACING_REFTYPE_VALUES.child_of}.
  \#{'opentracing.ref_type' => child_of}

  ?'OPENTRACING_REFTYPE_VALUES.child_of'.
  child_of

  ?OPENTRACING_REFTYPE_VALUES(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec opentracing_reftype :: :"opentracing.ref_type"
  def opentracing_reftype do
    :"opentracing.ref_type"
  end

  @spec opentracing_reftype_values() :: opentracing_reftype_values()
  def opentracing_reftype_values() do
    %{
      :child_of => :child_of,
      :follows_from => :follows_from
    }
  end

  @spec opentracing_reftype_values(atom() | String.t()) :: atom() | String.t()
  def opentracing_reftype_values(custom_value) do
    custom_value
  end
end
