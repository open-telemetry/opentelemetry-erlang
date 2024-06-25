defmodule OpenTelemetry.SemanticConventions.OpentracingAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Opentracing attributes.
  """

  @typedoc """
  Parent-child Reference type

  ### Enum Values
  * `:child_of` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - The parent Span depends on the child Span in some capacity
  * `:follows_from` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - The parent Span doesn't depend in any way on the result of the child Span
  """
  @type opentracing_reftype() :: %{
          :child_of => :child_of,
          :follows_from => :follows_from
        }
  @doc """
  Parent-child Reference type
  ### Notes

  The causal relationship between a child Span and a parent Span.


  ### Example
      iex> OpenTelemetry.SemanticConventions.OpentracingAttributes.opentracing_reftype().child_of
      :child_of
      
      iex> OpenTelemetry.SemanticConventions.OpentracingAttributes.opentracing_reftype(:custom_value)
      :custom_value
  """
  @spec opentracing_reftype() :: opentracing_reftype()
  def opentracing_reftype() do
    %{
      :child_of => :child_of,
      :follows_from => :follows_from
    }
  end

  @spec opentracing_reftype(atom() | String.t()) :: atom() | String.t()
  def opentracing_reftype(custom_value) do
    custom_value
  end
end
