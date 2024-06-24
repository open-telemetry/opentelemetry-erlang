defmodule OpenTelemetry.SemanticConventions.OpentracingAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Opentracing attributes.
  """

  @typedoc """
  Parent-child Reference type

  ### Options
  * `:child_of` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - The parent Span depends on the child Span in some capacity
  * `:follows_from` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - The parent Span doesn't depend in any way on the result of the child Span

  """
  @type opentracing_reftype() :: :child_of | :follows_from | atom()

  @doc """
  Parent-child Reference type
  ### Notes

  The causal relationship between a child Span and a parent Span.


  ### Example
      iex> OpenTelemetry.SemanticConventions.OpentracingAttributes.opentracing_reftype(:child_of)
      :child_of
      
      iex> OpenTelemetry.SemanticConventions.OpentracingAttributes.opentracing_reftype(:custom_value)
      :custom_value
  """
  @spec opentracing_reftype(opentracing_reftype()) :: :child_of | :follows_from | atom()
  def opentracing_reftype(option) do
    case option do
      :child_of -> :child_of
      :follows_from -> :follows_from
      _ -> option
    end
  end
end
