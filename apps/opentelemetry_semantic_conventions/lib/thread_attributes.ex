defmodule OpenTelemetry.SemanticConventions.ThreadAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Thread attributes.
  """

  @doc """
  Current "managed" thread ID (as opposed to OS thread ID).



  ### Example
      iex> OpenTelemetry.SemanticConventions.ThreadAttributes.thread_id()
      :"thread.id"
  """
  @spec thread_id :: :"thread.id"
  def thread_id do
    :"thread.id"
  end

  @doc """
  Current thread name.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ThreadAttributes.thread_name()
      :"thread.name"
  """
  @spec thread_name :: :"thread.name"
  def thread_name do
    :"thread.name"
  end
end
