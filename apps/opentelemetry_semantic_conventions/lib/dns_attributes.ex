defmodule OpenTelemetry.SemanticConventions.DnsAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Dns attributes.
  """

  @doc """
  The name being queried.
  ### Notes

  If the name field contains non-printable characters (below 32 or above 126), those characters should be represented as escaped base 10 integers (\DDD). Back slashes and quotes should be escaped. Tabs, carriage returns, and line feeds should be converted to \t, \r, and \n respectively.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DnsAttributes.dns_question_name()
      :"dns.question.name"
  """
  @spec dns_question_name :: :"dns.question.name"
  def dns_question_name do
    :"dns.question.name"
  end
end
