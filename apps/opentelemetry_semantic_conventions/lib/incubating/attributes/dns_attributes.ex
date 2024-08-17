defmodule OpenTelemetry.SemConv.Incubating.DNSAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for DNS attributes.
  """

  @doc """
  The name being queried.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  If the name field contains non-printable characters (below 32 or above 126), those characters should be represented as escaped base 10 integers (\DDD). Back slashes and quotes should be escaped. Tabs, carriage returns, and line feeds should be converted to \t, \r, and \n respectively.

  ### Examples

  ```
  ["www.example.com", "opentelemetry.io"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DNSAttributes.dns_question_name()
      :"dns.question.name"

  ### Erlang

  ```erlang
  ?DNS_QUESTION_NAME.
  'dns.question.name'
  ```

  <!-- tabs-close -->
  """
  @spec dns_question_name :: :"dns.question.name"
  def dns_question_name do
    :"dns.question.name"
  end
end
