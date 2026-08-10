defmodule OpenTelemetry.SemConv.Incubating.DNSAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for DNS attributes.
  """

  @doc """
  The list of IPv4 or IPv6 addresses resolved during DNS lookup.
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  [["10.0.0.1", "2001:0db8:85a3:0000:0000:8a2e:0370:7334"]]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DNSAttributes.dns_answers()
      :"dns.answers"

  ### Erlang

  ```erlang
  ?DNS_ANSWERS.
  'dns.answers'
  ```

  <!-- tabs-close -->
  """
  @spec dns_answers :: :"dns.answers"
  def dns_answers do
    :"dns.answers"
  end

  @doc """
  The name being queried.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The name represents the queried domain name as it appears in the DNS query without any additional normalization.

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
