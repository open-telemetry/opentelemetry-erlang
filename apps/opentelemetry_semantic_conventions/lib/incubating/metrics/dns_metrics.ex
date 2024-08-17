defmodule OpenTelemetry.SemConv.Incubating.Metrics.DNSMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for DNS metrics.
  """
  @doc """
  Measures the time taken to perform a DNS lookup.

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.DNSMetrics.dns_lookup_duration()
      :"dns.lookup.duration"

  ### Erlang

  ```erlang
  ?DNS_LOOKUP_DURATION.
  'dns.lookup.duration'
  ```

  <!-- tabs-close -->
  """

  @spec dns_lookup_duration :: :"dns.lookup.duration"
  def dns_lookup_duration do
    :"dns.lookup.duration"
  end
end
