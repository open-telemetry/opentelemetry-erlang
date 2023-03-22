defmodule OtelElixirTests.MixProject do
  use Mix.Project

  def project do
    [
      app: :otel_elixir_tests,
      version: "0.1.0",
      deps: deps()
    ]
  end

  def deps do
    [
      {:opentelemetry, path: "apps/opentelemetry", only: :test, override: true},
      {:opentelemetry_api, path: "apps/opentelemetry_api", only: :test, override: true},
      {:opentelemetry_semantic_conventions, path: "apps/opentelemetry_semantic_conventions", only: :test, override: true},
    ]
  end
end
