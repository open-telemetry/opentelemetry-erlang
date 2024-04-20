defmodule OpenTelemetry.SemanticConventions.MixProject do
  use Mix.Project

  def project do
    {app, desc} = load_app()

    [
      app: app,
      version: to_string(Keyword.fetch!(desc, :vsn)),
      description: to_string(Keyword.fetch!(desc, :description)),
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      deps: [
        {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
        {:covertool, ">= 0.0.0", only: :test},
        {:ex_doc, "~> 0.32", only: [:dev]}
      ],
      name: "OpenTelemetry.SemanticConventions",
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang/apps/opentelemetry_semantic_conventions",
      docs: [
        main: "OpenTelemetry.SemanticConventions.SemanticAttributes",
        extras: [],
        groups_for_docs: [
          AWS: &(&1[:namespace] == :aws),
          Browser: &(&1[:namespace] == :browser),
          Cloud: &(&1[:namespace] in [:cloud, :cloudevents]),
          Code: &(&1[:namespace] in [:code, :exception]),
          Container: &(&1[:namespace] == :container),
          Database: &(&1[:namespace] == :db),
          DNS: &(&1[:namespace] in [:client, :destination, :dns]),
          "End User": &(&1[:namespace] == :enduser),
          FAAS: &(&1[:namespace] == :faas),
          File: &(&1[:namespace] == :file),
          GCP: &(&1[:namespace] == :gcp),
          GraphQL: &(&1[:namespace] == :graphql),
          Host: &(&1[:namespace] == :host),
          HTTP: &(&1[:namespace] == :http),
          Kubernetes: &(&1[:namespace] == :k8s),
          Log: &(&1[:namespace] == :log),
          Messaging: &(&1[:namespace] == :messaging),
          Network: &(&1[:namespace] == :network),
          Process: &(&1[:namespace] == :process),
          RPC: &(&1[:namespace] == :rpc),
          Runtime: &(&1[:namespace] in [:thread]),
          Service: &(&1[:namespace] == :service),
          Session: &(&1[:namespace] == :session),
          System: &(&1[:namespace] in [:device, :system]),
          Telemetry: &(&1[:namespace] == :telemetry),
          TLS: &(&1[:namespace] == :tls),
          URL: &(&1[:namespace] == :url)
        ],
        nest_modules_by_prefix: [
          OpenTelemetry,
          SemanticConventions.SemanticAttributes
        ]
      ],
      test_coverage: [tool: :covertool],
      package: package()
    ]
  end

  def application, do: []

  defp package() do
    [
      description: "OpenTelemetry Semantic Conventions",
      build_tools: ["rebar3", "mix"],
      files: ~w(lib mix.exs README.md LICENSE rebar.config include src),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  defp load_app do
    {:ok, [{:application, name, desc}]} =
      :file.consult(~c"src/opentelemetry_semantic_conventions.app.src")

    {name, desc}
  end
end
