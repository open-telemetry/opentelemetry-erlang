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
        {:ex_doc, "~> 0.34", only: [:dev]}
      ],
      name: "OpenTelemetry.SemConv",
      source_url:
        "https://github.com/open-telemetry/opentelemetry-erlang/apps/opentelemetry_semantic_conventions",
      docs: [
        markdown_processor: {ExDoc.Markdown.Earmark, [all: true]},
        main: "OpenTelemetry.SemConv",
        extra_section: "GUIDES",
        extras: guides() ++ [],
        groups_for_extras: [
          "Attributes Registry": Path.wildcard("guides/attributes-registry/*.md"),
          "Cloud Providers": Path.wildcard("guides/cloud-providers/*.md"),
          "Cloud Events": Path.wildcard("guides/cloudevents/*.md"),
          Database: Path.wildcard("guides/database/*.md"),
          DNS: Path.wildcard("guides/dns/*.md"),
          Exceptions: Path.wildcard("guides/exceptions/*.md"),
          FAAS: Path.wildcard("guides/faas/*.md"),
          "Feature Flags": Path.wildcard("guides/feature-flags/*.md"),
          General: Path.wildcard("guides/general/*.md"),
          "Generative AI": Path.wildcard("guides/gen-ai/*.md"),
          GraphQL: Path.wildcard("guides/graphql/*.md"),
          HTTP: Path.wildcard("guides/http/*.md"),
          Messaging: Path.wildcard("guides/messaging/*.md"),
          "Object Stores": Path.wildcard("guides/object-stores/*.md"),
          Resource: Path.wildcard("guides/resource/**/*.md"),
          RPC: Path.wildcard("guides/rpc/*.md"),
          Runtime: Path.wildcard("guides/runtime/*.md"),
          System: Path.wildcard("guides/system/*.md"),
          URL: Path.wildcard("guides/url/*.md")
        ],
        groups_for_modules: [
          Attributes:
            ~r/^OpenTelemetry\.SemConv(?!\.Metrics|\.Incubating)(\.[A-Z][A-Za-z0-9]*)+$/,
          "Incubating Attributes":
            ~r/^OpenTelemetry\.SemConv\.Incubating(?!\.Metrics)(\.[A-Z][A-Za-z0-9]*)+$/,
          Metrics: ~r/OpenTelemetry.SemConv.Metrics/,
          "Incubating Metrics": ~r/OpenTelemetry.SemConv.Incubating.Metrics/,
          Deprecated: ~r/OpenTelemetry.SemanticConventions(\.[A-Z][A-Za-z0-9]*)+$/
        ],
        nest_modules_by_prefix: [
          OpenTelemetry.SemConv,
          OpenTelemetry.SemConv.Metrics,
          OpenTelemetry.SemConv.Incubating,
          OpenTelemetry.SemConv.Incubating.Metrics,
          OpenTelemetry.SemanticConventions
        ]
      ],
      test_coverage: [tool: :covertool],
      package: package()
    ]
  end

  def application, do: []

  defp package() do
    [
      name: "opentelemetry_semantic_conventions",
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

  defp guides do
    Path.wildcard("guides/**/*.md")
    |> Enum.reject(fn path ->
      path in ([
                 "guides/attributes-registry/android.md",
                 "guides/attributes-registry/aspnetcore.md",
                 "guides/attributes-registry/dotnet.md",
                 "guides/attributes-registry/ios.md",
                 "guides/attributes-registry/jvm.md",
                 "guides/attributes-registry/signalr.md"
               ] ++ Path.wildcard("guides/dotnet/*") ++ Path.wildcard("guides/mobile/*"))
    end)
  end
end
