defmodule OpenTelemetry.MixProject do
  use Mix.Project

  def project do
    [
      app: :open_telemetry,
      version: version(),
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      # We should never have dependencies
      deps: deps(),
      # Docs
      name: "OpenTelemetry API",
      # source_url: "https://github.com/USER/PROJECT",
      # homepage_url: "http://YOUR_PROJECT_HOMEPAGE",
      docs: [
        markdown_processor: ExDoc.Markdown.Cmark,
        main: "OpenTelemetry",
        # logo: "path/to/logo.png",
        extras: erlang_docs()
      ],
      aliases: [
        # when build docs first build edocs with rebar3
        docs: ["cmd rebar3 edoc", "docs"]
      ],
      package: package()
    ]
  end

  defp version do
    "VERSION"
    |> File.read!()
    |> String.trim()
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    []
  end

  def deps() do
    [
      {:cmark, "~> 0.7", only: :dev, runtime: false},
      {:ex_doc, "~> 0.21", only: :dev, runtime: false}
    ]
  end

  defp package() do
    [
      description: "OpenTelemetry API",
      build_tools: ["rebar3", "mix"],
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/open-telemetry/opentelemetry-erlang-api",
               "OpenTelemetry.io" => "https://opentelemetry.io"}
    ]
  end

  def erlang_docs() do
    files =
      for file <- Path.wildcard("edoc/*.md"),
        file != "edoc/README.md",
        do: {file, [title: Path.basename(file, ".md")]}

    [{"README.md", [title: "Overview"]} | files]
  end
end
