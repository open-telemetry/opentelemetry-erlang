defmodule OpenTelemetryExperimental.MixProject do
  use Mix.Project

  def project do
    {app, desc} = load_app()
    config = load_config()

    [
      app: app,
      version: version(Keyword.fetch!(desc, :vsn)),
      description: to_string(Keyword.fetch!(desc, :description)),
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      # We should never have dependencies
      deps: deps(Keyword.fetch!(config, :deps)),
      # Docs
      name: "OpenTelemetry Experimental API",
      # source_url: "https://github.com/USER/PROJECT",
      # homepage_url: "http://YOUR_PROJECT_HOMEPAGE",
      test_coverage: [tool: :covertool],
      aliases: [docs: & &1],
      package: package()
    ]
  end

  defp version(version) when is_list(version) do
    List.to_string(version)
  end

  defp version({:file, path}) do
    path
    |> File.read!()
    |> String.trim()
  end

  # Run "mix help compile.app" to learn about applications.
  def application, do: []

  defp deps(rebar) do
    rebar
    |> Enum.map(fn
      {dep, version} -> {dep, to_string(version)}
      dep when is_atom(dep) -> {dep, ">= 0.0.0"}
    end)
    |> Enum.concat([
      {:cmark, "~> 0.10", only: :dev, runtime: false},
      {:ex_doc, "~> 0.37", only: :dev, runtime: false},
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:covertool, ">= 0.0.0", only: :test}
    ])
  end

  defp package() do
    [
      description: "OpenTelemetry Experimental API",
      build_tools: ["rebar3", "mix"],
      files: ~w(lib mix.exs README.md LICENSE rebar.config include src),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  defp load_config do
    {:ok, config} = :file.consult(~c"rebar.config")

    config
  end

  defp load_app do
    {:ok, [{:application, name, desc}]} =
      :file.consult(~c"src/opentelemetry_api_experimental.app.src")

    {name, desc}
  end
end
