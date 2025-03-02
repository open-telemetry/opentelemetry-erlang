defmodule OpenTelemetry.MixProject do
  use Mix.Project

  def project do
    {app, desc} = load_app()

    [
      app: app,
      version: to_string(Keyword.fetch!(desc, :vsn)),
      description: to_string(Keyword.fetch!(desc, :description)),
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      deps: [
        {:eqwalizer_support,
         git: "https://github.com/whatsapp/eqwalizer.git",
         branch: "main",
         sparse: "eqwalizer_support",
         only: [:dev],
         runtime: false},
        {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
        {:covertool, ">= 0.0.0", only: :test},
        {:ex_doc, "~> 0.37", only: :dev, runtime: false}
      ],
      name: "OpenTelemetry API",
      test_coverage: [tool: :covertool],
      package: package(),
      dialyzer: [
        ignore_warnings: "dialyzer.ignore-warnings",
        remove_defaults: [:unknown],
        plt_add_apps: [:eqwalizer_support],
        list_unused_filters: true
      ],
      source_url: "https://github.com/open-telemetry/opentelemetry-erlang",
      homepage_url: "https://github.com/open-telemetry/opentelemetry-erlang",
      docs: [
        main: "readme",
        source_url_pattern:
          "https://github.com/open-telemetry/opentelemetry-erlang/blob/main/apps/opentelemetry_api/%{path}#L%{line}",
        extras: ["README.md", "LICENSE"],
        groups_for_modules: [
          Context: [:otel_ctx, OpenTelemetry.Ctx],
          Baggage: [:otel_baggage, OpenTelemetry.Baggage],
          Tracer: [
            :otel_tracer,
            :otel_tracer_noop,
            :otel_tracer_provider,
            :otel_tracestate,
            OpenTelemetry.Tracer,
            OpenTelemetry.Span
          ],
          Propagators: [
            :otel_propagator,
            :otel_propagator_baggage,
            :otel_propagator_b3,
            :otel_propagator_b3multi,
            :otel_propagator_b3single,
            :otel_propagator_text_map,
            :otel_propagator_text_map_composite,
            :otel_propagator_text_map_noop,
            :otel_propagator_trace_context
          ]
        ]
      ]
    ]
  end

  def application, do: []

  defp package() do
    [
      description: "OpenTelemetry API",
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
    {:ok, [{:application, name, desc}]} = :file.consult(~c"src/opentelemetry_api.app.src")

    {name, desc}
  end
end
