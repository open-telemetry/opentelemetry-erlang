contents =
  quote do
    use ExUnit.Case, async: true

    {:ok, modules} = :application.get_key(:opentelemetry_semantic_conventions, :modules)

    modules_to_test =
      Enum.filter(modules, fn module ->
        module = to_string(module)
        String.starts_with?(module, "Elixir") && String.contains?(module, "SemConv")
      end)

    for module <- modules_to_test do
      doctest(module)
    end
  end

Module.create(
  OpenTelemetry.SemanticConventions.RegistryTest,
  contents,
  Macro.Env.location(__ENV__)
)
