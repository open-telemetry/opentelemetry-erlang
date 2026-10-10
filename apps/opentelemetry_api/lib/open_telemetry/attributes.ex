defmodule OpenTelemetry.Attributes do
  @moduledoc """
  This module contains utility functions for span attributes.

  Elixir has built in variables like `__ENV__` and `__CALLER__` that can be used to generate
  span attributes like `code.function`, `code.lineno`, and `code.namespace` either during runtime
  or compile time. This module provides a function to generate these attributes from a `t:Macro.Env`
  struct.

  For more information, view the [OpenTelemetry Semantic Conventions](OSC).

  [OSC]: https://opentelemetry.io/docs/specs/semconv/attributes-registry
  """

  @code_filepath :"code.filepath"
  @code_function :"code.function"
  @code_lineno :"code.lineno"
  @code_namespace :"code.namespace"

  @doc """
  A function used to generate attributes from a `t:Macro.Env` struct.

  This function is used to generate span attributes like `code.function`, `code.lineno`, and
  `code.namespace` from a `__CALLER__` variable during compile time or a `__ENV__` variable
  run time.

  ## Usage

      # During run time
      def my_function() do
        OpenTelemetry.Attributes.from_macro_env(__ENV__)
      end

      iex> my_function()
      %{code_function: "my_function/0", ...}

      # During compile time in a macro
      defmacro my_macro() do
        attributes =
          __CALLER__
          |> OpenTelemetry.Attributes.from_macro_env()
          |> Macro.escape()

        quote do
          unquote(attributes)
        end
      end

      def my_other_function() do
        my_macro()
      end

      iex> my_macro()
      %{code_function: "my_other_function/0", ...}

  """
  @spec from_macro_env(Macro.Env.t()) :: OpenTelemetry.attributes_map()
  def from_macro_env(%Macro.Env{} = env) do
    function_arty =
      case env.function do
        {func_name, func_arity} -> "#{func_name}/#{func_arity}"
        nil -> nil
      end

    %{
      @code_function => function_arty,
      @code_namespace => inspect(env.module),
      @code_filepath => env.file,
      @code_lineno => env.line
    }
    |> Enum.reject(fn {_k, v} -> is_nil(v) end)
    |> Map.new()
  end
end
