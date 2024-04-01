defmodule OpenTelemetry.Ctx do
  @moduledoc """
  The Context is responsible for propagating values within a process that
  are associated with a particular Trace or set of Baggage.

  `OpenTelemetry.Tracer' and `OpenTelemetry.Baggage' handle updating
  the Context.

  The Elixir version of this module only delegates to the `:otel_ctx` module.
  """

  @typedoc """
  Same as `t::otel_ctx.t/0`.
  """
  @type t() :: :otel_ctx.t()

  defdelegate new(), to: :otel_ctx
  defdelegate attach(ctx), to: :otel_ctx
  defdelegate detach(token), to: :otel_ctx
  defdelegate set_value(key, value), to: :otel_ctx
  defdelegate set_value(ctx, key, value), to: :otel_ctx
  defdelegate get_value(key, default), to: :otel_ctx
  defdelegate get_value(ctx, key, default), to: :otel_ctx
  defdelegate clear(), to: :otel_ctx
  defdelegate remove(key), to: :otel_ctx
  defdelegate get_current(), to: :otel_ctx
end
