defmodule OpenTelemetry.SemConvTest do
  use ExUnit.Case, async: false

  alias OpenTelemetry.SemConv

  describe "stability_opt_in" do
    test "http" do
      assert SemConv.stability_opt_in() == %{http: :default}

      System.put_env("OTEL_SEMCONV_STABILITY_OPT_IN", "")
      assert SemConv.stability_opt_in() == %{http: :default}

      System.put_env("OTEL_SEMCONV_STABILITY_OPT_IN", "unsupported")
      assert SemConv.stability_opt_in() == %{http: :default}

      System.put_env("OTEL_SEMCONV_STABILITY_OPT_IN", "http")
      assert SemConv.stability_opt_in() == %{http: :http}

      System.put_env("OTEL_SEMCONV_STABILITY_OPT_IN", "http/dup,http")
      assert SemConv.stability_opt_in() == %{http: :http_dup}

      # dup takes precedence
      System.put_env("OTEL_SEMCONV_STABILITY_OPT_IN", "http,http/dup")
      assert SemConv.stability_opt_in() == %{http: :http_dup}

      System.delete_env("OTEL_SEMCONV_STABILITY_OPT_IN")
    end
  end

  test "stable attrs are delegated in incubating module" do
    assert SemConv.Incubating.HTTPAttributes.http_route() ==
             SemConv.HTTPAttributes.http_route()
  end
end
