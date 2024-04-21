defmodule OpenTelemetry.SemanticConventions.Resource do
  @moduledoc """
  OpenTelemetry Semantic Conventions for Attributes.
  """

  @doc namespace: :aws
  @typedoc """
  The [launch type](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html) for an ECS task

  ### Options


  * `:ec2`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ec2

  * `:fargate`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - fargate



  """
  @type aws_ecs_launchtype() :: :ec2 | :fargate

  @doc namespace: :host
  @typedoc """
  The CPU architecture the host system is running on

  ### Options


  * `:amd64`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - AMD64

  * `:arm32`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ARM32

  * `:arm64`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ARM64

  * `:ia64`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Itanium

  * `:ppc32`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 32-bit PowerPC

  * `:ppc64`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 64-bit PowerPC

  * `:s390x`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - IBM z/Architecture

  * `:x86`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 32-bit x86



  """
  @type host_arch() :: :amd64 | :arm32 | :arm64 | :ia64 | :ppc32 | :ppc64 | :s390x | :x86 | atom()

  @doc namespace: :telemetry
  @typedoc """
  The language of the telemetry SDK

  ### Options


  * `:cpp` - cpp

  * `:dotnet` - dotnet

  * `:erlang` - erlang

  * `:go` - go

  * `:java` - java

  * `:nodejs` - nodejs

  * `:php` - php

  * `:python` - python

  * `:ruby` - ruby

  * `:rust` - rust

  * `:swift` - swift

  * `:webjs` - webjs



  """
  @type telemetry_sdk_language() ::
          :cpp
          | :dotnet
          | :erlang
          | :go
          | :java
          | :nodejs
          | :php
          | :python
          | :ruby
          | :rust
          | :swift
          | :webjs
          | atom()

  @doc """
  The URL of the OpenTelemetry schema for these keys and values.

      iex> OpenTelemetry.SemanticConventions.Resource.schema_url()
      "https://opentelemetry.io/schemas/1.25.0"
  """
  @spec schema_url :: String.t()
  def schema_url do
    "https://opentelemetry.io/schemas/1.25.0"
  end

  @doc namespace: :aws

  @doc """


  The [launch type](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html) for an ECS task

      iex> OpenTelemetry.SemanticConventions.Resource.aws_ecs_launchtype()
      :"aws.ecs.launchtype"
  """

  @spec aws_ecs_launchtype :: :"aws.ecs.launchtype"
  def aws_ecs_launchtype do
    :"aws.ecs.launchtype"
  end

  @doc namespace: :host

  @doc """


  The CPU architecture the host system is running on

      iex> OpenTelemetry.SemanticConventions.Resource.host_arch()
      :"host.arch"
  """

  @spec host_arch :: :"host.arch"
  def host_arch do
    :"host.arch"
  end

  @doc namespace: :telemetry

  @doc """


  The language of the telemetry SDK

      iex> OpenTelemetry.SemanticConventions.Resource.telemetry_sdk_language()
      :"telemetry.sdk.language"
  """

  @spec telemetry_sdk_language :: :"telemetry.sdk.language"
  def telemetry_sdk_language do
    :"telemetry.sdk.language"
  end
end
