defmodule OpenTelemetry.SemConv.Incubating.ContainerAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Container attributes.
  """

  @doc """
  The command used to run the container (i.e. the command name).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  If using embedded credentials or sensitive data, it is recommended to remove them to prevent potential leakage.

  ### Examples

  ```
  ["otelcontribcol"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ContainerAttributes.container_command()
      :"container.command"

  ### Erlang

  ```erlang
  ?CONTAINER_COMMAND.
  'container.command'
  ```

  <!-- tabs-close -->
  """
  @spec container_command :: :"container.command"
  def container_command do
    :"container.command"
  end

  @doc """
  All the command arguments (including the command/executable itself) run by the container. [2]

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["otelcontribcol, --config, config.yaml"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ContainerAttributes.container_command_args()
      :"container.command_args"

  ### Erlang

  ```erlang
  ?CONTAINER_COMMAND_ARGS.
  'container.command_args'
  ```

  <!-- tabs-close -->
  """
  @spec container_command_args :: :"container.command_args"
  def container_command_args do
    :"container.command_args"
  end

  @doc """
  The full command run by the container as a single string representing the full command. [2]

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["otelcontribcol --config config.yaml"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ContainerAttributes.container_command_line()
      :"container.command_line"

  ### Erlang

  ```erlang
  ?CONTAINER_COMMAND_LINE.
  'container.command_line'
  ```

  <!-- tabs-close -->
  """
  @spec container_command_line :: :"container.command_line"
  def container_command_line do
    :"container.command_line"
  end

  @typedoc """
  Deprecated, use `cpu.mode` instead.

  ### Enum Values
  * `:user` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - When tasks of the cgroup are in user mode (Linux). When all container processes are in user mode (Windows).
  * `:system` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - When CPU is used by the system (host OS)
  * `:kernel` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - When tasks of the cgroup are in kernel mode (Linux). When all container processes are in kernel mode (Windows).
  """
  @type container_cpu_state_values() :: %{
          :user => :user,
          :system => :system,
          :kernel => :kernel
        }
  @deprecated """
  Replaced by `cpu.mode`
  """
  @spec container_cpu_state :: :"container.cpu.state"
  def container_cpu_state do
    :"container.cpu.state"
  end

  @spec container_cpu_state_values() :: container_cpu_state_values()
  def container_cpu_state_values() do
    %{
      :user => :user,
      :system => :system,
      :kernel => :kernel
    }
  end

  @doc """
  Container ID. Usually a UUID, as for example used to [identify Docker containers](https://docs.docker.com/engine/reference/run/#container-identification). The UUID might be abbreviated.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["a3bf90e006b2"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ContainerAttributes.container_id()
      :"container.id"

  ### Erlang

  ```erlang
  ?CONTAINER_ID.
  'container.id'
  ```

  <!-- tabs-close -->
  """
  @spec container_id :: :"container.id"
  def container_id do
    :"container.id"
  end

  @doc """
  Runtime specific image identifier. Usually a hash algorithm followed by a UUID.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Docker defines a sha256 of the image id; `container.image.id` corresponds to the `Image` field from the Docker container inspect [API](https://docs.docker.com/engine/api/v1.43/#tag/Container/operation/ContainerInspect) endpoint.
  K8s defines a link to the container registry repository with digest `"imageID": "registry.azurecr.io /namespace/service/dockerfile@sha256:bdeabd40c3a8a492eaf9e8e44d0ebbb84bac7ee25ac0cf8a7159d25f62555625"`.
  The ID is assigned by the container runtime and can vary in different environments. Consider using `oci.manifest.digest` if it is important to identify the same image in different environments/runtimes.

  ### Examples

  ```
  ["sha256:19c92d0a00d1b66d897bceaa7319bee0dd38a10a851c60bcec9474aa3f01e50f"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ContainerAttributes.container_image_id()
      :"container.image.id"

  ### Erlang

  ```erlang
  ?CONTAINER_IMAGE_ID.
  'container.image.id'
  ```

  <!-- tabs-close -->
  """
  @spec container_image_id :: :"container.image.id"
  def container_image_id do
    :"container.image.id"
  end

  @doc """
  Name of the image the container was built on.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["gcr.io/opentelemetry/operator"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ContainerAttributes.container_image_name()
      :"container.image.name"

  ### Erlang

  ```erlang
  ?CONTAINER_IMAGE_NAME.
  'container.image.name'
  ```

  <!-- tabs-close -->
  """
  @spec container_image_name :: :"container.image.name"
  def container_image_name do
    :"container.image.name"
  end

  @doc """
  Repo digests of the container image as provided by the container runtime.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  [Docker](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect) and [CRI](https://github.com/kubernetes/cri-api/blob/c75ef5b473bbe2d0a4fc92f82235efd665ea8e9f/pkg/apis/runtime/v1/api.proto#L1237-L1238) report those under the `RepoDigests` field.

  ### Examples

  ```
  ["example@sha256:afcc7f1ac1b49db317a7196c902e61c6c3c4607d63599ee1a82d702d249a0ccb", "internal.registry.example.com:5000/example@sha256:b69959407d21e8a062e0416bf13405bb2b71ed7a84dde4158ebafacfa06f5578"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ContainerAttributes.container_image_repo_digests()
      :"container.image.repo_digests"

  ### Erlang

  ```erlang
  ?CONTAINER_IMAGE_REPO_DIGESTS.
  'container.image.repo_digests'
  ```

  <!-- tabs-close -->
  """
  @spec container_image_repo_digests :: :"container.image.repo_digests"
  def container_image_repo_digests do
    :"container.image.repo_digests"
  end

  @doc """
  Container image tags. An example can be found in [Docker Image Inspect](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect). Should be only the `<tag>` section of the full name for example from `registry.example.com/my-org/my-image:<tag>`.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["v1.27.1", "3.5.7-0"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ContainerAttributes.container_image_tags()
      :"container.image.tags"

  ### Erlang

  ```erlang
  ?CONTAINER_IMAGE_TAGS.
  'container.image.tags'
  ```

  <!-- tabs-close -->
  """
  @spec container_image_tags :: :"container.image.tags"
  def container_image_tags do
    :"container.image.tags"
  end

  @doc """
  Container labels, `<key>` being the label name, the value being the label value.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["container.label.app=nginx"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ContainerAttributes.container_label()
      :"container.label"

  ### Erlang

  ```erlang
  ?CONTAINER_LABEL.
  'container.label'
  ```

  <!-- tabs-close -->
  """
  @spec container_label :: :"container.label"
  def container_label do
    :"container.label"
  end

  @deprecated """
  Replaced by `container.label`.
  """
  @spec container_labels :: :"container.labels"
  def container_labels do
    :"container.labels"
  end

  @doc """
  Container name used by container runtime.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry-autoconf"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ContainerAttributes.container_name()
      :"container.name"

  ### Erlang

  ```erlang
  ?CONTAINER_NAME.
  'container.name'
  ```

  <!-- tabs-close -->
  """
  @spec container_name :: :"container.name"
  def container_name do
    :"container.name"
  end

  @doc """
  The container runtime managing this container.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["docker", "containerd", "rkt"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ContainerAttributes.container_runtime()
      :"container.runtime"

  ### Erlang

  ```erlang
  ?CONTAINER_RUNTIME.
  'container.runtime'
  ```

  <!-- tabs-close -->
  """
  @spec container_runtime :: :"container.runtime"
  def container_runtime do
    :"container.runtime"
  end
end
