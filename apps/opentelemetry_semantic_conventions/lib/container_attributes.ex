defmodule OpenTelemetry.SemanticConventions.ContainerAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Container attributes.
  """

  @doc """
  The command used to run the container (i.e. the command name).

  ### Notes

  If using embedded credentials or sensitive data, it is recommended to remove them to prevent potential leakage.


  ### Example
      iex> OpenTelemetry.SemanticConventions.ContainerAttributes.container_command()
      :"container.command"
  """
  @spec container_command :: :"container.command"
  def container_command do
    :"container.command"
  end

  @doc """
  All the command arguments (including the command/executable itself) run by the container. [2]



  ### Example
      iex> OpenTelemetry.SemanticConventions.ContainerAttributes.container_commandargs()
      :"container.command_args"
  """
  @spec container_commandargs :: :"container.command_args"
  def container_commandargs do
    :"container.command_args"
  end

  @doc """
  The full command run by the container as a single string representing the full command. [2]



  ### Example
      iex> OpenTelemetry.SemanticConventions.ContainerAttributes.container_commandline()
      :"container.command_line"
  """
  @spec container_commandline :: :"container.command_line"
  def container_commandline do
    :"container.command_line"
  end

  @typedoc """
  The CPU state for this data point.

  ### Enum Values
  * `:user` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When tasks of the cgroup are in user mode (Linux). When all container processes are in user mode (Windows).
  * `:system` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When CPU is used by the system (host OS)
  * `:kernel` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When tasks of the cgroup are in kernel mode (Linux). When all container processes are in kernel mode (Windows).
  """
  @type container_cpu_state() :: %{
          :user => :user,
          :system => :system,
          :kernel => :kernel
        }
  @doc """
  The CPU state for this data point.


  ### Example
      iex> OpenTelemetry.SemanticConventions.ContainerAttributes.container_cpu_state().user
      :user
      
      iex> OpenTelemetry.SemanticConventions.ContainerAttributes.container_cpu_state(:custom_value)
      :custom_value
  """
  @spec container_cpu_state() :: container_cpu_state()
  def container_cpu_state() do
    %{
      :user => :user,
      :system => :system,
      :kernel => :kernel
    }
  end

  @spec container_cpu_state(atom() | String.t()) :: atom() | String.t()
  def container_cpu_state(custom_value) do
    custom_value
  end

  @doc """
  Container ID. Usually a UUID, as for example used to [identify Docker containers](https://docs.docker.com/engine/reference/run/#container-identification). The UUID might be abbreviated.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ContainerAttributes.container_id()
      :"container.id"
  """
  @spec container_id :: :"container.id"
  def container_id do
    :"container.id"
  end

  @doc """
  Runtime specific image identifier. Usually a hash algorithm followed by a UUID.

  ### Notes

  Docker defines a sha256 of the image id; `container.image.id` corresponds to the `Image` field from the Docker container inspect [API](https://docs.docker.com/engine/api/v1.43/#tag/Container/operation/ContainerInspect) endpoint.
  K8s defines a link to the container registry repository with digest `"imageID": "registry.azurecr.io /namespace/service/dockerfile@sha256:bdeabd40c3a8a492eaf9e8e44d0ebbb84bac7ee25ac0cf8a7159d25f62555625"`.
  The ID is assigned by the container runtime and can vary in different environments. Consider using `oci.manifest.digest` if it is important to identify the same image in different environments/runtimes.


  ### Example
      iex> OpenTelemetry.SemanticConventions.ContainerAttributes.container_image_id()
      :"container.image.id"
  """
  @spec container_image_id :: :"container.image.id"
  def container_image_id do
    :"container.image.id"
  end

  @doc """
  Name of the image the container was built on.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ContainerAttributes.container_image_name()
      :"container.image.name"
  """
  @spec container_image_name :: :"container.image.name"
  def container_image_name do
    :"container.image.name"
  end

  @doc """
  Repo digests of the container image as provided by the container runtime.

  ### Notes

  [Docker](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect) and [CRI](https://github.com/kubernetes/cri-api/blob/c75ef5b473bbe2d0a4fc92f82235efd665ea8e9f/pkg/apis/runtime/v1/api.proto#L1237-L1238) report those under the `RepoDigests` field.


  ### Example
      iex> OpenTelemetry.SemanticConventions.ContainerAttributes.container_image_repodigests()
      :"container.image.repo_digests"
  """
  @spec container_image_repodigests :: :"container.image.repo_digests"
  def container_image_repodigests do
    :"container.image.repo_digests"
  end

  @doc """
  Container image tags. An example can be found in [Docker Image Inspect](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect). Should be only the `<tag>` section of the full name for example from `registry.example.com/my-org/my-image:<tag>`.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ContainerAttributes.container_image_tags()
      :"container.image.tags"
  """
  @spec container_image_tags :: :"container.image.tags"
  def container_image_tags do
    :"container.image.tags"
  end

  @doc """
  Container labels, `<key>` being the label name, the value being the label value.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ContainerAttributes.container_label()
      :"container.label"
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



  ### Example
      iex> OpenTelemetry.SemanticConventions.ContainerAttributes.container_name()
      :"container.name"
  """
  @spec container_name :: :"container.name"
  def container_name do
    :"container.name"
  end

  @doc """
  The container runtime managing this container.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ContainerAttributes.container_runtime()
      :"container.runtime"
  """
  @spec container_runtime :: :"container.runtime"
  def container_runtime do
    :"container.runtime"
  end
end
