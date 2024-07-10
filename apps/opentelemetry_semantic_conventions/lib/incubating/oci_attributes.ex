defmodule OpenTelemetry.SemConv.Incubating.OCIAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for OCI attributes.
  """

  @doc """
  The digest of the OCI image manifest. For container images specifically is the digest by which the container image is known.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Follows [OCI Image Manifest Specification](https://github.com/opencontainers/image-spec/blob/main/manifest.md), and specifically the [Digest property](https://github.com/opencontainers/image-spec/blob/main/descriptor.md#digests).
  An example can be found in [Example Image Manifest](https://docs.docker.com/registry/spec/manifest-v2-2/#example-image-manifest).

  ### Examples

  ```
  ["sha256:e4ca62c0d62f3e886e684806dfe9d4e0cda60d54986898173c1083856cfda0f4"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OCIAttributes.oci_manifest_digest()
      :"oci.manifest.digest"

  ### Erlang

  ```erlang
  ?OCI_MANIFEST_DIGEST.
  'oci.manifest.digest'
  ```

  <!-- tabs-close -->
  """
  @spec oci_manifest_digest :: :"oci.manifest.digest"
  def oci_manifest_digest do
    :"oci.manifest.digest"
  end
end
