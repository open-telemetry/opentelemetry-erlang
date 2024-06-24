defmodule OpenTelemetry.SemanticConventions.OciAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Oci attributes.
  """

  @doc """
  The digest of the OCI image manifest. For container images specifically is the digest by which the container image is known.

  ### Notes

  Follows [OCI Image Manifest Specification](https://github.com/opencontainers/image-spec/blob/main/manifest.md), and specifically the [Digest property](https://github.com/opencontainers/image-spec/blob/main/descriptor.md#digests).
  An example can be found in [Example Image Manifest](https://docs.docker.com/registry/spec/manifest-v2-2/#example-image-manifest).


  ### Example
      iex> OpenTelemetry.SemanticConventions.OciAttributes.oci_manifest_digest()
      :"oci.manifest.digest"
  """
  @spec oci_manifest_digest :: :"oci.manifest.digest"
  def oci_manifest_digest do
    :"oci.manifest.digest"
  end
end
