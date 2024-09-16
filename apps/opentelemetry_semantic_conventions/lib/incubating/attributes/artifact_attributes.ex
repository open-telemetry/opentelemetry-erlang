defmodule OpenTelemetry.SemConv.Incubating.ArtifactAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Artifact attributes.
  """

  @doc """
  The provenance filename of the built attestation which directly relates to the build artifact filename. This filename **SHOULD** accompany the artifact at publish time. See the [SLSA Relationship](https://slsa.dev/spec/v1.0/distributing-provenance#relationship-between-artifacts-and-attestations) specification for more information.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["golang-binary-amd64-v0.1.0.attestation", "docker-image-amd64-v0.1.0.intoto.json1", "release-1.tar.gz.attestation", "file-name-package.tar.gz.intoto.json1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ArtifactAttributes.artifact_attestation_filename()
      :"artifact.attestation.filename"

  ### Erlang

  ```erlang
  ?ARTIFACT_ATTESTATION_FILENAME.
  'artifact.attestation.filename'
  ```

  <!-- tabs-close -->
  """
  @spec artifact_attestation_filename :: :"artifact.attestation.filename"
  def artifact_attestation_filename do
    :"artifact.attestation.filename"
  end

  @doc """
  The full [hash value (see glossary)](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.186-5.pdf), of the built attestation. Some envelopes in the software attestation space also refer to this as the [digest](https://github.com/in-toto/attestation/blob/main/spec/README.md#in-toto-attestation-framework-spec).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1b31dfcd5b7f9267bf2ff47651df1cfb9147b9e4df1f335accf65b4cda498408"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ArtifactAttributes.artifact_attestation_hash()
      :"artifact.attestation.hash"

  ### Erlang

  ```erlang
  ?ARTIFACT_ATTESTATION_HASH.
  'artifact.attestation.hash'
  ```

  <!-- tabs-close -->
  """
  @spec artifact_attestation_hash :: :"artifact.attestation.hash"
  def artifact_attestation_hash do
    :"artifact.attestation.hash"
  end

  @doc """
  The id of the build [software attestation](https://slsa.dev/attestation-model).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["123"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ArtifactAttributes.artifact_attestation_id()
      :"artifact.attestation.id"

  ### Erlang

  ```erlang
  ?ARTIFACT_ATTESTATION_ID.
  'artifact.attestation.id'
  ```

  <!-- tabs-close -->
  """
  @spec artifact_attestation_id :: :"artifact.attestation.id"
  def artifact_attestation_id do
    :"artifact.attestation.id"
  end

  @doc """
  The human readable file name of the artifact, typically generated during build and release processes. Often includes the package name and version in the file name.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This file name can also act as the [Package Name](https://slsa.dev/spec/v1.0/terminology#package-model)
  in cases where the package ecosystem maps accordingly.
  Additionally, the artifact [can be published](https://slsa.dev/spec/v1.0/terminology#software-supply-chain)
  for others, but that is not a guarantee.

  ### Examples

  ```
  ["golang-binary-amd64-v0.1.0", "docker-image-amd64-v0.1.0", "release-1.tar.gz", "file-name-package.tar.gz"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ArtifactAttributes.artifact_filename()
      :"artifact.filename"

  ### Erlang

  ```erlang
  ?ARTIFACT_FILENAME.
  'artifact.filename'
  ```

  <!-- tabs-close -->
  """
  @spec artifact_filename :: :"artifact.filename"
  def artifact_filename do
    :"artifact.filename"
  end

  @doc """
  The full [hash value (see glossary)](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.186-5.pdf), often found in checksum.txt on a release of the artifact and used to verify package integrity.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The specific algorithm used to create the cryptographic hash value is
  not defined. In situations where an artifact has multiple
  cryptographic hashes, it is up to the implementer to choose which
  hash value to set here; this should be the most secure hash algorithm
  that is suitable for the situation and consistent with the
  corresponding attestation. The implementer can then provide the other
  hash values through an additional set of attribute extensions as they
  deem necessary.

  ### Examples

  ```
  ["9ff4c52759e2c4ac70b7d517bc7fcdc1cda631ca0045271ddd1b192544f8a3e9"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ArtifactAttributes.artifact_hash()
      :"artifact.hash"

  ### Erlang

  ```erlang
  ?ARTIFACT_HASH.
  'artifact.hash'
  ```

  <!-- tabs-close -->
  """
  @spec artifact_hash :: :"artifact.hash"
  def artifact_hash do
    :"artifact.hash"
  end

  @doc """
  The [Package URL](https://github.com/package-url/purl-spec) of the [package artifact](https://slsa.dev/spec/v1.0/terminology#package-model) provides a standard way to identify and locate the packaged artifact.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["pkg:github/package-url/purl-spec@1209109710924", "pkg:npm/foo@12.12.3"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ArtifactAttributes.artifact_purl()
      :"artifact.purl"

  ### Erlang

  ```erlang
  ?ARTIFACT_PURL.
  'artifact.purl'
  ```

  <!-- tabs-close -->
  """
  @spec artifact_purl :: :"artifact.purl"
  def artifact_purl do
    :"artifact.purl"
  end

  @doc """
  The version of the artifact.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["v0.1.0", "1.2.1", "122691-build"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ArtifactAttributes.artifact_version()
      :"artifact.version"

  ### Erlang

  ```erlang
  ?ARTIFACT_VERSION.
  'artifact.version'
  ```

  <!-- tabs-close -->
  """
  @spec artifact_version :: :"artifact.version"
  def artifact_version do
    :"artifact.version"
  end
end
