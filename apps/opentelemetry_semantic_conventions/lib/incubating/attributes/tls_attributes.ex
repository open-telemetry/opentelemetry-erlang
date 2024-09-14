defmodule OpenTelemetry.SemConv.Incubating.TLSAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for TLS attributes.
  """

  @doc """
  String indicating the [cipher](https://datatracker.ietf.org/doc/html/rfc5246#appendix-A.5) used during the current connection.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The values allowed for `tls.cipher` **MUST** be one of the `Descriptions` of the [registered TLS Cipher Suits](https://www.iana.org/assignments/tls-parameters/tls-parameters.xhtml#table-tls-parameters-4).

  ### Examples

  ```
  ["TLS_RSA_WITH_3DES_EDE_CBC_SHA", "TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_cipher()
      :"tls.cipher"

  ### Erlang

  ```erlang
  ?TLS_CIPHER.
  'tls.cipher'
  ```

  <!-- tabs-close -->
  """
  @spec tls_cipher :: :"tls.cipher"
  def tls_cipher do
    :"tls.cipher"
  end

  @doc """
  PEM-encoded stand-alone certificate offered by the client. This is usually mutually-exclusive of `client.certificate_chain` since this value also exists in that list.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["MII..."]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_client_certificate()
      :"tls.client.certificate"

  ### Erlang

  ```erlang
  ?TLS_CLIENT_CERTIFICATE.
  'tls.client.certificate'
  ```

  <!-- tabs-close -->
  """
  @spec tls_client_certificate :: :"tls.client.certificate"
  def tls_client_certificate do
    :"tls.client.certificate"
  end

  @doc """
  Array of PEM-encoded certificates that make up the certificate chain offered by the client. This is usually mutually-exclusive of `client.certificate` since that value should be the first certificate in the chain.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["MII...", "MI..."]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_client_certificate_chain()
      :"tls.client.certificate_chain"

  ### Erlang

  ```erlang
  ?TLS_CLIENT_CERTIFICATE_CHAIN.
  'tls.client.certificate_chain'
  ```

  <!-- tabs-close -->
  """
  @spec tls_client_certificate_chain :: :"tls.client.certificate_chain"
  def tls_client_certificate_chain do
    :"tls.client.certificate_chain"
  end

  @doc """
  Certificate fingerprint using the MD5 digest of DER-encoded version of certificate offered by the client. For consistency with other hash values, this value should be formatted as an uppercase hash.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["0F76C7F2C55BFD7D8E8B8F4BFBF0C9EC"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_client_hash_md5()
      :"tls.client.hash.md5"

  ### Erlang

  ```erlang
  ?TLS_CLIENT_HASH_MD5.
  'tls.client.hash.md5'
  ```

  <!-- tabs-close -->
  """
  @spec tls_client_hash_md5 :: :"tls.client.hash.md5"
  def tls_client_hash_md5 do
    :"tls.client.hash.md5"
  end

  @doc """
  Certificate fingerprint using the SHA1 digest of DER-encoded version of certificate offered by the client. For consistency with other hash values, this value should be formatted as an uppercase hash.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["9E393D93138888D288266C2D915214D1D1CCEB2A"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_client_hash_sha1()
      :"tls.client.hash.sha1"

  ### Erlang

  ```erlang
  ?TLS_CLIENT_HASH_SHA1.
  'tls.client.hash.sha1'
  ```

  <!-- tabs-close -->
  """
  @spec tls_client_hash_sha1 :: :"tls.client.hash.sha1"
  def tls_client_hash_sha1 do
    :"tls.client.hash.sha1"
  end

  @doc """
  Certificate fingerprint using the SHA256 digest of DER-encoded version of certificate offered by the client. For consistency with other hash values, this value should be formatted as an uppercase hash.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["0687F666A054EF17A08E2F2162EAB4CBC0D265E1D7875BE74BF3C712CA92DAF0"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_client_hash_sha256()
      :"tls.client.hash.sha256"

  ### Erlang

  ```erlang
  ?TLS_CLIENT_HASH_SHA256.
  'tls.client.hash.sha256'
  ```

  <!-- tabs-close -->
  """
  @spec tls_client_hash_sha256 :: :"tls.client.hash.sha256"
  def tls_client_hash_sha256 do
    :"tls.client.hash.sha256"
  end

  @doc """
  Distinguished name of [subject](https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.6) of the issuer of the x.509 certificate presented by the client.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["CN=Example Root CA, OU=Infrastructure Team, DC=example, DC=com"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_client_issuer()
      :"tls.client.issuer"

  ### Erlang

  ```erlang
  ?TLS_CLIENT_ISSUER.
  'tls.client.issuer'
  ```

  <!-- tabs-close -->
  """
  @spec tls_client_issuer :: :"tls.client.issuer"
  def tls_client_issuer do
    :"tls.client.issuer"
  end

  @doc """
  A hash that identifies clients based on how they perform an SSL/TLS handshake.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["d4e5b18d6b55c71272893221c96ba240"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_client_ja3()
      :"tls.client.ja3"

  ### Erlang

  ```erlang
  ?TLS_CLIENT_JA3.
  'tls.client.ja3'
  ```

  <!-- tabs-close -->
  """
  @spec tls_client_ja3 :: :"tls.client.ja3"
  def tls_client_ja3 do
    :"tls.client.ja3"
  end

  @doc """
  Date/Time indicating when client certificate is no longer considered valid.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["2021-01-01T00:00:00.000Z"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_client_not_after()
      :"tls.client.not_after"

  ### Erlang

  ```erlang
  ?TLS_CLIENT_NOT_AFTER.
  'tls.client.not_after'
  ```

  <!-- tabs-close -->
  """
  @spec tls_client_not_after :: :"tls.client.not_after"
  def tls_client_not_after do
    :"tls.client.not_after"
  end

  @doc """
  Date/Time indicating when client certificate is first considered valid.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1970-01-01T00:00:00.000Z"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_client_not_before()
      :"tls.client.not_before"

  ### Erlang

  ```erlang
  ?TLS_CLIENT_NOT_BEFORE.
  'tls.client.not_before'
  ```

  <!-- tabs-close -->
  """
  @spec tls_client_not_before :: :"tls.client.not_before"
  def tls_client_not_before do
    :"tls.client.not_before"
  end

  @deprecated """
  Replaced by `server.address.
  """
  @spec tls_client_server_name :: :"tls.client.server_name"
  def tls_client_server_name do
    :"tls.client.server_name"
  end

  @doc """
  Distinguished name of subject of the x.509 certificate presented by the client.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["CN=myclient, OU=Documentation Team, DC=example, DC=com"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_client_subject()
      :"tls.client.subject"

  ### Erlang

  ```erlang
  ?TLS_CLIENT_SUBJECT.
  'tls.client.subject'
  ```

  <!-- tabs-close -->
  """
  @spec tls_client_subject :: :"tls.client.subject"
  def tls_client_subject do
    :"tls.client.subject"
  end

  @doc """
  Array of ciphers offered by the client during the client hello.
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384", "TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384", "..."]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_client_supported_ciphers()
      :"tls.client.supported_ciphers"

  ### Erlang

  ```erlang
  ?TLS_CLIENT_SUPPORTED_CIPHERS.
  'tls.client.supported_ciphers'
  ```

  <!-- tabs-close -->
  """
  @spec tls_client_supported_ciphers :: :"tls.client.supported_ciphers"
  def tls_client_supported_ciphers do
    :"tls.client.supported_ciphers"
  end

  @doc """
  String indicating the curve used for the given cipher, when applicable
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["secp256r1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_curve()
      :"tls.curve"

  ### Erlang

  ```erlang
  ?TLS_CURVE.
  'tls.curve'
  ```

  <!-- tabs-close -->
  """
  @spec tls_curve :: :"tls.curve"
  def tls_curve do
    :"tls.curve"
  end

  @doc """
  Boolean flag indicating if the TLS negotiation was successful and transitioned to an encrypted tunnel.
  ### Value type

  Value must be of type `boolean()`.
  ### Examples

  ```
  [true]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_established()
      :"tls.established"

  ### Erlang

  ```erlang
  ?TLS_ESTABLISHED.
  'tls.established'
  ```

  <!-- tabs-close -->
  """
  @spec tls_established :: :"tls.established"
  def tls_established do
    :"tls.established"
  end

  @doc """
  String indicating the protocol being tunneled. Per the values in the [IANA registry](https://www.iana.org/assignments/tls-extensiontype-values/tls-extensiontype-values.xhtml#alpn-protocol-ids), this string should be lower case.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["http/1.1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_next_protocol()
      :"tls.next_protocol"

  ### Erlang

  ```erlang
  ?TLS_NEXT_PROTOCOL.
  'tls.next_protocol'
  ```

  <!-- tabs-close -->
  """
  @spec tls_next_protocol :: :"tls.next_protocol"
  def tls_next_protocol do
    :"tls.next_protocol"
  end

  @typedoc """
  Normalized lowercase protocol name parsed from original string of the negotiated [SSL/TLS protocol version](https://www.openssl.org/docs/man1.1.1/man3/SSL_get_version.html#RETURN-VALUES)


  ### Enum Values
  * `:ssl` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:tls` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type tls_protocol_name_values() :: %{
          :ssl => :ssl,
          :tls => :tls
        }
  @doc """
  Normalized lowercase protocol name parsed from original string of the negotiated [SSL/TLS protocol version](https://www.openssl.org/docs/man1.1.1/man3/SSL_get_version.html#RETURN-VALUES)



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_protocol_name()
      :"tls.protocol.name"

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_protocol_name_values().ssl
      :ssl

      iex> %{OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_protocol_name() => OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_protocol_name_values().ssl}
      %{:"tls.protocol.name" => :ssl}

  ### Erlang

  ```erlang
  ?TLS_PROTOCOL_NAME.
  'tls.protocol.name'

  ?TLS_PROTOCOL_NAME_VALUES_SSL.
  'ssl'

  \#{?TLS_PROTOCOL_NAME => ?TLS_PROTOCOL_NAME_VALUES_SSL}.
  \#{'tls.protocol.name' => 'ssl'}
  ```

  <!-- tabs-close -->
  """
  @spec tls_protocol_name :: :"tls.protocol.name"
  def tls_protocol_name do
    :"tls.protocol.name"
  end

  @spec tls_protocol_name_values() :: tls_protocol_name_values()
  def tls_protocol_name_values() do
    %{
      :ssl => :ssl,
      :tls => :tls
    }
  end

  @doc """
  Numeric part of the version parsed from the original string of the negotiated [SSL/TLS protocol version](https://www.openssl.org/docs/man1.1.1/man3/SSL_get_version.html#RETURN-VALUES)

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1.2", "3"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_protocol_version()
      :"tls.protocol.version"

  ### Erlang

  ```erlang
  ?TLS_PROTOCOL_VERSION.
  'tls.protocol.version'
  ```

  <!-- tabs-close -->
  """
  @spec tls_protocol_version :: :"tls.protocol.version"
  def tls_protocol_version do
    :"tls.protocol.version"
  end

  @doc """
  Boolean flag indicating if this TLS connection was resumed from an existing TLS negotiation.
  ### Value type

  Value must be of type `boolean()`.
  ### Examples

  ```
  [true]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_resumed()
      :"tls.resumed"

  ### Erlang

  ```erlang
  ?TLS_RESUMED.
  'tls.resumed'
  ```

  <!-- tabs-close -->
  """
  @spec tls_resumed :: :"tls.resumed"
  def tls_resumed do
    :"tls.resumed"
  end

  @doc """
  PEM-encoded stand-alone certificate offered by the server. This is usually mutually-exclusive of `server.certificate_chain` since this value also exists in that list.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["MII..."]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_server_certificate()
      :"tls.server.certificate"

  ### Erlang

  ```erlang
  ?TLS_SERVER_CERTIFICATE.
  'tls.server.certificate'
  ```

  <!-- tabs-close -->
  """
  @spec tls_server_certificate :: :"tls.server.certificate"
  def tls_server_certificate do
    :"tls.server.certificate"
  end

  @doc """
  Array of PEM-encoded certificates that make up the certificate chain offered by the server. This is usually mutually-exclusive of `server.certificate` since that value should be the first certificate in the chain.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["MII...", "MI..."]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_server_certificate_chain()
      :"tls.server.certificate_chain"

  ### Erlang

  ```erlang
  ?TLS_SERVER_CERTIFICATE_CHAIN.
  'tls.server.certificate_chain'
  ```

  <!-- tabs-close -->
  """
  @spec tls_server_certificate_chain :: :"tls.server.certificate_chain"
  def tls_server_certificate_chain do
    :"tls.server.certificate_chain"
  end

  @doc """
  Certificate fingerprint using the MD5 digest of DER-encoded version of certificate offered by the server. For consistency with other hash values, this value should be formatted as an uppercase hash.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["0F76C7F2C55BFD7D8E8B8F4BFBF0C9EC"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_server_hash_md5()
      :"tls.server.hash.md5"

  ### Erlang

  ```erlang
  ?TLS_SERVER_HASH_MD5.
  'tls.server.hash.md5'
  ```

  <!-- tabs-close -->
  """
  @spec tls_server_hash_md5 :: :"tls.server.hash.md5"
  def tls_server_hash_md5 do
    :"tls.server.hash.md5"
  end

  @doc """
  Certificate fingerprint using the SHA1 digest of DER-encoded version of certificate offered by the server. For consistency with other hash values, this value should be formatted as an uppercase hash.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["9E393D93138888D288266C2D915214D1D1CCEB2A"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_server_hash_sha1()
      :"tls.server.hash.sha1"

  ### Erlang

  ```erlang
  ?TLS_SERVER_HASH_SHA1.
  'tls.server.hash.sha1'
  ```

  <!-- tabs-close -->
  """
  @spec tls_server_hash_sha1 :: :"tls.server.hash.sha1"
  def tls_server_hash_sha1 do
    :"tls.server.hash.sha1"
  end

  @doc """
  Certificate fingerprint using the SHA256 digest of DER-encoded version of certificate offered by the server. For consistency with other hash values, this value should be formatted as an uppercase hash.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["0687F666A054EF17A08E2F2162EAB4CBC0D265E1D7875BE74BF3C712CA92DAF0"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_server_hash_sha256()
      :"tls.server.hash.sha256"

  ### Erlang

  ```erlang
  ?TLS_SERVER_HASH_SHA256.
  'tls.server.hash.sha256'
  ```

  <!-- tabs-close -->
  """
  @spec tls_server_hash_sha256 :: :"tls.server.hash.sha256"
  def tls_server_hash_sha256 do
    :"tls.server.hash.sha256"
  end

  @doc """
  Distinguished name of [subject](https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.6) of the issuer of the x.509 certificate presented by the client.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["CN=Example Root CA, OU=Infrastructure Team, DC=example, DC=com"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_server_issuer()
      :"tls.server.issuer"

  ### Erlang

  ```erlang
  ?TLS_SERVER_ISSUER.
  'tls.server.issuer'
  ```

  <!-- tabs-close -->
  """
  @spec tls_server_issuer :: :"tls.server.issuer"
  def tls_server_issuer do
    :"tls.server.issuer"
  end

  @doc """
  A hash that identifies servers based on how they perform an SSL/TLS handshake.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["d4e5b18d6b55c71272893221c96ba240"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_server_ja3s()
      :"tls.server.ja3s"

  ### Erlang

  ```erlang
  ?TLS_SERVER_JA3S.
  'tls.server.ja3s'
  ```

  <!-- tabs-close -->
  """
  @spec tls_server_ja3s :: :"tls.server.ja3s"
  def tls_server_ja3s do
    :"tls.server.ja3s"
  end

  @doc """
  Date/Time indicating when server certificate is no longer considered valid.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["2021-01-01T00:00:00.000Z"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_server_not_after()
      :"tls.server.not_after"

  ### Erlang

  ```erlang
  ?TLS_SERVER_NOT_AFTER.
  'tls.server.not_after'
  ```

  <!-- tabs-close -->
  """
  @spec tls_server_not_after :: :"tls.server.not_after"
  def tls_server_not_after do
    :"tls.server.not_after"
  end

  @doc """
  Date/Time indicating when server certificate is first considered valid.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1970-01-01T00:00:00.000Z"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_server_not_before()
      :"tls.server.not_before"

  ### Erlang

  ```erlang
  ?TLS_SERVER_NOT_BEFORE.
  'tls.server.not_before'
  ```

  <!-- tabs-close -->
  """
  @spec tls_server_not_before :: :"tls.server.not_before"
  def tls_server_not_before do
    :"tls.server.not_before"
  end

  @doc """
  Distinguished name of subject of the x.509 certificate presented by the server.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["CN=myserver, OU=Documentation Team, DC=example, DC=com"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TLSAttributes.tls_server_subject()
      :"tls.server.subject"

  ### Erlang

  ```erlang
  ?TLS_SERVER_SUBJECT.
  'tls.server.subject'
  ```

  <!-- tabs-close -->
  """
  @spec tls_server_subject :: :"tls.server.subject"
  def tls_server_subject do
    :"tls.server.subject"
  end
end
