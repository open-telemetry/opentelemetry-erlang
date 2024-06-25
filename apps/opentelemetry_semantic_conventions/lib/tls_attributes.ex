defmodule OpenTelemetry.SemanticConventions.TlsAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Tls attributes.
  """

  @doc """
  String indicating the [cipher](https://datatracker.ietf.org/doc/html/rfc5246#appendix-A.5) used during the current connection.

  ### Notes

  The values allowed for `tls.cipher` **MUST** be one of the `Descriptions` of the [registered TLS Cipher Suits](https://www.iana.org/assignments/tls-parameters/tls-parameters.xhtml#table-tls-parameters-4).


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_cipher()
      :"tls.cipher"
  """
  @spec tls_cipher :: :"tls.cipher"
  def tls_cipher do
    :"tls.cipher"
  end

  @doc """
  PEM-encoded stand-alone certificate offered by the client. This is usually mutually-exclusive of `client.certificate_chain` since this value also exists in that list.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_client_certificate()
      :"tls.client.certificate"
  """
  @spec tls_client_certificate :: :"tls.client.certificate"
  def tls_client_certificate do
    :"tls.client.certificate"
  end

  @doc """
  Array of PEM-encoded certificates that make up the certificate chain offered by the client. This is usually mutually-exclusive of `client.certificate` since that value should be the first certificate in the chain.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_client_certificatechain()
      :"tls.client.certificate_chain"
  """
  @spec tls_client_certificatechain :: :"tls.client.certificate_chain"
  def tls_client_certificatechain do
    :"tls.client.certificate_chain"
  end

  @doc """
  Certificate fingerprint using the MD5 digest of DER-encoded version of certificate offered by the client. For consistency with other hash values, this value should be formatted as an uppercase hash.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_client_hash_md_5()
      :"tls.client.hash.md5"
  """
  @spec tls_client_hash_md_5 :: :"tls.client.hash.md5"
  def tls_client_hash_md_5 do
    :"tls.client.hash.md5"
  end

  @doc """
  Certificate fingerprint using the SHA1 digest of DER-encoded version of certificate offered by the client. For consistency with other hash values, this value should be formatted as an uppercase hash.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_client_hash_sha_1()
      :"tls.client.hash.sha1"
  """
  @spec tls_client_hash_sha_1 :: :"tls.client.hash.sha1"
  def tls_client_hash_sha_1 do
    :"tls.client.hash.sha1"
  end

  @doc """
  Certificate fingerprint using the SHA256 digest of DER-encoded version of certificate offered by the client. For consistency with other hash values, this value should be formatted as an uppercase hash.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_client_hash_sha_256()
      :"tls.client.hash.sha256"
  """
  @spec tls_client_hash_sha_256 :: :"tls.client.hash.sha256"
  def tls_client_hash_sha_256 do
    :"tls.client.hash.sha256"
  end

  @doc """
  Distinguished name of [subject](https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.6) of the issuer of the x.509 certificate presented by the client.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_client_issuer()
      :"tls.client.issuer"
  """
  @spec tls_client_issuer :: :"tls.client.issuer"
  def tls_client_issuer do
    :"tls.client.issuer"
  end

  @doc """
  A hash that identifies clients based on how they perform an SSL/TLS handshake.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_client_ja_3()
      :"tls.client.ja3"
  """
  @spec tls_client_ja_3 :: :"tls.client.ja3"
  def tls_client_ja_3 do
    :"tls.client.ja3"
  end

  @doc """
  Date/Time indicating when client certificate is no longer considered valid.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_client_notafter()
      :"tls.client.not_after"
  """
  @spec tls_client_notafter :: :"tls.client.not_after"
  def tls_client_notafter do
    :"tls.client.not_after"
  end

  @doc """
  Date/Time indicating when client certificate is first considered valid.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_client_notbefore()
      :"tls.client.not_before"
  """
  @spec tls_client_notbefore :: :"tls.client.not_before"
  def tls_client_notbefore do
    :"tls.client.not_before"
  end

  @doc """
  Also called an SNI, this tells the server which hostname to which the client is attempting to connect to.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_client_servername()
      :"tls.client.server_name"
  """
  @spec tls_client_servername :: :"tls.client.server_name"
  def tls_client_servername do
    :"tls.client.server_name"
  end

  @doc """
  Distinguished name of subject of the x.509 certificate presented by the client.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_client_subject()
      :"tls.client.subject"
  """
  @spec tls_client_subject :: :"tls.client.subject"
  def tls_client_subject do
    :"tls.client.subject"
  end

  @doc """
  Array of ciphers offered by the client during the client hello.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_client_supportedciphers()
      :"tls.client.supported_ciphers"
  """
  @spec tls_client_supportedciphers :: :"tls.client.supported_ciphers"
  def tls_client_supportedciphers do
    :"tls.client.supported_ciphers"
  end

  @doc """
  String indicating the curve used for the given cipher, when applicable


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_curve()
      :"tls.curve"
  """
  @spec tls_curve :: :"tls.curve"
  def tls_curve do
    :"tls.curve"
  end

  @doc """
  Boolean flag indicating if the TLS negotiation was successful and transitioned to an encrypted tunnel.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_established()
      :"tls.established"
  """
  @spec tls_established :: :"tls.established"
  def tls_established do
    :"tls.established"
  end

  @doc """
  String indicating the protocol being tunneled. Per the values in the [IANA registry](https://www.iana.org/assignments/tls-extensiontype-values/tls-extensiontype-values.xhtml#alpn-protocol-ids), this string should be lower case.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_nextprotocol()
      :"tls.next_protocol"
  """
  @spec tls_nextprotocol :: :"tls.next_protocol"
  def tls_nextprotocol do
    :"tls.next_protocol"
  end

  @typedoc """
  Normalized lowercase protocol name parsed from original string of the negotiated [SSL/TLS protocol version](https://www.openssl.org/docs/man1.1.1/man3/SSL_get_version.html#RETURN-VALUES)


  ### Enum Values
  * `:ssl` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:tls` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  """
  @type tls_protocol_name() :: %{
          :ssl => :ssl,
          :tls => :tls
        }
  @doc """
  Normalized lowercase protocol name parsed from original string of the negotiated [SSL/TLS protocol version](https://www.openssl.org/docs/man1.1.1/man3/SSL_get_version.html#RETURN-VALUES)



  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_protocol_name().ssl
      :ssl
      
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_protocol_name(:custom_value)
      :custom_value
  """
  @spec tls_protocol_name() :: tls_protocol_name()
  def tls_protocol_name() do
    %{
      :ssl => :ssl,
      :tls => :tls
    }
  end

  @spec tls_protocol_name(atom() | String.t()) :: atom() | String.t()
  def tls_protocol_name(custom_value) do
    custom_value
  end

  @doc """
  Numeric part of the version parsed from the original string of the negotiated [SSL/TLS protocol version](https://www.openssl.org/docs/man1.1.1/man3/SSL_get_version.html#RETURN-VALUES)



  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_protocol_version()
      :"tls.protocol.version"
  """
  @spec tls_protocol_version :: :"tls.protocol.version"
  def tls_protocol_version do
    :"tls.protocol.version"
  end

  @doc """
  Boolean flag indicating if this TLS connection was resumed from an existing TLS negotiation.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_resumed()
      :"tls.resumed"
  """
  @spec tls_resumed :: :"tls.resumed"
  def tls_resumed do
    :"tls.resumed"
  end

  @doc """
  PEM-encoded stand-alone certificate offered by the server. This is usually mutually-exclusive of `server.certificate_chain` since this value also exists in that list.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_server_certificate()
      :"tls.server.certificate"
  """
  @spec tls_server_certificate :: :"tls.server.certificate"
  def tls_server_certificate do
    :"tls.server.certificate"
  end

  @doc """
  Array of PEM-encoded certificates that make up the certificate chain offered by the server. This is usually mutually-exclusive of `server.certificate` since that value should be the first certificate in the chain.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_server_certificatechain()
      :"tls.server.certificate_chain"
  """
  @spec tls_server_certificatechain :: :"tls.server.certificate_chain"
  def tls_server_certificatechain do
    :"tls.server.certificate_chain"
  end

  @doc """
  Certificate fingerprint using the MD5 digest of DER-encoded version of certificate offered by the server. For consistency with other hash values, this value should be formatted as an uppercase hash.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_server_hash_md_5()
      :"tls.server.hash.md5"
  """
  @spec tls_server_hash_md_5 :: :"tls.server.hash.md5"
  def tls_server_hash_md_5 do
    :"tls.server.hash.md5"
  end

  @doc """
  Certificate fingerprint using the SHA1 digest of DER-encoded version of certificate offered by the server. For consistency with other hash values, this value should be formatted as an uppercase hash.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_server_hash_sha_1()
      :"tls.server.hash.sha1"
  """
  @spec tls_server_hash_sha_1 :: :"tls.server.hash.sha1"
  def tls_server_hash_sha_1 do
    :"tls.server.hash.sha1"
  end

  @doc """
  Certificate fingerprint using the SHA256 digest of DER-encoded version of certificate offered by the server. For consistency with other hash values, this value should be formatted as an uppercase hash.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_server_hash_sha_256()
      :"tls.server.hash.sha256"
  """
  @spec tls_server_hash_sha_256 :: :"tls.server.hash.sha256"
  def tls_server_hash_sha_256 do
    :"tls.server.hash.sha256"
  end

  @doc """
  Distinguished name of [subject](https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.6) of the issuer of the x.509 certificate presented by the client.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_server_issuer()
      :"tls.server.issuer"
  """
  @spec tls_server_issuer :: :"tls.server.issuer"
  def tls_server_issuer do
    :"tls.server.issuer"
  end

  @doc """
  A hash that identifies servers based on how they perform an SSL/TLS handshake.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_server_ja_3_s()
      :"tls.server.ja3s"
  """
  @spec tls_server_ja_3_s :: :"tls.server.ja3s"
  def tls_server_ja_3_s do
    :"tls.server.ja3s"
  end

  @doc """
  Date/Time indicating when server certificate is no longer considered valid.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_server_notafter()
      :"tls.server.not_after"
  """
  @spec tls_server_notafter :: :"tls.server.not_after"
  def tls_server_notafter do
    :"tls.server.not_after"
  end

  @doc """
  Date/Time indicating when server certificate is first considered valid.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_server_notbefore()
      :"tls.server.not_before"
  """
  @spec tls_server_notbefore :: :"tls.server.not_before"
  def tls_server_notbefore do
    :"tls.server.not_before"
  end

  @doc """
  Distinguished name of subject of the x.509 certificate presented by the server.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TlsAttributes.tls_server_subject()
      :"tls.server.subject"
  """
  @spec tls_server_subject :: :"tls.server.subject"
  def tls_server_subject do
    :"tls.server.subject"
  end
end
