defmodule OpenTelemetry.SemanticConventions.UrlAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Url attributes.
  """

  @doc """
  Domain extracted from the `url.full`, such as "opentelemetry.io".

  ### Notes

  In some cases a URL may refer to an IP and/or port directly, without a domain name. In this case, the IP address would go to the domain field. If the URL contains a [literal IPv6 address](https://www.rfc-editor.org/rfc/rfc2732#section-2) enclosed by `[` and `]`, the `[` and `]` characters should also be captured in the domain field.


  ### Example
      iex> OpenTelemetry.SemanticConventions.UrlAttributes.url_domain()
      :"url.domain"
  """
  @spec url_domain :: :"url.domain"
  def url_domain do
    :"url.domain"
  end

  @doc """
  The file extension extracted from the `url.full`, excluding the leading dot.

  ### Notes

  The file extension is only set if it exists, as not every url has a file extension. When the file name has multiple extensions `example.tar.gz`, only the last one should be captured `gz`, not `tar.gz`.


  ### Example
      iex> OpenTelemetry.SemanticConventions.UrlAttributes.url_extension()
      :"url.extension"
  """
  @spec url_extension :: :"url.extension"
  def url_extension do
    :"url.extension"
  end

  @doc """
  The [URI fragment](https://www.rfc-editor.org/rfc/rfc3986#section-3.5) component



  ### Example
      iex> OpenTelemetry.SemanticConventions.UrlAttributes.url_fragment()
      :"url.fragment"
  """
  @spec url_fragment :: :"url.fragment"
  def url_fragment do
    :"url.fragment"
  end

  @doc """
  Absolute URL describing a network resource according to [RFC3986](https://www.rfc-editor.org/rfc/rfc3986)
  ### Notes

  For network calls, URL usually has `scheme://host[:port][path][?query][#fragment]` format, where the fragment is not transmitted over HTTP, but if it is known, it **SHOULD** be included nevertheless.
  `url.full` **MUST** **NOT** contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case username and password **SHOULD** be redacted and attribute's value **SHOULD** be `https://REDACTED:REDACTED@www.example.com/`.
  `url.full` **SHOULD** capture the absolute URL when it is available (or can be reconstructed). Sensitive content provided in `url.full` **SHOULD** be scrubbed when instrumentations can identify it.


  ### Example
      iex> OpenTelemetry.SemanticConventions.UrlAttributes.url_full()
      :"url.full"
  """
  @spec url_full :: :"url.full"
  def url_full do
    :"url.full"
  end

  @doc """
  Unmodified original URL as seen in the event source.

  ### Notes

  In network monitoring, the observed URL may be a full URL, whereas in access logs, the URL is often just represented as a path. This field is meant to represent the URL as it was observed, complete or not.
  `url.original` might contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case password and username **SHOULD** **NOT** be redacted and attribute's value **SHOULD** remain the same.


  ### Example
      iex> OpenTelemetry.SemanticConventions.UrlAttributes.url_original()
      :"url.original"
  """
  @spec url_original :: :"url.original"
  def url_original do
    :"url.original"
  end

  @doc """
  The [URI path](https://www.rfc-editor.org/rfc/rfc3986#section-3.3) component

  ### Notes

  Sensitive content provided in `url.path` **SHOULD** be scrubbed when instrumentations can identify it.


  ### Example
      iex> OpenTelemetry.SemanticConventions.UrlAttributes.url_path()
      :"url.path"
  """
  @spec url_path :: :"url.path"
  def url_path do
    :"url.path"
  end

  @doc """
  Port extracted from the `url.full`



  ### Example
      iex> OpenTelemetry.SemanticConventions.UrlAttributes.url_port()
      :"url.port"
  """
  @spec url_port :: :"url.port"
  def url_port do
    :"url.port"
  end

  @doc """
  The [URI query](https://www.rfc-editor.org/rfc/rfc3986#section-3.4) component

  ### Notes

  Sensitive content provided in `url.query` **SHOULD** be scrubbed when instrumentations can identify it.


  ### Example
      iex> OpenTelemetry.SemanticConventions.UrlAttributes.url_query()
      :"url.query"
  """
  @spec url_query :: :"url.query"
  def url_query do
    :"url.query"
  end

  @doc """
  The highest registered url domain, stripped of the subdomain.

  ### Notes

  This value can be determined precisely with the [public suffix list](http://publicsuffix.org). For example, the registered domain for `foo.example.com` is `example.com`. Trying to approximate this by simply taking the last two labels will not work well for TLDs such as `co.uk`.


  ### Example
      iex> OpenTelemetry.SemanticConventions.UrlAttributes.url_registereddomain()
      :"url.registered_domain"
  """
  @spec url_registereddomain :: :"url.registered_domain"
  def url_registereddomain do
    :"url.registered_domain"
  end

  @doc """
  The [URI scheme](https://www.rfc-editor.org/rfc/rfc3986#section-3.1) component identifying the used protocol.



  ### Example
      iex> OpenTelemetry.SemanticConventions.UrlAttributes.url_scheme()
      :"url.scheme"
  """
  @spec url_scheme :: :"url.scheme"
  def url_scheme do
    :"url.scheme"
  end

  @doc """
  The subdomain portion of a fully qualified domain name includes all of the names except the host name under the registered_domain. In a partially qualified domain, or if the qualification level of the full name cannot be determined, subdomain contains all of the names below the registered domain.

  ### Notes

  The subdomain portion of `www.east.mydomain.co.uk` is `east`. If the domain has multiple levels of subdomain, such as `sub2.sub1.example.com`, the subdomain field should contain `sub2.sub1`, with no trailing period.


  ### Example
      iex> OpenTelemetry.SemanticConventions.UrlAttributes.url_subdomain()
      :"url.subdomain"
  """
  @spec url_subdomain :: :"url.subdomain"
  def url_subdomain do
    :"url.subdomain"
  end

  @doc """
  The low-cardinality template of an [absolute path reference](https://www.rfc-editor.org/rfc/rfc3986#section-4.2).



  ### Example
      iex> OpenTelemetry.SemanticConventions.UrlAttributes.url_template()
      :"url.template"
  """
  @spec url_template :: :"url.template"
  def url_template do
    :"url.template"
  end

  @doc """
  The effective top level domain (eTLD), also known as the domain suffix, is the last part of the domain name. For example, the top level domain for example.com is `com`.

  ### Notes

  This value can be determined precisely with the [public suffix list](http://publicsuffix.org).


  ### Example
      iex> OpenTelemetry.SemanticConventions.UrlAttributes.url_topleveldomain()
      :"url.top_level_domain"
  """
  @spec url_topleveldomain :: :"url.top_level_domain"
  def url_topleveldomain do
    :"url.top_level_domain"
  end
end
