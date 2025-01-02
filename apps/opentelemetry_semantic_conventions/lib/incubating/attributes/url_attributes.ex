defmodule OpenTelemetry.SemConv.Incubating.URLAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for URL attributes.
  """
  defdelegate url_fragment(), to: OpenTelemetry.SemConv.URLAttributes

  defdelegate url_full(), to: OpenTelemetry.SemConv.URLAttributes

  defdelegate url_path(), to: OpenTelemetry.SemConv.URLAttributes

  defdelegate url_query(), to: OpenTelemetry.SemConv.URLAttributes

  defdelegate url_scheme(), to: OpenTelemetry.SemConv.URLAttributes

  @doc """
  Domain extracted from the `url.full`, such as "opentelemetry.io".

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  In some cases a URL may refer to an IP and/or port directly, without a domain name. In this case, the IP address would go to the domain field. If the URL contains a [literal IPv6 address](https://www.rfc-editor.org/rfc/rfc2732#section-2) enclosed by `[` and `]`, the `[` and `]` characters should also be captured in the domain field.

  ### Examples

  ```
  ["www.foo.bar", "opentelemetry.io", "3.12.167.2", "[1080:0:0:0:8:800:200C:417A]"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.URLAttributes.url_domain()
      :"url.domain"

  ### Erlang

  ```erlang
  ?URL_DOMAIN.
  'url.domain'
  ```

  <!-- tabs-close -->
  """
  @spec url_domain :: :"url.domain"
  def url_domain do
    :"url.domain"
  end

  @doc """
  The file extension extracted from the `url.full`, excluding the leading dot.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The file extension is only set if it exists, as not every url has a file extension. When the file name has multiple extensions `example.tar.gz`, only the last one should be captured `gz`, not `tar.gz`.

  ### Examples

  ```
  ["png", "gz"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.URLAttributes.url_extension()
      :"url.extension"

  ### Erlang

  ```erlang
  ?URL_EXTENSION.
  'url.extension'
  ```

  <!-- tabs-close -->
  """
  @spec url_extension :: :"url.extension"
  def url_extension do
    :"url.extension"
  end

  @doc """
  Unmodified original URL as seen in the event source.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  In network monitoring, the observed URL may be a full URL, whereas in access logs, the URL is often just represented as a path. This field is meant to represent the URL as it was observed, complete or not.
  `url.original` might contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case password and username **SHOULD** **NOT** be redacted and attribute's value **SHOULD** remain the same.

  ### Examples

  ```
  ["https://www.foo.bar/search?q=OpenTelemetry#SemConv", "search?q=OpenTelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.URLAttributes.url_original()
      :"url.original"

  ### Erlang

  ```erlang
  ?URL_ORIGINAL.
  'url.original'
  ```

  <!-- tabs-close -->
  """
  @spec url_original :: :"url.original"
  def url_original do
    :"url.original"
  end

  @doc """
  Port extracted from the `url.full`

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [443]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.URLAttributes.url_port()
      :"url.port"

  ### Erlang

  ```erlang
  ?URL_PORT.
  'url.port'
  ```

  <!-- tabs-close -->
  """
  @spec url_port :: :"url.port"
  def url_port do
    :"url.port"
  end

  @doc """
  The highest registered url domain, stripped of the subdomain.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This value can be determined precisely with the [public suffix list](http://publicsuffix.org). For example, the registered domain for `foo.example.com` is `example.com`. Trying to approximate this by simply taking the last two labels will not work well for TLDs such as `co.uk`.

  ### Examples

  ```
  ["example.com", "foo.co.uk"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.URLAttributes.url_registered_domain()
      :"url.registered_domain"

  ### Erlang

  ```erlang
  ?URL_REGISTERED_DOMAIN.
  'url.registered_domain'
  ```

  <!-- tabs-close -->
  """
  @spec url_registered_domain :: :"url.registered_domain"
  def url_registered_domain do
    :"url.registered_domain"
  end

  @doc """
  The subdomain portion of a fully qualified domain name includes all of the names except the host name under the registered_domain. In a partially qualified domain, or if the qualification level of the full name cannot be determined, subdomain contains all of the names below the registered domain.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The subdomain portion of `www.east.mydomain.co.uk` is `east`. If the domain has multiple levels of subdomain, such as `sub2.sub1.example.com`, the subdomain field should contain `sub2.sub1`, with no trailing period.

  ### Examples

  ```
  ["east", "sub2.sub1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.URLAttributes.url_subdomain()
      :"url.subdomain"

  ### Erlang

  ```erlang
  ?URL_SUBDOMAIN.
  'url.subdomain'
  ```

  <!-- tabs-close -->
  """
  @spec url_subdomain :: :"url.subdomain"
  def url_subdomain do
    :"url.subdomain"
  end

  @doc """
  The low-cardinality template of an [absolute path reference](https://www.rfc-editor.org/rfc/rfc3986#section-4.2).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["/users/{id}", "/users/:id", "/users?id={id}"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.URLAttributes.url_template()
      :"url.template"

  ### Erlang

  ```erlang
  ?URL_TEMPLATE.
  'url.template'
  ```

  <!-- tabs-close -->
  """
  @spec url_template :: :"url.template"
  def url_template do
    :"url.template"
  end

  @doc """
  The effective top level domain (eTLD), also known as the domain suffix, is the last part of the domain name. For example, the top level domain for example.com is `com`.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This value can be determined precisely with the [public suffix list](http://publicsuffix.org).

  ### Examples

  ```
  ["com", "co.uk"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.URLAttributes.url_top_level_domain()
      :"url.top_level_domain"

  ### Erlang

  ```erlang
  ?URL_TOP_LEVEL_DOMAIN.
  'url.top_level_domain'
  ```

  <!-- tabs-close -->
  """
  @spec url_top_level_domain :: :"url.top_level_domain"
  def url_top_level_domain do
    :"url.top_level_domain"
  end
end
