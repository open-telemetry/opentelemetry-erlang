defmodule OpenTelemetry.SemConv.URLAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for URL attributes.
  """

  @doc """
  The [URI fragment](https://www.rfc-editor.org/rfc/rfc3986#section-3.5) component

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["SemConv"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.URLAttributes.url_fragment()
      :"url.fragment"

  ### Erlang

  ```erlang
  ?URL_FRAGMENT.
  'url.fragment'
  ```

  <!-- tabs-close -->
  """
  @spec url_fragment :: :"url.fragment"
  def url_fragment do
    :"url.fragment"
  end

  @doc """
  Absolute URL describing a network resource according to [RFC3986](https://www.rfc-editor.org/rfc/rfc3986)
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  For network calls, URL usually has `scheme://host[:port][path][?query][#fragment]` format, where the fragment is not transmitted over HTTP, but if it is known, it **SHOULD** be included nevertheless.
  `url.full` **MUST** **NOT** contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case username and password **SHOULD** be redacted and attribute's value **SHOULD** be `https://REDACTED:REDACTED@www.example.com/`.
  `url.full` **SHOULD** capture the absolute URL when it is available (or can be reconstructed). Sensitive content provided in `url.full` **SHOULD** be scrubbed when instrumentations can identify it.

  ### Examples

  ```
  ["https://www.foo.bar/search?q=OpenTelemetry#SemConv", "//localhost"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.URLAttributes.url_full()
      :"url.full"

  ### Erlang

  ```erlang
  ?URL_FULL.
  'url.full'
  ```

  <!-- tabs-close -->
  """
  @spec url_full :: :"url.full"
  def url_full do
    :"url.full"
  end

  @doc """
  The [URI path](https://www.rfc-editor.org/rfc/rfc3986#section-3.3) component

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Sensitive content provided in `url.path` **SHOULD** be scrubbed when instrumentations can identify it.

  ### Examples

  ```
  ["/search"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.URLAttributes.url_path()
      :"url.path"

  ### Erlang

  ```erlang
  ?URL_PATH.
  'url.path'
  ```

  <!-- tabs-close -->
  """
  @spec url_path :: :"url.path"
  def url_path do
    :"url.path"
  end

  @doc """
  The [URI query](https://www.rfc-editor.org/rfc/rfc3986#section-3.4) component

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Sensitive content provided in `url.query` **SHOULD** be scrubbed when instrumentations can identify it.

  ### Examples

  ```
  ["q=OpenTelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.URLAttributes.url_query()
      :"url.query"

  ### Erlang

  ```erlang
  ?URL_QUERY.
  'url.query'
  ```

  <!-- tabs-close -->
  """
  @spec url_query :: :"url.query"
  def url_query do
    :"url.query"
  end

  @doc """
  The [URI scheme](https://www.rfc-editor.org/rfc/rfc3986#section-3.1) component identifying the used protocol.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["https", "ftp", "telnet"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.URLAttributes.url_scheme()
      :"url.scheme"

  ### Erlang

  ```erlang
  ?URL_SCHEME.
  'url.scheme'
  ```

  <!-- tabs-close -->
  """
  @spec url_scheme :: :"url.scheme"
  def url_scheme do
    :"url.scheme"
  end
end
