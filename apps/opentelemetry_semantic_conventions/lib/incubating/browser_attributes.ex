defmodule OpenTelemetry.SemConv.Incubating.BrowserAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Browser attributes.
  """

  @doc """
  Array of brand name and version separated by a space
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.brands`).

  ### Examples

  ```
  [" Not A;Brand 99", "Chromium 99", "Chrome 99"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.BrowserAttributes.browser_brands()
      :"browser.brands"

  ### Erlang

  ```erlang
  ?BROWSER_BRANDS.
  'browser.brands'
  ```

  <!-- tabs-close -->
  """
  @spec browser_brands :: :"browser.brands"
  def browser_brands do
    :"browser.brands"
  end

  @doc """
  Preferred language of the user using the browser
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This value is intended to be taken from the Navigator API `navigator.language`.

  ### Examples

  ```
  ["en", "en-US", "fr", "fr-FR"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.BrowserAttributes.browser_language()
      :"browser.language"

  ### Erlang

  ```erlang
  ?BROWSER_LANGUAGE.
  'browser.language'
  ```

  <!-- tabs-close -->
  """
  @spec browser_language :: :"browser.language"
  def browser_language do
    :"browser.language"
  end

  @doc """
  A boolean that is true if the browser is running on a mobile device
  ### Value type

  Value must be of type `boolean()`.
  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.mobile`). If unavailable, this attribute **SHOULD** be left unset.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.BrowserAttributes.browser_mobile()
      :"browser.mobile"

  ### Erlang

  ```erlang
  ?BROWSER_MOBILE.
  'browser.mobile'
  ```

  <!-- tabs-close -->
  """
  @spec browser_mobile :: :"browser.mobile"
  def browser_mobile do
    :"browser.mobile"
  end

  @doc """
  The platform on which the browser is running
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.platform`). If unavailable, the legacy `navigator.platform` API **SHOULD** **NOT** be used instead and this attribute **SHOULD** be left unset in order for the values to be consistent.
  The list of possible values is defined in the [W3C User-Agent Client Hints specification](https://wicg.github.io/ua-client-hints/#sec-ch-ua-platform). Note that some (but not all) of these values can overlap with values in the [`os.type` and `os.name` attributes](./os.md). However, for consistency, the values in the `browser.platform` attribute should capture the exact value that the user agent provides.

  ### Examples

  ```
  ["Windows", "macOS", "Android"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.BrowserAttributes.browser_platform()
      :"browser.platform"

  ### Erlang

  ```erlang
  ?BROWSER_PLATFORM.
  'browser.platform'
  ```

  <!-- tabs-close -->
  """
  @spec browser_platform :: :"browser.platform"
  def browser_platform do
    :"browser.platform"
  end
end
