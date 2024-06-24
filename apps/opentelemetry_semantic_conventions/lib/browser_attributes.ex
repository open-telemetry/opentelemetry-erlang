defmodule OpenTelemetry.SemanticConventions.BrowserAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Browser attributes.
  """

  @doc """
  Array of brand name and version separated by a space
  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.brands`).


  ### Example
      iex> OpenTelemetry.SemanticConventions.BrowserAttributes.browser_brands()
      :"browser.brands"
  """
  @spec browser_brands :: :"browser.brands"
  def browser_brands do
    :"browser.brands"
  end

  @doc """
  Preferred language of the user using the browser
  ### Notes

  This value is intended to be taken from the Navigator API `navigator.language`.


  ### Example
      iex> OpenTelemetry.SemanticConventions.BrowserAttributes.browser_language()
      :"browser.language"
  """
  @spec browser_language :: :"browser.language"
  def browser_language do
    :"browser.language"
  end

  @doc """
  A boolean that is true if the browser is running on a mobile device
  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.mobile`). If unavailable, this attribute **SHOULD** be left unset.


  ### Example
      iex> OpenTelemetry.SemanticConventions.BrowserAttributes.browser_mobile()
      :"browser.mobile"
  """
  @spec browser_mobile :: :"browser.mobile"
  def browser_mobile do
    :"browser.mobile"
  end

  @doc """
  The platform on which the browser is running
  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.platform`). If unavailable, the legacy `navigator.platform` API **SHOULD** **NOT** be used instead and this attribute **SHOULD** be left unset in order for the values to be consistent.
  The list of possible values is defined in the [W3C User-Agent Client Hints specification](https://wicg.github.io/ua-client-hints/#sec-ch-ua-platform). Note that some (but not all) of these values can overlap with values in the [`os.type` and `os.name` attributes](./os.md). However, for consistency, the values in the `browser.platform` attribute should capture the exact value that the user agent provides.


  ### Example
      iex> OpenTelemetry.SemanticConventions.BrowserAttributes.browser_platform()
      :"browser.platform"
  """
  @spec browser_platform :: :"browser.platform"
  def browser_platform do
    :"browser.platform"
  end
end
