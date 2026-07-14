defmodule OpenTelemetry.SemConv.Incubating.DeviceAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Device attributes.
  """

  @doc """
  A unique identifier representing the device

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Its value **SHOULD** be identical for all apps on a device and it **SHOULD** **NOT** change if an app is uninstalled and re-installed.
  However, it might be resettable by the user for all apps on a device.
  Hardware IDs (e.g. vendor-specific serial number, IMEI or MAC address) **MAY** be used as values.

  More information about Android identifier best practices can be found [here](https://developer.android.com/training/articles/user-data-ids).

  > [!WARNING]
  >
  > This attribute may contain sensitive (PII) information. Caution should be taken when storing personal data or anything which can identify a user. GDPR and data protection laws may apply,
  > ensure you do your own due diligence.
  >
  > Due to these reasons, this identifier is not recommended for consumer applications and will likely result in rejection from both Google Play and App Store.
  > However, it may be appropriate for specific enterprise scenarios, such as kiosk devices or enterprise-managed devices, with appropriate compliance clearance.
  > Any instrumentation providing this identifier **MUST** implement it as an opt-in feature.
  >
  > See [`app.installation.id`](/docs/attributes-registry/app.md#app-installation-id) for a more privacy-preserving alternative.

  ### Examples

  ```
  ["123456789012345", "01:23:45:67:89:AB"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DeviceAttributes.device_id()
      :"device.id"

  ### Erlang

  ```erlang
  ?DEVICE_ID.
  'device.id'
  ```

  <!-- tabs-close -->
  """
  @spec device_id :: :"device.id"
  def device_id do
    :"device.id"
  end

  @doc """
  The name of the device manufacturer

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The Android OS provides this field via [Build](https://developer.android.com/reference/android/os/Build#MANUFACTURER). iOS apps **SHOULD** hardcode the value `Apple`.

  ### Examples

  ```
  ["Apple", "Samsung"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DeviceAttributes.device_manufacturer()
      :"device.manufacturer"

  ### Erlang

  ```erlang
  ?DEVICE_MANUFACTURER.
  'device.manufacturer'
  ```

  <!-- tabs-close -->
  """
  @spec device_manufacturer :: :"device.manufacturer"
  def device_manufacturer do
    :"device.manufacturer"
  end

  @doc """
  The model identifier for the device

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  It's recommended this value represents a machine-readable version of the model identifier rather than the market or consumer-friendly name of the device.

  ### Examples

  ```
  ["iPhone3,4", "SM-G920F"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DeviceAttributes.device_model_identifier()
      :"device.model.identifier"

  ### Erlang

  ```erlang
  ?DEVICE_MODEL_IDENTIFIER.
  'device.model.identifier'
  ```

  <!-- tabs-close -->
  """
  @spec device_model_identifier :: :"device.model.identifier"
  def device_model_identifier do
    :"device.model.identifier"
  end

  @doc """
  The marketing name for the device model

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  It's recommended this value represents a human-readable version of the device model rather than a machine-readable alternative.

  ### Examples

  ```
  ["iPhone 6s Plus", "Samsung Galaxy S6"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DeviceAttributes.device_model_name()
      :"device.model.name"

  ### Erlang

  ```erlang
  ?DEVICE_MODEL_NAME.
  'device.model.name'
  ```

  <!-- tabs-close -->
  """
  @spec device_model_name :: :"device.model.name"
  def device_model_name do
    :"device.model.name"
  end
end
