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

  The device identifier **MUST** only be defined using the values outlined below. This value is not an advertising identifier and **MUST** **NOT** be used as such. On iOS (Swift or Objective-C), this value **MUST** be equal to the [vendor identifier](https://developer.apple.com/documentation/uikit/uidevice/1620059-identifierforvendor). On Android (Java or Kotlin), this value **MUST** be equal to the Firebase Installation ID or a globally unique UUID which is persisted across sessions in your application. More information can be found [here](https://developer.android.com/training/articles/user-data-ids) on best practices and exact implementation details. Caution should be taken when storing personal data or anything which can identify a user. GDPR and data protection laws may apply, ensure you do your own due diligence.

  ### Examples

  ```
  ["2ab2916d-a51f-4ac8-80ee-45ac31a28092"]
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
