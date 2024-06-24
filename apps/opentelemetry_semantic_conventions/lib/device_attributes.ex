defmodule OpenTelemetry.SemanticConventions.DeviceAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Device attributes.
  """

  @doc """
  A unique identifier representing the device

  ### Notes

  The device identifier **MUST** only be defined using the values outlined below. This value is not an advertising identifier and **MUST** **NOT** be used as such. On iOS (Swift or Objective-C), this value **MUST** be equal to the [vendor identifier](https://developer.apple.com/documentation/uikit/uidevice/1620059-identifierforvendor). On Android (Java or Kotlin), this value **MUST** be equal to the Firebase Installation ID or a globally unique UUID which is persisted across sessions in your application. More information can be found [here](https://developer.android.com/training/articles/user-data-ids) on best practices and exact implementation details. Caution should be taken when storing personal data or anything which can identify a user. GDPR and data protection laws may apply, ensure you do your own due diligence.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DeviceAttributes.device_id()
      :"device.id"
  """
  @spec device_id :: :"device.id"
  def device_id do
    :"device.id"
  end

  @doc """
  The name of the device manufacturer

  ### Notes

  The Android OS provides this field via [Build](https://developer.android.com/reference/android/os/Build#MANUFACTURER). iOS apps **SHOULD** hardcode the value `Apple`.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DeviceAttributes.device_manufacturer()
      :"device.manufacturer"
  """
  @spec device_manufacturer :: :"device.manufacturer"
  def device_manufacturer do
    :"device.manufacturer"
  end

  @doc """
  The model identifier for the device

  ### Notes

  It's recommended this value represents a machine-readable version of the model identifier rather than the market or consumer-friendly name of the device.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DeviceAttributes.device_model_identifier()
      :"device.model.identifier"
  """
  @spec device_model_identifier :: :"device.model.identifier"
  def device_model_identifier do
    :"device.model.identifier"
  end

  @doc """
  The marketing name for the device model

  ### Notes

  It's recommended this value represents a human-readable version of the device model rather than a machine-readable alternative.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DeviceAttributes.device_model_name()
      :"device.model.name"
  """
  @spec device_model_name :: :"device.model.name"
  def device_model_name do
    :"device.model.name"
  end
end
