defmodule OpenTelemetry.SemConv.Incubating.AndroidAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Android attributes.
  """

  @doc """
  Uniquely identifies the framework API revision offered by a version (`os.version`) of the android operating system. More information can be found [here](https://developer.android.com/guide/topics/manifest/uses-sdk-element#ApiLevels).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["33", "32"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AndroidAttributes.android_os_api_level()
      :"android.os.api_level"

  ### Erlang

  ```erlang
  ?ANDROID_OS_API_LEVEL.
  'android.os.api_level'
  ```

  <!-- tabs-close -->
  """
  @spec android_os_api_level :: :"android.os.api_level"
  def android_os_api_level do
    :"android.os.api_level"
  end

  @typedoc """
  Deprecated use the `device.app.lifecycle` event definition including `android.state` as a payload field instead.


  ### Enum Values
  * `:created` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Any time before Activity.onResume() or, if the app has no Activity, Context.startService() has been called in the app for the first time.

  * `:background` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Any time after Activity.onPause() or, if the app has no Activity, Context.stopService() has been called when the app was in the foreground state.

  * `:foreground` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Any time after Activity.onResume() or, if the app has no Activity, Context.startService() has been called when the app was in either the created or background states.

  """
  @type android_state_values() :: %{
          :created => :created,
          :background => :background,
          :foreground => :foreground
        }
  @doc """
  Deprecated use the `device.app.lifecycle` event definition including `android.state` as a payload field instead.


  ### Notes

  The Android lifecycle states are defined in [Activity lifecycle callbacks](https://developer.android.com/guide/components/activities/activity-lifecycle#lc), and from which the `OS identifiers` are derived.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AndroidAttributes.android_state()
      :"android.state"

      iex> OpenTelemetry.SemConv.Incubating.AndroidAttributes.android_state_values().created
      :created

      iex> %{OpenTelemetry.SemConv.Incubating.AndroidAttributes.android_state() => OpenTelemetry.SemConv.Incubating.AndroidAttributes.android_state_values().created}
      %{:"android.state" => :created}

  ### Erlang

  ```erlang
  ?ANDROID_STATE.
  'android.state'

  ?ANDROID_STATE_VALUES_CREATED.
  'created'

  \#{?ANDROID_STATE => ?ANDROID_STATE_VALUES_CREATED}.
  \#{'android.state' => 'created'}
  ```

  <!-- tabs-close -->
  """
  @spec android_state :: :"android.state"
  def android_state do
    :"android.state"
  end

  @spec android_state_values() :: android_state_values()
  def android_state_values() do
    %{
      :created => :created,
      :background => :background,
      :foreground => :foreground
    }
  end
end
