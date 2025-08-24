defmodule OpenTelemetry.SemConv.Incubating.AndroidAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Android attributes.
  """

  @typedoc """
  This attribute represents the state of the application.


  ### Enum Values
  * `:created` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Any time before Activity.onResume() or, if the app has no Activity, Context.startService() has been called in the app for the first time.

  * `:background` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Any time after Activity.onPause() or, if the app has no Activity, Context.stopService() has been called when the app was in the foreground state.

  * `:foreground` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Any time after Activity.onResume() or, if the app has no Activity, Context.startService() has been called when the app was in either the created or background states.

  """
  @type android_app_state_values() :: %{
          :created => :created,
          :background => :background,
          :foreground => :foreground
        }
  @doc """
  This attribute represents the state of the application.


  ### Notes

  The Android lifecycle states are defined in [Activity lifecycle callbacks](https://developer.android.com/guide/components/activities/activity-lifecycle#lc), and from which the `OS identifiers` are derived.

  ### Examples

  ```
  ["created"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AndroidAttributes.android_app_state()
      :"android.app.state"

      iex> OpenTelemetry.SemConv.Incubating.AndroidAttributes.android_app_state_values().created
      :created

      iex> %{OpenTelemetry.SemConv.Incubating.AndroidAttributes.android_app_state() => OpenTelemetry.SemConv.Incubating.AndroidAttributes.android_app_state_values().created}
      %{:"android.app.state" => :created}

  ### Erlang

  ```erlang
  ?ANDROID_APP_STATE.
  'android.app.state'

  ?ANDROID_APP_STATE_VALUES_CREATED.
  'created'

  \#{?ANDROID_APP_STATE => ?ANDROID_APP_STATE_VALUES_CREATED}.
  \#{'android.app.state' => 'created'}
  ```

  <!-- tabs-close -->
  """
  @spec android_app_state :: :"android.app.state"
  def android_app_state do
    :"android.app.state"
  end

  @spec android_app_state_values() :: android_app_state_values()
  def android_app_state_values() do
    %{
      :created => :created,
      :background => :background,
      :foreground => :foreground
    }
  end

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
  Deprecated. Use `android.app.state` instead.

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
  @deprecated """
  Renamed to `android.app.state`
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
