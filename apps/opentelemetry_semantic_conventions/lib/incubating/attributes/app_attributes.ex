defmodule OpenTelemetry.SemConv.Incubating.AppAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for App attributes.
  """

  @doc """
  A unique identifier representing the installation of an application on a specific device

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Its value **SHOULD** persist across launches of the same application installation, including through application upgrades.
  It **SHOULD** change if the application is uninstalled or if all applications of the vendor are uninstalled.
  Additionally, users might be able to reset this value (e.g. by clearing application data).
  If an app is installed multiple times on the same device (e.g. in different accounts on Android), each `app.installation.id` **SHOULD** have a different value.
  If multiple OpenTelemetry SDKs are used within the same application, they **SHOULD** use the same value for `app.installation.id`.
  Hardware IDs (e.g. serial number, IMEI, MAC address) **MUST** **NOT** be used as the `app.installation.id`.

  For iOS, this value **SHOULD** be equal to the [vendor identifier](https://developer.apple.com/documentation/uikit/uidevice/identifierforvendor).

  For Android, examples of `app.installation.id` implementations include:

  - [Firebase Installation ID](https://firebase.google.com/docs/projects/manage-installations).
  - A globally unique UUID which is persisted across sessions in your application.
  - [App set ID](https://developer.android.com/identity/app-set-id).
  - [`Settings.getString(Settings.Secure.ANDROID_ID)`](https://developer.android.com/reference/android/provider/Settings.Secure#ANDROID_ID).

  More information about Android identifier best practices can be found [here](https://developer.android.com/training/articles/user-data-ids).

  ### Examples

  ```
  ["2ab2916d-a51f-4ac8-80ee-45ac31a28092"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AppAttributes.app_installation_id()
      :"app.installation.id"

  ### Erlang

  ```erlang
  ?APP_INSTALLATION_ID.
  'app.installation.id'
  ```

  <!-- tabs-close -->
  """
  @spec app_installation_id :: :"app.installation.id"
  def app_installation_id do
    :"app.installation.id"
  end

  @doc """
  The x (horizontal) coordinate of a screen coordinate, in screen pixels.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [0, 131]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AppAttributes.app_screen_coordinate_x()
      :"app.screen.coordinate.x"

  ### Erlang

  ```erlang
  ?APP_SCREEN_COORDINATE_X.
  'app.screen.coordinate.x'
  ```

  <!-- tabs-close -->
  """
  @spec app_screen_coordinate_x :: :"app.screen.coordinate.x"
  def app_screen_coordinate_x do
    :"app.screen.coordinate.x"
  end

  @doc """
  The y (vertical) component of a screen coordinate, in screen pixels.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [12, 99]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AppAttributes.app_screen_coordinate_y()
      :"app.screen.coordinate.y"

  ### Erlang

  ```erlang
  ?APP_SCREEN_COORDINATE_Y.
  'app.screen.coordinate.y'
  ```

  <!-- tabs-close -->
  """
  @spec app_screen_coordinate_y :: :"app.screen.coordinate.y"
  def app_screen_coordinate_y do
    :"app.screen.coordinate.y"
  end

  @doc """
  An identifier that uniquely differentiates this widget from other widgets in the same application.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  A widget is an application component, typically an on-screen visual GUI element.

  ### Examples

  ```
  ["f9bc787d-ff05-48ad-90e1-fca1d46130b3", "submit_order_1829"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AppAttributes.app_widget_id()
      :"app.widget.id"

  ### Erlang

  ```erlang
  ?APP_WIDGET_ID.
  'app.widget.id'
  ```

  <!-- tabs-close -->
  """
  @spec app_widget_id :: :"app.widget.id"
  def app_widget_id do
    :"app.widget.id"
  end

  @doc """
  The name of an application widget.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  A widget is an application component, typically an on-screen visual GUI element.

  ### Examples

  ```
  ["submit", "attack", "Clear Cart"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AppAttributes.app_widget_name()
      :"app.widget.name"

  ### Erlang

  ```erlang
  ?APP_WIDGET_NAME.
  'app.widget.name'
  ```

  <!-- tabs-close -->
  """
  @spec app_widget_name :: :"app.widget.name"
  def app_widget_name do
    :"app.widget.name"
  end
end
