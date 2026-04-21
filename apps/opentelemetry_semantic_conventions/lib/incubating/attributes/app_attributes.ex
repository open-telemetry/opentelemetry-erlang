defmodule OpenTelemetry.SemConv.Incubating.AppAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for App attributes.
  """

  @doc """
  Unique identifier for a particular build or compilation of the application.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["6cff0a7e-cefc-4668-96f5-1273d8b334d0", "9f2b833506aa6973a92fde9733e6271f", "my-app-1.0.0-code-123"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AppAttributes.app_build_id()
      :"app.build_id"

  ### Erlang

  ```erlang
  ?APP_BUILD_ID.
  'app.build_id'
  ```

  <!-- tabs-close -->
  """
  @spec app_build_id :: :"app.build_id"
  def app_build_id do
    :"app.build_id"
  end

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

  More information about Android identifier best practices can be found in the [Android user data IDs guide](https://developer.android.com/training/articles/user-data-ids).

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
  A number of frame renders that experienced jank.
  ### Value type

  Value must be of type `integer()`.
  ### Notes

  Depending on platform limitations, the value provided **MAY** be approximation.

  ### Examples

  ```
  [9, 42]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AppAttributes.app_jank_frame_count()
      :"app.jank.frame_count"

  ### Erlang

  ```erlang
  ?APP_JANK_FRAME_COUNT.
  'app.jank.frame_count'
  ```

  <!-- tabs-close -->
  """
  @spec app_jank_frame_count :: :"app.jank.frame_count"
  def app_jank_frame_count do
    :"app.jank.frame_count"
  end

  @doc """
  The time period, in seconds, for which this jank is being reported.
  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [1.0, 5.0, 10.24]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AppAttributes.app_jank_period()
      :"app.jank.period"

  ### Erlang

  ```erlang
  ?APP_JANK_PERIOD.
  'app.jank.period'
  ```

  <!-- tabs-close -->
  """
  @spec app_jank_period :: :"app.jank.period"
  def app_jank_period do
    :"app.jank.period"
  end

  @doc """
  The minimum rendering threshold for this jank, in seconds.
  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [0.016, 0.7, 1.024]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AppAttributes.app_jank_threshold()
      :"app.jank.threshold"

  ### Erlang

  ```erlang
  ?APP_JANK_THRESHOLD.
  'app.jank.threshold'
  ```

  <!-- tabs-close -->
  """
  @spec app_jank_threshold :: :"app.jank.threshold"
  def app_jank_threshold do
    :"app.jank.threshold"
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
  An identifier that uniquely differentiates this screen from other screens in the same application.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  A screen represents only the part of the device display drawn by the app. It typically contains multiple widgets or UI components and is larger in scope than individual widgets. Multiple screens can coexist on the same display simultaneously (e.g., split view on tablets).

  ### Examples

  ```
  ["f9bc787d-ff05-48ad-90e1-fca1d46130b3", "com.example.app.MainActivity", "com.example.shop.ProductDetailFragment", "MyApp.ProfileView", "MyApp.ProfileViewController"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AppAttributes.app_screen_id()
      :"app.screen.id"

  ### Erlang

  ```erlang
  ?APP_SCREEN_ID.
  'app.screen.id'
  ```

  <!-- tabs-close -->
  """
  @spec app_screen_id :: :"app.screen.id"
  def app_screen_id do
    :"app.screen.id"
  end

  @doc """
  The name of an application screen.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  A screen represents only the part of the device display drawn by the app. It typically contains multiple widgets or UI components and is larger in scope than individual widgets. Multiple screens can coexist on the same display simultaneously (e.g., split view on tablets).

  ### Examples

  ```
  ["MainActivity", "ProductDetailFragment", "ProfileView", "ProfileViewController"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.AppAttributes.app_screen_name()
      :"app.screen.name"

  ### Erlang

  ```erlang
  ?APP_SCREEN_NAME.
  'app.screen.name'
  ```

  <!-- tabs-close -->
  """
  @spec app_screen_name :: :"app.screen.name"
  def app_screen_name do
    :"app.screen.name"
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
