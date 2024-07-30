defmodule OpenTelemetry.SemConv do
  @moduledoc """
  OpenTelemetry Semantic Conventions


  ## Lifecycle Status

  The support guarantees and allowed changes are governed by the lifecycle of the document.

  [OpenTelemetry Document Status](https://opentelemetry.io/docs/specs/otel/document-status) conventions.

  ### Experimental

  Experimental items are denoted as such, e.g. `attribute`^e^. These items _can_ be changed
  but that does not happen frequently in practice.

  ## Usage

  Check out the Guides tab for detailed attribute information and usage.

  ### Migration from v1.13.0 (v0.2.0 package version) semantic conventions

  The structure of OpenTelemetry Semantic Conventions has evolved a great
  deal since the last version we have published. All attributes now live
  under a common attribute registry. In addition, attributes have been classified
  as stable or experimental.

  Attributes are now organized by attribute group and stability. The prior code
  has been kept in a deprecated status to allow backward compatability
  during migration.

  ### Erlang

  For Erlang include the semantic conventions header for the particular kind you
  need:

  ```
  -include_lib("opentelemetry_semantic_conventions/include/attributes/url_attributes.hrl").
  ```

  You can then use the macros for the attribute keys:

  ```
  ?URL_PATH
  ```

  #### Enums

  Enum Attribute types define each value in a macro with the attribute name prefixed.

  Enum Attributes allow for a user-supplied value when no pre-defined option exists. Users
  may set this value manually while paying attention to the required value type

  #### Incubating Attributes & Metrics

  Incubating attribute header files are located in the `incubating` folder and metrics under `incubating/metrics`.

  Experimental attributes are considered to be incubating. Attribute groups can
  contain attributes which are stable, experimental, or both. Experimental
  attributes are contained in an incubating header. Attribute groups containing
  attributes of both stability levels will have two header files in this case.

  ```
  -include_lib("opentelemetry_semantic_conventions/include/incubating/attributes/url_attributes.hrl").
  ```

  You can then use the macros for the experimental attribute keys:

  ```
  ?URL_DOMAIN
  ```

  ### Elixir

  Attributes in Elixir are defined as functions. To use an attribute, simply call it
  with that attribute's name.

  ```
  iex> OpenTelemetry.SemConv.URLAttributes.url_path()
  :"url.path"
  ```

  #### Enums

  Enum Attribute types are defined by a function that returns a map of all defined values.
  To get a particular value, you can use map dot or access patterns. Enum keys are always atoms.

  ```
  iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_system_values().postgresql
  :postgresql
  ```

  Enum Attributes allow for a user-supplied value when no pre-defined option exists. Users
  may set this value manually while paying attention to the required value type

  #### Incubating Attributes & Metrics

  Incubating attributes are located under the `OpenTelemetry.SemConv.Incubating`
  and `OpenTelemetry.SemConv.Incubating.Metrics` namespaces.

  Experimental attributes are considered to be incubating. Attribute groups can
  contain attributes which are stable, experimental, or both. Experimental
  attributes are contained in an incubating module. Attribute groups containing
  attributes of both stability levels will have two modules in this case.

  """

  @typedoc """
  HTTP stability opt-in
  """
  @type http_stability() :: :http | :http_dup | :default

  @typedoc """
  Map of stability opt-ins
  """
  @type stability_opt_ins() :: %{http: http_stability()}

  @doc """
  Map of stability opt-ins defined by the `OTEL_SEMCONV_STABILITY_OPT_IN` env var.

  Current valid options:

  * [http](migration-guide.md)
  """
  @spec stability_opt_in() :: stability_opt_ins()
  defdelegate stability_opt_in(), to: :opentelemetry_sem_conv
end
