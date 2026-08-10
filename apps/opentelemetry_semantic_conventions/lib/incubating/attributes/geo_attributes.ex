defmodule OpenTelemetry.SemConv.Incubating.GeoAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Geo attributes.
  """

  @typedoc """
  Two-letter code representing continent’s name.


  ### Enum Values
  * `:af` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Africa
  * `:an` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Antarctica
  * `:as` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Asia
  * `:eu` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Europe
  * `:na` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - North America
  * `:oc` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Oceania
  * `:sa` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - South America
  """
  @type geo_continent_code_values() :: %{
          :af => :AF,
          :an => :AN,
          :as => :AS,
          :eu => :EU,
          :na => :NA,
          :oc => :OC,
          :sa => :SA
        }
  @doc """
  Two-letter code representing continent’s name.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GeoAttributes.geo_continent_code()
      :"geo.continent.code"

      iex> OpenTelemetry.SemConv.Incubating.GeoAttributes.geo_continent_code_values().af
      :AF

      iex> %{OpenTelemetry.SemConv.Incubating.GeoAttributes.geo_continent_code() => OpenTelemetry.SemConv.Incubating.GeoAttributes.geo_continent_code_values().af}
      %{:"geo.continent.code" => :AF}

  ### Erlang

  ```erlang
  ?GEO_CONTINENT_CODE.
  'geo.continent.code'

  ?GEO_CONTINENT_CODE_VALUES_AF.
  'AF'

  \#{?GEO_CONTINENT_CODE => ?GEO_CONTINENT_CODE_VALUES_AF}.
  \#{'geo.continent.code' => 'AF'}
  ```

  <!-- tabs-close -->
  """
  @spec geo_continent_code :: :"geo.continent.code"
  def geo_continent_code do
    :"geo.continent.code"
  end

  @spec geo_continent_code_values() :: geo_continent_code_values()
  def geo_continent_code_values() do
    %{
      :af => :AF,
      :an => :AN,
      :as => :AS,
      :eu => :EU,
      :na => :NA,
      :oc => :OC,
      :sa => :SA
    }
  end

  @doc """
  Two-letter ISO Country Code ([ISO 3166-1 alpha2](https://wikipedia.org/wiki/ISO_3166-1#Codes)).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["CA"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GeoAttributes.geo_country_iso_code()
      :"geo.country.iso_code"

  ### Erlang

  ```erlang
  ?GEO_COUNTRY_ISO_CODE.
  'geo.country.iso_code'
  ```

  <!-- tabs-close -->
  """
  @spec geo_country_iso_code :: :"geo.country.iso_code"
  def geo_country_iso_code do
    :"geo.country.iso_code"
  end

  @doc """
  Locality name. Represents the name of a city, town, village, or similar populated place.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Montreal", "Berlin"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GeoAttributes.geo_locality_name()
      :"geo.locality.name"

  ### Erlang

  ```erlang
  ?GEO_LOCALITY_NAME.
  'geo.locality.name'
  ```

  <!-- tabs-close -->
  """
  @spec geo_locality_name :: :"geo.locality.name"
  def geo_locality_name do
    :"geo.locality.name"
  end

  @doc """
  Latitude of the geo location in [WGS84](https://wikipedia.org/wiki/World_Geodetic_System#WGS84).

  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [45.505918]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GeoAttributes.geo_location_lat()
      :"geo.location.lat"

  ### Erlang

  ```erlang
  ?GEO_LOCATION_LAT.
  'geo.location.lat'
  ```

  <!-- tabs-close -->
  """
  @spec geo_location_lat :: :"geo.location.lat"
  def geo_location_lat do
    :"geo.location.lat"
  end

  @doc """
  Longitude of the geo location in [WGS84](https://wikipedia.org/wiki/World_Geodetic_System#WGS84).

  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [-73.61483]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GeoAttributes.geo_location_lon()
      :"geo.location.lon"

  ### Erlang

  ```erlang
  ?GEO_LOCATION_LON.
  'geo.location.lon'
  ```

  <!-- tabs-close -->
  """
  @spec geo_location_lon :: :"geo.location.lon"
  def geo_location_lon do
    :"geo.location.lon"
  end

  @doc """
  Postal code associated with the location. Values appropriate for this field may also be known as a postcode or ZIP code and will vary widely from country to country.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["94040"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GeoAttributes.geo_postal_code()
      :"geo.postal_code"

  ### Erlang

  ```erlang
  ?GEO_POSTAL_CODE.
  'geo.postal_code'
  ```

  <!-- tabs-close -->
  """
  @spec geo_postal_code :: :"geo.postal_code"
  def geo_postal_code do
    :"geo.postal_code"
  end

  @doc """
  Region ISO code ([ISO 3166-2](https://wikipedia.org/wiki/ISO_3166-2)).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["CA-QC"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GeoAttributes.geo_region_iso_code()
      :"geo.region.iso_code"

  ### Erlang

  ```erlang
  ?GEO_REGION_ISO_CODE.
  'geo.region.iso_code'
  ```

  <!-- tabs-close -->
  """
  @spec geo_region_iso_code :: :"geo.region.iso_code"
  def geo_region_iso_code do
    :"geo.region.iso_code"
  end
end
