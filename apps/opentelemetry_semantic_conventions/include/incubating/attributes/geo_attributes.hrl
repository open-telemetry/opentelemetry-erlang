
%%%------------------------------------------------------------------------
%% Copyright The OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%-------------------------------------------------------------------------
-include_lib("opentelemetry_semantic_conventions/include/attributes/geo_attributes.hrl").


%% Two-letter code representing continent’s name.
%%  
-define(GEO_CONTINENT_CODE, 'geo.continent.code').

-define(GEO_CONTINENT_CODE_VALUES_AF, 'AF').

-define(GEO_CONTINENT_CODE_VALUES_AN, 'AN').

-define(GEO_CONTINENT_CODE_VALUES_AS, 'AS').

-define(GEO_CONTINENT_CODE_VALUES_EU, 'EU').

-define(GEO_CONTINENT_CODE_VALUES_NA, 'NA').

-define(GEO_CONTINENT_CODE_VALUES_OC, 'OC').

-define(GEO_CONTINENT_CODE_VALUES_SA, 'SA').



%% Two-letter ISO Country Code ([ISO 3166-1 alpha2](https://wikipedia.org/wiki/ISO_3166-1#Codes)).
%%  
-define(GEO_COUNTRY_ISO_CODE, 'geo.country.iso_code').


%% Locality name. Represents the name of a city, town, village, or similar populated place.
%%  
-define(GEO_LOCALITY_NAME, 'geo.locality.name').


%% Latitude of the geo location in [WGS84](https://wikipedia.org/wiki/World_Geodetic_System#WGS84).
%%  
-define(GEO_LOCATION_LAT, 'geo.location.lat').


%% Longitude of the geo location in [WGS84](https://wikipedia.org/wiki/World_Geodetic_System#WGS84).
%%  
-define(GEO_LOCATION_LON, 'geo.location.lon').


%% Postal code associated with the location. Values appropriate for this field may also be known as a postcode or ZIP code and will vary widely from country to country.
%%  
-define(GEO_POSTAL_CODE, 'geo.postal_code').


%% Region ISO code ([ISO 3166-2](https://wikipedia.org/wiki/ISO_3166-2)).
%%  
-define(GEO_REGION_ISO_CODE, 'geo.region.iso_code').
