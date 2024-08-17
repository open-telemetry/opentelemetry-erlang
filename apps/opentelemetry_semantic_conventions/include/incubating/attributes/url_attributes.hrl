
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/url_attributes.hrl").


%% Domain extracted from the `url.full`, such as "opentelemetry.io".
%%  
-define(URL_DOMAIN, 'url.domain').


%% The file extension extracted from the `url.full`, excluding the leading dot.
%%  
-define(URL_EXTENSION, 'url.extension').


%% Unmodified original URL as seen in the event source.
%%  
-define(URL_ORIGINAL, 'url.original').


%% Port extracted from the `url.full`
%%  
-define(URL_PORT, 'url.port').


%% The highest registered url domain, stripped of the subdomain.
%%  
-define(URL_REGISTERED_DOMAIN, 'url.registered_domain').


%% The subdomain portion of a fully qualified domain name includes all of the names except the host name under the registered_domain. In a partially qualified domain, or if the qualification level of the full name cannot be determined, subdomain contains all of the names below the registered domain.
%%  
-define(URL_SUBDOMAIN, 'url.subdomain').


%% The low-cardinality template of an [absolute path reference](https://www.rfc-editor.org/rfc/rfc3986#section-4.2).
%%  
-define(URL_TEMPLATE, 'url.template').


%% The effective top level domain (eTLD), also known as the domain suffix, is the last part of the domain name. For example, the top level domain for example.com is `com`.
%%  
-define(URL_TOP_LEVEL_DOMAIN, 'url.top_level_domain').
