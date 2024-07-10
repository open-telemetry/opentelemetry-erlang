

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
-define(URL_REGISTEREDDOMAIN, 'url.registered_domain').


%% The subdomain portion of a fully qualified domain name includes all of the names except the host name under the registered_domain. In a partially qualified domain, or if the qualification level of the full name cannot be determined, subdomain contains all of the names below the registered domain.
%%  
-define(URL_SUBDOMAIN, 'url.subdomain').


%% The low-cardinality template of an [absolute path reference](https://www.rfc-editor.org/rfc/rfc3986#section-4.2).
%%  
-define(URL_TEMPLATE, 'url.template').


%% The effective top level domain (eTLD), also known as the domain suffix, is the last part of the domain name. For example, the top level domain for example.com is `com`.
%%  
-define(URL_TOPLEVELDOMAIN, 'url.top_level_domain').
