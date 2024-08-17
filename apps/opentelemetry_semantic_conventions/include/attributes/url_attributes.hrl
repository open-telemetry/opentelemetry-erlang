
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

%% The [URI fragment](https://www.rfc-editor.org/rfc/rfc3986#section-3.5) component
%%  
-define(URL_FRAGMENT, 'url.fragment').


%% Absolute URL describing a network resource according to [RFC3986](https://www.rfc-editor.org/rfc/rfc3986)
-define(URL_FULL, 'url.full').


%% The [URI path](https://www.rfc-editor.org/rfc/rfc3986#section-3.3) component
%%  
-define(URL_PATH, 'url.path').


%% The [URI query](https://www.rfc-editor.org/rfc/rfc3986#section-3.4) component
%%  
-define(URL_QUERY, 'url.query').


%% The [URI scheme](https://www.rfc-editor.org/rfc/rfc3986#section-3.1) component identifying the used protocol.
%%  
-define(URL_SCHEME, 'url.scheme').
