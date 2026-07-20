
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

%% Unique identifier for a particular build or compilation of the application.
-define(APP_BUILD_ID, 'app.build_id').


%% A unique identifier representing the installation of an application on a specific device
%%  
-define(APP_INSTALLATION_ID, 'app.installation.id').


%% A number of frame renders that experienced jank.
-define(APP_JANK_FRAME_COUNT, 'app.jank.frame_count').


%% The time period, in seconds, for which this jank is being reported.
-define(APP_JANK_PERIOD, 'app.jank.period').


%% The minimum rendering threshold for this jank, in seconds.
-define(APP_JANK_THRESHOLD, 'app.jank.threshold').


%% The x (horizontal) coordinate of a screen coordinate, in screen pixels.
-define(APP_SCREEN_COORDINATE_X, 'app.screen.coordinate.x').


%% The y (vertical) component of a screen coordinate, in screen pixels.
%%  
-define(APP_SCREEN_COORDINATE_Y, 'app.screen.coordinate.y').


%% An identifier that uniquely differentiates this screen from other screens in the same application.
%%  
-define(APP_SCREEN_ID, 'app.screen.id').


%% The name of an application screen.
-define(APP_SCREEN_NAME, 'app.screen.name').


%% An identifier that uniquely differentiates this widget from other widgets in the same application.
%%  
-define(APP_WIDGET_ID, 'app.widget.id').


%% The name of an application widget.
-define(APP_WIDGET_NAME, 'app.widget.name').
