
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

%% Provides an indication that multiple symbols map to this location's address, for example due to identical code folding by the linker. In that case the line information represents one of the multiple symbols. This field must be recomputed when the symbolization state of the profile changes.
%%  
-define(PPROF_LOCATION_IS_FOLDED, 'pprof.location.is_folded').


%% Indicates that there are filenames related to this mapping.
%%  
-define(PPROF_MAPPING_HAS_FILENAMES, 'pprof.mapping.has_filenames').


%% Indicates that there are functions related to this mapping.
%%  
-define(PPROF_MAPPING_HAS_FUNCTIONS, 'pprof.mapping.has_functions').


%% Indicates that there are inline frames related to this mapping.
%%  
-define(PPROF_MAPPING_HAS_INLINE_FRAMES, 'pprof.mapping.has_inline_frames').


%% Indicates that there are line numbers related to this mapping.
%%  
-define(PPROF_MAPPING_HAS_LINE_NUMBERS, 'pprof.mapping.has_line_numbers').


%% Free-form text associated with the profile. This field should not be used to store any machine-readable information, it is only for human-friendly content.
%%  
-define(PPROF_PROFILE_COMMENT, 'pprof.profile.comment').


%% Documentation link for this profile type.
%%  
-define(PPROF_PROFILE_DOC_URL, 'pprof.profile.doc_url').


%% Frames with Function.function_name fully matching the regexp will be dropped from the samples, along with their successors.
%%  
-define(PPROF_PROFILE_DROP_FRAMES, 'pprof.profile.drop_frames').


%% Frames with Function.function_name fully matching the regexp will be kept, even if it matches drop_frames.
%%  
-define(PPROF_PROFILE_KEEP_FRAMES, 'pprof.profile.keep_frames').


%% Records the pprof's default_sample_type in the original profile. Not set if the default sample type was missing.
%%  
-define(PPROF_SCOPE_DEFAULT_SAMPLE_TYPE, 'pprof.scope.default_sample_type').


%% Records the indexes of the sample types in the original profile.
%%  
-define(PPROF_SCOPE_SAMPLE_TYPE_ORDER, 'pprof.scope.sample_type_order').
