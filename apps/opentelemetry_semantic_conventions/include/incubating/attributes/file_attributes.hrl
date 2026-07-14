
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/file_attributes.hrl").


%% Time when the file was last accessed, in ISO 8601 format.
%%  
-define(FILE_ACCESSED, 'file.accessed').


%% Array of file attributes.
%%  
-define(FILE_ATTRIBUTES, 'file.attributes').


%% Time when the file attributes or metadata was last changed, in ISO 8601 format.
%%  
-define(FILE_CHANGED, 'file.changed').


%% Time when the file was created, in ISO 8601 format.
%%  
-define(FILE_CREATED, 'file.created').


%% Directory where the file is located. It should include the drive letter, when appropriate.
%%  
-define(FILE_DIRECTORY, 'file.directory').


%% File extension, excluding the leading dot.
%%  
-define(FILE_EXTENSION, 'file.extension').


%% Name of the fork. A fork is additional data associated with a filesystem object.
%%  
-define(FILE_FORK_NAME, 'file.fork_name').


%% Primary Group ID (GID) of the file.
%%  
-define(FILE_GROUP_ID, 'file.group.id').


%% Primary group name of the file.
%%  
-define(FILE_GROUP_NAME, 'file.group.name').


%% Inode representing the file in the filesystem.
%%  
-define(FILE_INODE, 'file.inode').


%% Mode of the file in octal representation.
%%  
-define(FILE_MODE, 'file.mode').


%% Time when the file content was last modified, in ISO 8601 format.
%%  
-define(FILE_MODIFIED, 'file.modified').


%% Name of the file including the extension, without the directory.
%%  
-define(FILE_NAME, 'file.name').


%% The user ID (UID) or security identifier (SID) of the file owner.
%%  
-define(FILE_OWNER_ID, 'file.owner.id').


%% Username of the file owner.
%%  
-define(FILE_OWNER_NAME, 'file.owner.name').


%% Full path to the file, including the file name. It should include the drive letter, when appropriate.
%%  
-define(FILE_PATH, 'file.path').


%% File size in bytes.
%%  
-define(FILE_SIZE, 'file.size').


%% Path to the target of a symbolic link.
%%  
-define(FILE_SYMBOLIC_LINK_TARGET_PATH, 'file.symbolic_link.target_path').
