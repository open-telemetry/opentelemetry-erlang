
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

%% The database domain associated with the connection.
%%  
-define(ORACLE_DB_DOMAIN, 'oracle.db.domain').


%% The instance name associated with the connection in an Oracle Real Application Clusters environment.
%%  
-define(ORACLE_DB_INSTANCE_NAME, 'oracle.db.instance.name').


%% The database name associated with the connection.
%%  
-define(ORACLE_DB_NAME, 'oracle.db.name').


%% The pluggable database (PDB) name associated with the connection.
%%  
-define(ORACLE_DB_PDB, 'oracle.db.pdb').


%% The service name currently associated with the database connection.
%%  
-define(ORACLE_DB_SERVICE, 'oracle.db.service').
