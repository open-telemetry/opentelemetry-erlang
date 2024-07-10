

%% The basename of the file.
%%  
-define(LOG_FILE_NAME, 'log.file.name').


%% The basename of the file, with symlinks resolved.
%%  
-define(LOG_FILE_NAMERESOLVED, 'log.file.name_resolved').


%% The full path to the file.
%%  
-define(LOG_FILE_PATH, 'log.file.path').


%% The full path to the file, with symlinks resolved.
%%  
-define(LOG_FILE_PATHRESOLVED, 'log.file.path_resolved').


%% The stream associated with the log. See below for a list of well-known values.
%%  

-define('log_iostream.stdout', 'stdout').

-define('log_iostream.stderr', 'stderr').

-define(log_iostream(Custom), Custom).


%% A unique identifier for the Log Record.
%%  
-define(LOG_RECORD_UID, 'log.record.uid').
