

%% SHOULD be set to true if the exception event is recorded at a point where it is known that the exception is escaping the scope of the span.
%%  
-define(EXCEPTION_ESCAPED, 'exception.escaped').


%% The exception message.
-define(EXCEPTION_MESSAGE, 'exception.message').


%% A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG.
%%  
-define(EXCEPTION_STACKTRACE, 'exception.stacktrace').


%% The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it.
%%  
-define(EXCEPTION_TYPE, 'exception.type').
