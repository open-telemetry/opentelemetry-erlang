%% The schema url for telemetry resources
-define(EVENT_SCHEMA_URL, <<"https://opentelemetry.io/schemas/1.25.0">>).

%% The unique identifier of the feature flag
-define(FEATURE_FLAG_KEY, 'feature_flag.key').

%% The name of the service provider that performs the flag evaluation
-define(FEATURE_FLAG_PROVIDER_NAME, 'feature_flag.provider_name').

%% SHOULD be a semantic identifier for a value. If one is unavailable, a stringified version of the value can be used
%% A semantic identifier, commonly referred to as a variant, provides a means
%% for referring to a value without including the value itself. This can
%% provide additional context for understanding the meaning behind a value.
%% For example, the variant `red` maybe be used for the value `#c05543`.
%% 
%% A stringified version of the value can be used in situations where a
%% semantic identifier is unavailable. String representation of the value
%% should be determined by the implementer
-define(FEATURE_FLAG_VARIANT, 'feature_flag.variant').

%% This attribute represents the state the application has transitioned into at the occurrence of the event
%% The iOS lifecycle states are defined in the [UIApplicationDelegate documentation](https://developer.apple.com/documentation/uikit/uiapplicationdelegate#1656902), and from which the `OS terminology` column values are derived
-define(IOS_STATE, 'ios.state').

%% This attribute represents the state the application has transitioned into at the occurrence of the event
%% The Android lifecycle states are defined in [Activity lifecycle callbacks](https://developer.android.com/guide/components/activities/activity-lifecycle#lc), and from which the `OS identifiers` are derived
-define(ANDROID_STATE, 'android.state').

%% Compressed size of the message in bytes
-define(MESSAGE_COMPRESSED_SIZE, 'message.compressed_size').

%% MUST be calculated as two different counters starting from `1` one for sent messages and one for received message
%% This way we guarantee that the values will be consistent between different implementations
-define(MESSAGE_ID, 'message.id').

%% Whether this is a received or sent message
-define(MESSAGE_TYPE, 'message.type').

%% Uncompressed size of the message in bytes
-define(MESSAGE_UNCOMPRESSED_SIZE, 'message.uncompressed_size').

%% SHOULD be set to true if the exception event is recorded at a point where it is known that the exception is escaping the scope of the span
%% An exception is considered to have escaped (or left) the scope of a span,
%% if that span is ended while the exception is still logically "in flight".
%% This may be actually "in flight" in some languages (e.g. if the exception
%% is passed to a Context manager's `__exit__` method in Python) but will
%% usually be caught at the point of recording the exception in most languages.
%% 
%% It is usually not possible to determine at the point where an exception is thrown
%% whether it will escape the scope of a span.
%% However, it is trivial to know that an exception
%% will escape, if one checks for an active exception just before ending the span,
%% as done in the [example for recording span exceptions](#recording-an-exception).
%% 
%% It follows that an exception may still escape the scope of the span
%% even if the `exception.escaped` attribute was not set or set to false,
%% since the event might have been recorded at a time where it was not
%% clear whether the exception will escape
-define(EXCEPTION_ESCAPED, 'exception.escaped').

%% The exception message
-define(EXCEPTION_MESSAGE, 'exception.message').

%% A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG
-define(EXCEPTION_STACKTRACE, 'exception.stacktrace').

%% The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it
-define(EXCEPTION_TYPE, 'exception.type').
