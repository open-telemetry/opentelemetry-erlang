

%% The [event_id](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#id) uniquely identifies the event.
%%  
-define(CLOUDEVENTS_EVENTID, 'cloudevents.event_id').


%% The [source](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#source-1) identifies the context in which an event happened.
%%  
-define(CLOUDEVENTS_EVENTSOURCE, 'cloudevents.event_source').


%% The [version of the CloudEvents specification](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#specversion) which the event uses.
%%  
-define(CLOUDEVENTS_EVENTSPECVERSION, 'cloudevents.event_spec_version').


%% The [subject](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#subject) of the event in the context of the event producer (identified by source).
%%  
-define(CLOUDEVENTS_EVENTSUBJECT, 'cloudevents.event_subject').


%% The [event_type](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#type) contains a value describing the type of event related to the originating occurrence.
%%  
-define(CLOUDEVENTS_EVENTTYPE, 'cloudevents.event_type').
