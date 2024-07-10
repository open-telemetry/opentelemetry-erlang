

%% The unique identifier of the feature flag.
-define(FEATUREFLAG_KEY, 'feature_flag.key').


%% The name of the service provider that performs the flag evaluation.
-define(FEATUREFLAG_PROVIDERNAME, 'feature_flag.provider_name').


%% SHOULD be a semantic identifier for a value. If one is unavailable, a stringified version of the value can be used.
%%  
-define(FEATUREFLAG_VARIANT, 'feature_flag.variant').
