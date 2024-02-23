-record(view,
        {name                    :: otel_instrument:name(),
         instrument_matchspec    :: ets:compiled_match_spec(),
         description             :: unicode:unicode_binary() | undefined,
         attribute_keys          :: [opentelemetry:attribute_key()] | undefined,
         aggregation_module      :: module(),
         aggregation_options=#{} :: map()}).

-record(stream,
        {%% name of the view or instrument if the view has no name
         name :: atom(),
         scope :: opentelemetry:instrumentation_scope(),
         instrument :: otel_instrument:t(),
         reader :: reference() | undefined,

         attribute_keys :: [opentelemetry:attribute_key()] | undefined,

         aggregation_module :: module(),
         aggregation_options :: map(),

         temporality :: otel_instrument:temporality(),
         is_monotonic :: boolean(),

         %% description from the view or the instrument if the view has no name
         description :: unicode:unicode_binary() | undefined,

         %% whether to forget metrics if they aren't recorded to during a
         %% collection cycle. This is the case for Observables and Delta
         %% temporality metrics
         forget :: boolean() | undefined,

         %%
         exemplar_reservoir :: term()
        }).
