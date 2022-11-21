-record(view,
        {name                    :: otel_instrument:name() | '_' | undefined,
         instrument_matchspec    :: ets:compiled_match_spec() | undefined,
         description             :: unicode:unicode_binary() | undefined,
         attribute_keys          :: [opentelemetry:attribute_key()] | undefined,
         aggregation_module      :: module() | undefined,
         aggregation_options=#{} :: map(),
         number=0                :: number()}).

-record(view_aggregation,
        {%% name of the view or instrument if the view has no name
         name ::  atom() | unicode:latin1_chardata(),
         scope :: opentelemetry:instrumentation_scope(),
         instrument :: otel_instrument:t(),
         reader :: pid() | undefined,

         aggregation_module :: module(),
         aggregation_options :: map(),

         temporality :: otel_aggregation:temporality(),
         is_monotonic :: boolean(),

         %% description from the view or the instrument if the view has no name
         description :: unicode:unicode_binary() | undefined
        }).
