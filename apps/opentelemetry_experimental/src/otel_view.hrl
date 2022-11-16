
-record(selection,
        {instrument_name = '_' :: otel_instrument:name() | '_' | undefined,
         instrument_kind = '_' :: otel_instrument:kind() | '_',
         meter_name = '_'      :: unicode:unicode_binary() | undefined | '_',
         meter_version = '_'   :: unicode:unicode_binary() | undefined | '_',
         schema_url = '_'      :: unicode:unicode_binary() | undefined | '_'}).

-record(view,
        {name             :: otel_instrument:name() | '_' | undefined,
         selection        :: #selection{} | undefined,
         %% instrument_kind  :: otel_instrument:kind(),
         %% instrument_name  :: otel_instrument:name(),
         %% meter_name       :: otel_meter:name(),
         %% meter_version    :: otel_meter:version(),
         %% meter_schema_url :: otel_meter:schema_url(),
         description      :: unicode:unicode_binary() | undefined,
         attribute_keys   :: [opentelemetry:attribute_key()] | undefined,
         aggregation_module      :: module() | undefined,
         aggregation_options :: map(),
         number=0 :: number()}).

-record(view_aggregation,
        {%% name of the view or instrument if the view has no name
         name ::  atom() | unicode:latin1_chardata(),
         scope :: opentelemetry:instrumentation_scope(),
         instrument :: otel_instrument:t(),

         aggregation_module :: module(),
         aggregation_options :: map(),

         temporality :: otel_aggregation:temporality(),
         is_monotonic :: boolean(),

         %% description from the view or the instrument if the view has no name
         description :: unicode:unicode_binary() | undefined
        }).
