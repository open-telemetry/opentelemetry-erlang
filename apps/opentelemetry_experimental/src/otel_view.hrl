
-record(selection,
        {instrument_name = '_' :: otel_instrument:name() | '_',
         instrument_kind = '_' :: otel_instrument:kind() | '_',
         meter_name = '_'      :: otel_meter:name() | '_',
         meter_version = '_'   :: otel_meter:version() | '_',
         schema_url = '_'      :: otel_meter:schema_url() | '_'}).

-record(view,
        {name             :: atom() | unicode:unicode_binary() | undefined,
         selection        :: #selection{} | undefined,
         %% instrument_kind  :: otel_instrument:kind(),
         %% instrument_name  :: otel_instrument:name(),
         %% meter_name       :: otel_meter:name(),
         %% meter_version    :: otel_meter:version(),
         %% meter_schema_url :: otel_meter:schema_url(),
         description      :: unicode:unicode_binary() | undefined,
         attribute_keys   :: [opentelemetry:attribute_key()] | undefined,
         aggregation_module      :: module() | undefined,
         aggregation_options :: term(),
         number=0 :: number()}).

-record(view_aggregation,
        {%% name of the view or instrument if the view has no name
         name ::  atom() | unicode:unicode_binary(),
         scope :: opentelemetry:instrumentation_scope(),
         instrument :: otel_instrument:t(),

         aggregation_module :: module(),
         aggregation_options :: term(),

         temporality :: otel_aggregation:temporality(),
         is_monotonic :: boolean(),

         %% description from the view or the instrument if the view has no name
         description :: unicode:unicode_binary() | undefined
        }).
