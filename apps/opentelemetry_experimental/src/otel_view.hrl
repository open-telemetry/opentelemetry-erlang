
-record(selection,
        {instrument_name = '_' :: otel_instrument:name() | '_',
         instrument_kind = '_' :: otel_instrument:kind() | '_',
         meter_name = '_'      :: otel_meter:name() | '_',
         meter_version = '_'   :: otel_meter:version() | '_',
         schema_url = '_'      :: otel_meter:schema_url() | '_'}).

-record(view,
        {name             :: atom() | unicode:unicode_binary() | undefined,
         selection        :: #selection{},
         %% instrument_kind  :: otel_instrument:kind(),
         %% instrument_name  :: otel_instrument:name(),
         %% meter_name       :: otel_meter:name(),
         %% meter_version    :: otel_meter:version(),
         %% meter_schema_url :: otel_meter:schema_url(),
         description      :: unicode:unicode_binary() | undefined,
         attribute_keys   :: [opentelemetry:attribute_key()] | undefined,
         aggregation_module      :: module() | undefined,
         number=0 :: number()}).

-record(view_aggregation,
        {%% name of the view or instrument if the view has no name
         name ::  atom() | unicode:unicode_binary(),
         view :: #view{},
         instrument :: otel_instrument:t(),

         %% description from the view or the instrument if the view has no name
         description :: unicode:unicode_binary() | undefined,

         %% map the attributes of an instrument to its aggregation
         attributes_aggregation :: #{opentelemetry:attributes_map() => otel_aggregation:t()}
        }).
