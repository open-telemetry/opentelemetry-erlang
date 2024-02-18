-record(exemplar, {value :: number(),
                   time_unix_nano :: integer(),
                   filtered_attributes :: opentelemetry:attributes_map(),
                   span_id :: opentelemetry:span_id() | undefined,
                   trace_id :: opentelemetry:trace_id() | undefined
                  }).
