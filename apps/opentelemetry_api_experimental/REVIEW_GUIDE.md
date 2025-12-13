Review Guide (Metrics API)
==========================

Audience: Validate instrumentation behavior using macros, without reading Erlang.

Prerequisites
-------------
- Ensure `opentelemetry_experimental` (SDK) is included and configured with a console reader (see SDK REVIEW_GUIDE/README).
- Include the API header in your module: `-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").`

1) Create instruments
---------------------
Goal: Instruments can be created with name, unit, and description.

Example (Counter):
```erlang
?create_counter(app_counter, #{description => <<"Requests">>, unit => '1'}).
```

Expect: No error on creation; SDK will create streams on first record.

2) Record synchronous measurements
----------------------------------
Goal: Recording calls forward context, meter, and attributes to SDK.

Counter:
```erlang
?counter_add(app_counter, 5, #{host => <<"a">>}).
```

UpDownCounter and Histogram similarly:
```erlang
?updown_counter_add(app_load, -2, #{}).
?histogram_record(latency_ms, 37, #{path => <<"/ping">>}).
```

Expect (with console reader): Sum/Histogram points printed periodically.

3) Observable instruments
-------------------------
Goal: Callback-based measurements appear only on collection.

Single instrument with callback at creation:
```erlang
?create_observable_gauge(proc_count,
                         fun(_) -> [{erlang:system_info(process_count), #{}}] end,
                         [],
                         #{description => <<"Proc count">>, unit => '1'}).
```

Multiple instruments with one callback:
```erlang
G1 = ?create_observable_gauge(proc_count, #{}),
G2 = ?create_observable_gauge(atom_count, #{}),
?register_callback([G1, G2],
                   fun(_) ->
                       [{proc_count, [{erlang:system_info(process_count), #{}}]},
                        {atom_count, [{erlang:system_info(atom_count), #{}}]}]
                   end, []).
```

Expect: Values printed each collection tick; nothing in between.

4) Attribute filtering via SDK Views
------------------------------------
Goal: Attributes are forwarded intact by API; filtering happens in SDK.
- Configure a View in SDK with `attribute_keys` limited to `[host]`.
- Record with extra attributes; expect only `host` to remain.

5) Negative value handling
--------------------------
Goal: API forwards values; SDK enforces monotonicity.
- Add negative values to a Counter; the SDK will discard (see SDK REVIEW_GUIDE step 1).

Pointers
--------
- Architecture: `ARCHITECTURE.md`
- Spec crosswalk: `SPEC_COMPLIANCE.md`
- SDK validation: `../opentelemetry_experimental/REVIEW_GUIDE.md`


