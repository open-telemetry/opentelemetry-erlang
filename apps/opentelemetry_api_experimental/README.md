opentelemetry_api_experimental
=====

Signals still in experimental status in the Erlang/Elixir API.

# Metrics

For configuration of the Experimental SDK for Metrics see the experimental SDK
`opentelemtry_experimental`. Without the SDK all operations for instruments and
recording to them will be no-ops and nothing will be created or exported.

## Quickstart

The metrics API is used for instrumenting application code through the creation
of instruments and calls to record data points with them.

### Instrument Creation

#### Supported Instruments

There are 3 synchronous instrument types and 3 observable instruments that are
tied to a callback function. The list below gives each instrument type supported
and details on the default aggregation used for the measurements recorded for
that instrument:

- Counter: An always increasing value. 
- Updown Counter: A non-monotonic counter.
- Histogram: A histogram with explicit bucket boundaries.
- Observable Counter: A counter that is tied to a callback for recording measurements.
- Observable Updown Counter: An updown counter that is tied to a callback for recording measurements.
- Observable Gauge: An instrument tied to a callback who's default aggregated
  value is the last value recorded by the last call of the callback.

#### Macros

The header `otel_meter.hrl` contains macros for working with Instruments. This
includes creation in the form `create_<instrument type>` and recording
measurements like `counter_add` and `histogram_record`.

Below is example creation found in the [`dice_roll_elli`
example](https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/examples/roll_dice_elli):

```erlang
-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

?create_counter(?ROLL_COUNTER, #{description => <<"The number of rolls by roll value.">>,
                                 unit => '1'}).
```
### Instrument Recording

Measurements are taken on an instrument through recordings. Each type of
instrument has functions and macros specific to its type for recording
measurements. Counters can be added to so have the `counter_add` macro, UpDown
counters can also be added to (but with the addition of accepting negative
numbers) so have `updown_counter_add` and Histograms are passed recordings so
have the macro `histogram_record`.


An example from [`dice_roll_elli`
example](https://github.com/open-telemetry/opentelemetry-erlang-contrib/tree/main/examples/roll_dice_elli)
of recording an addition to a counter with an attribute:

```erlang
?counter_add(?ROLL_COUNTER, 1, #{'roll.value' => Roll}),
```

See the Experimental SDK's `README.md` for how to setup Views for aggregation
and then the exporting of metrics.

## Details

### Meter Provider

The Meter Provider (here the default implementation is in the module
`otel_meter_server` in the SDK) is responsible for creating Meters and stores
their shared configuration along with the shared
[Resource](https://opentelemetry.io/docs/concepts/resources/) of the telemetry
created for those Meters. Including the SDK application ensures a default
Provider is created and used during Meter creation.

### Meter

Meters (default implementation found in `otel_meter_default` in the SDK) are
used to create Instruments. Direct interaction with a Meter is not required
except for special cases where the provided macros aren't enough to get the job
done. The majority of use is done behind the macros.

### Measurement

Measurements are individual data points and their associated attributes.

### Instrument

An instrument is used to capture measurements. 

Supported instrument kinds:

- Counter: Defaults to using a monotonic sum aggregation type in the SDK.
- Updown Counter: Defaults to a non-monotonic sum aggregation type in the SDK.
- Histogram: Defaults to an explicit histogram aggregation type in the SDK.
- Observable Counter: Defaults to using a monotonic sum aggregation type in the SDK.
- Observable Updown counter: Defaults to a non-monotonic sum aggregation type in the SDK.
- Observable Gauge: Defaults to a last value aggregation type in the SDK.

The first 3 are synchronous instruments while the latter 3 must be associated
with a callback function.

To record measurements an instrument must first be created. Each instrument kind
has a `?create_<kind>` macro in Erlang for creation:
 
```erlang
_RequestCounter = ?create_counter(app_request_counter, #{description => ~"Count of number of requests"})
```

Now the instrument can be used to record measurements either by passing the
atom name `app_request_counter`:

```erlang
?counter_add(app_request_counter, 5, #{<<"a">> => <<"b">>}),
```

For synchronous instruments the available macros are:

- `?counter_add(Name, Number, Attributes)`
- `?updown_counter_add(Name, Number, Attributes)`
- `?histogram_record(Name, Number, Attributes)`

The asynchronous (observable) instruments can be created with their callback or
be later registered with a callback that supports multiple instruments.

When created with a callback for the instrument the callback returns a list of
values and attributes to record for that instrument:

```erlang
?create_observable_counter(my_observable_counter,
                           fun(_Args) ->
                                   [{4, #{a => <<"b">>}},
                                    {12, #{c => <<"d">>}]
                           end,
                           [],
                           #{description => <<"Describe your instrument">>,
                             unit => kb})
```

When the measurements are taken from the same source it is more efficient to
create a single callback for multiple instruments:

```erlang
ProcessCountName = 'beam_processes',
AtomCountName = 'beam_atoms',

ProcessGauge = ?create_observable_gauge(ProcessCountName, #{description => <<"Number of currently running processes">>, unit => '1'}),
AtomGauge = ?create_observable_gauge(AtomCountName, #{description => <<"Number of created atoms">>, unit => '1'}),

?register_callback([ProcessGauge, AtomGauge],
                   fun(_) ->
                           ProcessCount = erlang:system_info(process_count),
                           AtomCount = erlang:system_info(atom_count),
                           [{ProcessCountName, [{ProcessCount, #{}}]},
                            {AtomCountName, [{AtomCount, #{}}]}]
                   end, [])
```

The callbacks are run when the Metric Reader collects metrics for export. See
the Experimental SDK's `README.md` for more details on Metric Readers and their
configuration.

## For non-Erlang reviewers

- **What to read first**: See `SPEC_COMPLIANCE.md` in this folder for a plain-language crosswalk of the Metrics API to the OpenTelemetry spec and where behaviors are implemented.
- **How to validate quickly**:
  - Ensure the experimental SDK app `opentelemetry_experimental` is included and configured with a console reader (see that app's README).
  - Use the macros in `include/otel_meter.hrl` to create an instrument and record a value.
  - Expect to see aggregated metric points printed by the SDK's console exporter.
- **Where defaults live**: Aggregation and temporality are SDK concerns; see `../opentelemetry_experimental/SPEC_COMPLIANCE.md` for defaults and reader behavior.

