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

RollCounter =
    ?create_counter('dice.rolls',
                    #{description => <<"The number of rolls by roll value.">>,
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
?counter_add(RollCounter, 1, #{'roll.value' => Roll}),
```

The recording macros accept either the instrument handle returned at creation
or an explicitly registered, node-local atom alias. An alias is useful when the
instrument is created in one process but recorded from unrelated processes or
modules:

```erlang
-define(ROLL_COUNTER, roll_counter).

RollCounter = ?create_counter('dice.rolls', #{}),
{ok, RollCounter} = ?register_instrument(?ROLL_COUNTER, RollCounter),

%% This can run in another process. The alias resolves directly to the exact
%% instrument and does not look up that process's current Meter.
?counter_add(?ROLL_COUNTER, 1, #{'roll.value' => Roll}).
```

Aliases are explicit application-level names, not OpenTelemetry instrument
names. Registering the same alias for a different instrument returns an
`alias_conflict` error. Re-registering it for the same instrument identity is
idempotent and refreshes the handle after a Meter Provider restart. An alias
can be removed with `?unregister_instrument(Alias)`.

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
RequestCounter =
    ?create_counter('app.requests',
                    #{description => <<"Count of number of requests">>})
```

The returned handle can be passed directly when recording:

```erlang
?counter_add(RequestCounter, 5, #{<<"a">> => <<"b">>}),
```

Alternatively, associate the handle with an atom once and use the atom from
any process on the node:

```erlang
{ok, RequestCounter} = ?register_instrument(app_request_counter, RequestCounter),
?counter_add(app_request_counter, 5, #{<<"a">> => <<"b">>}).
```

The `?register_counter/3`, `?register_updown_counter/3`, and
`?register_histogram/3` macros combine creation and alias registration when the
handle does not otherwise need to be retained.

For synchronous instruments the available macros are:

- `?counter_add(InstrumentOrAlias, Number, Attributes)`
- `?updown_counter_add(InstrumentOrAlias, Number, Attributes)`
- `?histogram_record(InstrumentOrAlias, Number, Attributes)`

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
                           [{ProcessGauge, [{ProcessCount, #{}}]},
                            {AtomGauge, [{AtomCount, #{}}]}]
                   end, [])
```

The callbacks are run when the Metric Reader collects metrics for export. See
the Experimental SDK's `README.md` for more details on Metric Readers and their
configuration.
