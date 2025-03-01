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
tied to a callback function:

- counter
- updown counter
- histogram
- observable counter
- observable updown counter
- observable gauge

#### Macros

The header `otel_meter.hrl` contains macros for working with Instruments. This
includes creation in the form `create_<instrument type>` and recording
measurements like `counter_add` and `histogram_record`.

Below is example creation found in the [`dice_roll_elli` example]():

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


An example from [`dice_roll_elli` example]() of recording an addition to a
counter with an attribute:

```erlang
?counter_add(?ROLL_COUNTER, 1, #{'roll.value' => Roll}),
```

See the Experimental SDK's `README.md` for how to setup Views for aggregation
and then the exporting of metrics.

## Details

There are a number of pieces that work together to make Metrics work in
OpenTelemetry. There are defaults you can use for everything except the Reader
and Exporter. Without configuring a Reader (which takes an Exporter) there will
be no metrics exported from the node.

### Meter Provider

The Meter Provider (here the default implementation is in the module
`otel_meter_server`) is responsible for creating Meters and stores their shared
configuration along with the shared Resource of the telemetry created for those
Meters.

### Meter

Meters (default implementation found in `otel_meter_default`) are used to create
Instruments. Direct interaction with a Meter is not required except for special
cases where the provided macros aren't enough to get the job done. The majority
of use is done behind the macros.

### Measurement

Measurements are individual data points and their associated attributes.

### Instrument

An instrument is used to capture measurements. 

Supported instrument kinds:

- counter
- updown counter
- histogram
- observable counter
- observable updown counter
- observable gauge

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

- `?counter_add`
- `?updown_counter_add`
- `?histogram_record`

The asynchronous (observable) instruments 
