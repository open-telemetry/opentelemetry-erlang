%% macros for meters
%% register a meter for an application with opentelemetry:register_application_meter(AppName)

-define(otel_current_meter, opentelemetry:get_meter(?MODULE)).

-define(otel_new_counter(Meter, Name, Opts),
        otel_counter:new(?otel_current_meter, Name, Opts)).

-define(otel_new_updown_counter(Meter, Name, Opts),
        otel_updown_counter:new(?otel_current_meter, Name, Opts)).

-define(otel_new_value_recorder(Meter, Name, Opts),
        otel_value_recorder:new(?otel_current_meter, Name, Opts)).

-define(otel_new_sum_observer(Meter, Name, Opts),
        otel_sum_observer:new(?otel_current_meter, Name, Opts)).

-define(otel_new_updown_observer(Meter, Name, Opts),
        otel_updown_observer:new(?otel_current_meter, Name, Opts)).

-define(otel_new_value_observer(Meter, Name, Opts),
        otel_value_observer:new(?otel_current_meter, Name, Opts)).

-define(otel_new_instruments(List),
        otel_meter:new_instruments(?otel_current_meter, List)).

-define(otel_counter_add(BoundCounter, Number),
        otel_counter:add(BoundCounter, Number)).

-define(otel_counter_add(Name, Number, LabelSet),
        otel_counter:add(?otel_current_meter, Name, Number, LabelSet)).

-define(otel_measure_record(BoundMeasure, Number),
        otel_measure:record(BoundMeasure, Number)).

-define(otel_measure_record(Name, Number, LabelSet),
        otel_measure:record(?otel_current_meter, Name, Number, LabelSet)).

-define(otel_bind(Name, LabelSet),
        otel_meter:bind(?otel_current_meter, Name, LabelSet)).

-define(otel_release(BoundInstrument),
        otel_meter:release(?otel_current_meter, BoundInstrument)).

-define(otel_record(Name, Number, LabelSet),
        otel_meter:record(?otel_current_meter, Name, Number, LabelSet)).

-define(otel_record(BoundInstrument, Number),
        otel_meter:record(BoundInstrument, Number)).

-define(otel_record_batch(LabelSet, Measurements),
        otel_meter:record_batch(?otel_current_meter, LabelSet, Measurements)).
