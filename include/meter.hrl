%% macros for meters
%% register a meter for an application with opentelemetry:register_application_meter(AppName)

-define(current_meter, opentelemetry:get_meter(?MODULE)).

-define(new_instruments(List),
        ot_meter:new_instruments(?current_meter, List)).

-define(counter_add(BoundCounter, Number),
        ot_counter:add(BoundCounter, Number)).

-define(counter_add(Name, Number, LabelSet),
        ot_counter:add(?current_meter, Name, Number, LabelSet)).

-define(measure_record(BoundMeasure, Number),
        ot_measure:record(BoundMeasure, Number)).

-define(measure_record(Name, Number, LabelSet),
        ot_measure:record(?current_meter, Name, Number, LabelSet)).

-define(bind(Name, LabelSet),
        ot_meter:bind(?current_meter, Name, LabelSet)).

-define(release(BoundInstrument),
        ot_meter:release(?current_meter, BoundInstrument)).

-define(record(Name, Number, LabelSet),
        ot_meter:record(?current_meter, Name, Number, LabelSet)).

-define(record(BoundInstrument, Number),
        ot_meter:record(BoundInstrument, Number)).

-define(record_batch(LabelSet, Measurements),
        ot_meter:record_batch(?current_meter, LabelSet, Measurements)).
