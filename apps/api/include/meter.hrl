%% macros for meters
%% register a meter for an application with opentelemetry:register_application_meter(AppName)

-define(ot_current_meter, opentelemetry:get_meter(?MODULE)).

-define(ot_new_counter(Meter, Name, Opts),
        ot_counter:new(?ot_current_meter, Name, Opts)).

-define(ot_new_updown_counter(Meter, Name, Opts),
        ot_updown_counter:new(?ot_current_meter, Name, Opts)).

-define(ot_new_value_recorder(Meter, Name, Opts),
        ot_value_recorder:new(?ot_current_meter, Name, Opts)).

-define(ot_new_sum_observer(Meter, Name, Opts),
        ot_sum_observer:new(?ot_current_meter, Name, Opts)).

-define(ot_new_updown_observer(Meter, Name, Opts),
        ot_updown_observer:new(?ot_current_meter, Name, Opts)).

-define(ot_new_value_observer(Meter, Name, Opts),
        ot_value_observer:new(?ot_current_meter, Name, Opts)).

-define(ot_new_instruments(List),
        ot_meter:new_instruments(?ot_current_meter, List)).

-define(ot_counter_add(BoundCounter, Number),
        ot_counter:add(BoundCounter, Number)).

-define(ot_counter_add(Name, Number, LabelSet),
        ot_counter:add(?ot_current_meter, Name, Number, LabelSet)).

-define(ot_measure_record(BoundMeasure, Number),
        ot_measure:record(BoundMeasure, Number)).

-define(ot_measure_record(Name, Number, LabelSet),
        ot_measure:record(?ot_current_meter, Name, Number, LabelSet)).

-define(ot_bind(Name, LabelSet),
        ot_meter:bind(?ot_current_meter, Name, LabelSet)).

-define(ot_release(BoundInstrument),
        ot_meter:release(?ot_current_meter, BoundInstrument)).

-define(ot_record(Name, Number, LabelSet),
        ot_meter:record(?ot_current_meter, Name, Number, LabelSet)).

-define(ot_record(BoundInstrument, Number),
        ot_meter:record(BoundInstrument, Number)).

-define(ot_record_batch(LabelSet, Measurements),
        ot_meter:record_batch(?ot_current_meter, LabelSet, Measurements)).
