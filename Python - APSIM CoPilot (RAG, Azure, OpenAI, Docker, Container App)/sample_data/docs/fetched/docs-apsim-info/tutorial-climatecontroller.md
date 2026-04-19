# ClimateController documentation

Source URL: https://docs.apsim.info/tutorial/ClimateController
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:53:45.996734+00:00

1 ClimateController Tutorial
The purpose of the “ClimateController” Manager component is to modify the daily weather supplied to the simulation before any other components use the data.  It can be used, for example:
to approximate climate change by adding a few degrees of temperature to MinT and MaxT;
to 'correct' a weather file if the rainfall or temerature is known to be not quite appropriate for a location;
create a heat stress event by modifying temperatures for a short defined period of time;
create a drought by setting Rain to zero for a period of time.
Of course many other possibilities exist.  See the testing below for various ways of interacting with the ClimateController component.
General parameters
:
AllowControl
, “Enable?” – enables or not the whole component
EnableDate
, “Start the climate controls beginning on date (dd/mmm/yyyy): ” – if this date is greater than the simulation start date then the climate controls will only be applied on and after this date.  This date can appear part way through a within-year control window (see below).
WithinYearControl
, “Implement climate control only during part of the year?")] ” – if the controls are to be applied during only part of a year (e.g. warming up the winter months) then set this to Yes and then set appropriate dates for the next two parameters
ControlStart
, “Within each year, the first day to start the climate controls is (dd-mmm)")] Within each year, end last day of climate control is (dd-mmm)” – the date without a year (e.g. 15-mar) that is the first day that the climate controls will start if WithinYearControl is set to Yes
ControlEnd
, “” – the date without a year (e.g. 30-apr) that is the last day of climate controls if WithinYearControl is set to Yes
Weather parameters:
RainfallMultiplier
, “Rainfall multiplier (-)” – the value by which to multiply the value of rain from the weather file, set <1 to decrease rainfall, =1 to have no effect, and >1 to increase rainfall
RainfallAddition
, “Rainfall addition (mm)” – the value to add to the value of rain from the weather file, set <0 to decrease rainfall, =0 to have no effect, and >0 to increase rainfall
And as above but for other weather variables:
RadiationMultiplier
RadiationAddition
MinTMultiplier
MinTAddition
MaxTMultiplier
MaxTAddition
WindMultiplier
WindAddition
VPMultiplier
VPAddition
Limitations:
the component makes no checks at all about the validity of the changes (e.g. Radiation can be set bit be negative)
there is currently no year-on-year change allowed (e.g. temperatures increasing by an increasing percentage each year compared to the base weather file)
it should be possible to have more than one ClimateController component in the simulation to create more complex patterns but this has not been tested
2 TestClimateControlSettings
This series of simulations tests that:
That disabling or enabling the whole component works (CompletelyOff)
Enabling the start of any climate control works (EnableFrom01Jan, EnableFrom05Jan, EnableFrom15Jan)
That the within-year window of control works (WithinYearControl)
That the start of any climate control works properly with a winthin-year window (WithinYearControlAndEnableDate)
Experiment Name
Design (Number of Treatments)
ClimateControlTestSettings
ClimateScenarios (6)
3 TestClimateControlValues
This series of simulations tests that the mulitpliers and additions to the various climate elements have been properly enabled.  There are four combinations tested:
The unmodified weather file (NoControl)
Multiply by 0 and add 0 so that the parameter value will be 0 (Mult0Add0)
Multiply by 0 and add 20 so that the parameter value will be 20 (Mult0Add20)
Multiply by 0.5 and add 0 so that the parameter value will be half that in the weather file (Mult0Add20)
Experiment Name
Design (Number of Treatments)
ClimateControlTestValues
ClimateScenarios (4)
4 PracticalTest
This test has two simulations.  The first is DalbyWheat with unmodified rainfall and in the second the rainfall is doubled.
