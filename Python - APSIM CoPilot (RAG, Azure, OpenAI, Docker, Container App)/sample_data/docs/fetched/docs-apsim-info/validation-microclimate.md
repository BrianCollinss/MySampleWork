# MicroClimate documentation

Source URL: https://docs.apsim.info/validation/MicroClimate
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:02.434626+00:00

1 The APSIM MicroClimate Model
APSIM MicroClimate Model
The module MICROMET, described here, has been developed to allow the calculation of
potential transpiration for multiple competing canopies that can be either layered or intermingled.
2 WageningenStripCrop
Experiment Name
Design (Number of Treatments)
WUR2013
Treatment (4)
WUR2014
Treatment (5)
3 Interface
3.1 MicroClimate
Parameters (Inputs)
Name
Description
Units
Type
Value
a_interception
mm/mm
double
0
b_interception
-
double
1
c_interception
mm
double
0
d_interception
mm
double
0
SoilHeatFluxFraction
MJ/MJ
double
0.4
MinimumHeightDiffForNewLayer
m
double
0
NightInterceptionFraction
0-1
double
0.5
ReferenceHeight
m
double
2
ResourceName
String
MicroClimate
Properties (Outputs)
Name
Description
Units
Type
Settable?
Structure
IStructure
True
CanopyHeight
mm
double
False
SurfaceRS
MJ/m2
double
False
PrecipitationInterception
mm
double
False
RadiationInterception
MJ/m^2
double
False
RadiationInterceptionOnGreen
MJ/m^2
double
False
PetTotal
MJ/m^2
double
False
PetRadiationTerm
mm
double
False
PetAerodynamicTerm
mm
double
False
DryLeafTimeFraction
-
double
False
NetRadiation
MJ/m^2
double
False
NetShortWaveRadiation
MJ/m^2
double
False
NetLongWaveRadiation
MJ/m^2
double
False
SoilHeatFlux
MJ/m^2
double
False
CanopyCover
-
double
False
NumLayers
int32
False
Links (Dependencies)
Name
Type
IsOptional?
clock
IClock
False
weather
IWeather
False
soilWater
ISoilWater
False
eoCalculator
ICalculateEo
False
4 Science Documentation
View science documentation here
