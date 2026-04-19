# SoilWater documentation

Source URL: https://docs.apsim.info/validation/SoilWater
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:54.353252+00:00

1 The APSIM SoilWater Model
The SoilWater module is a cascading water balance model that owes much to its precursors in
CERES (Jones and Kiniry, 1986) and PERFECT(Littleboy et al, 1992).
The algorithms for redistribution of water throughout the soil profile have been inherited from
the CERES family of models.
The water characteristics of the soil are specified in terms of the lower limit (ll15),
drained upper limit(dul) and saturated(sat) volumetric water contents. Water movement is
described using separate algorithms for saturated or unsaturated flow. It is notable that
redistribution of solutes, such as nitrate  and urea N, is carried out in this module.
Modifications adopted from PERFECT include:
the effects of surface residues and crop cover on modifying runoff and reducing potential soil evaporation,
small rainfall events are lost as first stage evaporation rather than by the slower process of second stage evaporation, and
specification of the second stage evaporation coefficient(cona) as an input parameter, providing more flexibility for describing differences in long term soil drying due to soil texture and environmental effects.
The module is interfaced with SurfaceOrganicMatter and crop modules so that simulation of the soil water balance
responds to change in the status of surface residues and crop cover(via tillage, decomposition and crop growth).
Enhancements beyond CERES and PERFECT include:
the specification of swcon for each layer, being the proportion of soil water above dul that drains in one day
isolation from the code of the coefficients determining diffusivity as a function of soil water
(used in calculating unsaturated flow).Choice of diffusivity coefficients more appropriate for soil type have been found to improve model performance.
unsaturated flow is permitted to move water between adjacent soil layers until some nominated gradient in
soil water content is achieved, thereby accounting for the effect of gravity on the fully drained soil water profile.
SoilWater is called by APSIM on a daily basis, and typical of such models, the various processes are calculated consecutively.
This contrasts with models such as SWIM that solve simultaneously a set of differential equations that describe the flow processes.
2 Evaporation
Experiment Name
Design (Number of Treatments)
ABlock_Residues
Cover (4)
This experiment was conducted in Plant and Food Researches A Block field near Lincoln, New Zealand.  A uniformly managed, unirrigated barley crop was grown and harvested prior to the estastablishment of this experiment.  A randomised complete block was layed out with 5 x 15 m plots laid out to avoid harvester wheel tracks with 4 replicates of 4 treatments:
StandingStubble
where the stubble that was left by the combine harvester remained standing and any residue that
HarvesterWindrow
where the residue mown from the other two treatments was piled ontop of the standing stubble to simulate a harvester windrow with the residue from approximately 2 times the area applied
MownStubble
where stubble was mown at 5cm height and removed from the plots
BareSoil
where stubble was mown and removed and then plots were ploughed and cultivated to leave a bare soil.
Following the establishment of treatments CS616 soil moisture sensors were installed in each plot at 0-20, 20-40 and 40-60 cm depth and logged at 1 hourly intervals from establishment in Autumn through until the following spring.
2.1 Wellcamp
This dataset was developed as part of work by
Huth et al., 2008
.  Evaporation from a Black Vertosol over a period of 6 months was measured using a Neutron Moisture Meter at Wellcamp, near Toowoomba in Queensland, Australia.  Rainfall was excluded from the soil.  Drainage was unlikely during the study period and so all changes in water content should be due to evaporation alone.
Experiment Name
Design (Number of Treatments)
WellCamp
Surface (1)
2.2 TorStreet
Evaporation from a Grey Vertosol over a period of 7 months was measured using weighing lysimeters in Toowoomba, Queensland, Australia.
Experiment Name
Design (Number of Treatments)
TorStreet
Surface (2)
2.3 Norwin
This dataset was developed as part of work by
Huth et al., 2008
.  Evaporation from a Black Vertosol over a period of 6 months was measured using a Neutron Moisture Meter at Wellcamp, near Toowoomba in Queensland, Australia.  Rainfall was excluded from the soil.  Drainage was unlikely during the study period and so all changes in water content should be due to evaporation alone.
Experiment Name
Design (Number of Treatments)
Norwin
Surface (1)
2.4 Gatton
Evaporation from a Grey Vertosol over a period of 7 months was measured using weighing lysimeters in Toowoomba, Queensland, Australia.
Experiment Name
Design (Number of Treatments)
Gatton
Surface (3)
2.5 BondAndWillis
This dataset reproduces the evaporation experiment by
Bond et al., 1970
in which microlysimeters were used to measure evaporation under varying residue levels and evaporative demands within controled environments.  Note that evaporation parameters may be higher than under natural systems as evaporative conditions were maintained for 24 hours per day.  The soil type was a fine sandy loam and the residue was clean bright wheat straw cut into 1.3 cm lengths.
Experiment Name
Design (Number of Treatments)
BondAndWillis
Residue (7)
3 Drainage
3.1 Libardi
Data used here were collected during an internal drainage experiment
Libardi et al., 2001
, carried out on a sandyâ€“loam Red Yellow Latosol (Typic Hapludox) near Piracicaba, SP, Brazil.  The soil had a fairly homogeneous
profile down to the depth of 2 m. Soil water content values were calculated from tensiometer
readings, through the use of laboratory established soil water retention curves and of soil water potential
heads, measured with the same mercury manometer tensiometers.
Experiment Name
Design (Number of Treatments)
Libardi
Surface (1)
4 WaterExtraction
Experiment Name
Design (Number of Treatments)
Lincoln2015
Nit x Irr (6)
LandP
Irrigation x CoverType (8)
Lincoln2015 (Rain-Shelter Trail)
This dataset demonstrates the impact of three N (0, 50 and 250 kg/ha N) and two water regimes (dryland and fully irrigated) using a rain-shelter structure at Lincoln, New Zealand.  A crop of 'Discovery' wheat was sown in October and managed as a typical spring wheat crop.  Soil water content was measured with CS650 soil moisture probes positioned at 7 depths in each plot (192 probes in total) and logged at 15 min intervals for the duration of the experiment.  Total cover was measured using linear PAR sensors in each plot logged for the duration of the experiment and green cover was measured ever 3-4 days with NDVI and interpolated to daily values.
Slurp is used to represent the crop in this test so it can focus on soil water and not be blured by the performance of the crop model.  To do this observed values for green and total cover, LAI, and Height were set in Slurp each day using a manager script.
This experiment was run over 3 years in Lincoln, New Zealand on a shallow stony soil.  Treatments of Lucerne and Pasture (Ryegrass) were established in the Autumn of 2011 and irrigtion treatments were installed in the spring of 2011 and applied for three years.  Treatments were:
None where no irrigation was applied
TwoPerWeek where irrigation was applied twice per week during spring/summer/autumn to replace water use measured by neutron probe
OnePerWeek where the same amout of irrigation was applied as the above treatment but at weekly frequency (i.e fewer, larger irrigations)
ThreeWeekly where the same amount of irrigation was applied as abouve but at a 3 weekely frequency (i.e fewer, larger irrigations)
Lucerne and pasture were defoliated at differing frequiencies following best mananagment practice resulting in 8-10 regrowth period per year for pasture and 5-6 for lucerne.  Neutron probes were installed to 1.6 m depth using a 20T digger with a pnumatic plate on its boom to drive and extract a pilot rod into the stony sub soil and then an aluminum access tube was installed into the resultant hole.  Neutron probes measurements were tatken at 7 - 10 day intervals during the irrigation season.  CS616 probes were also installed in the top soil at 0-15 and 15-30 cm depths in each plot and logged hourly.
Slurp is used to represent the crop in this test so it can focus on soil water and not be blured by the performance of the crop model.  To do this observed values for green cover, LAI, and Height were set in Slurp each day using a manager script.
5 SensibilityTests
Experiment Name
Design (Number of Treatments)
ResidueEvaporationTest
Residue (6)
6 Interface
6.1 SoilWater
Parameters (Inputs)
Name
Description
Units
Type
Value
X
double
Y
double
FixedValue
double
Units
String
tillageCnCumWater
double
tillageCnRed
double
Properties (Outputs)
Name
Description
Units
Type
Settable?
Structure
IStructure
True
SummerDate
dd-mmm
String
True
SummerU
mm
double
True
SummerCona
double
True
WinterDate
dd-mmm
String
True
WinterU
mm
double
True
WinterCona
double
True
DiffusConst
mm2/day
double
True
DiffusSlope
/mm
double
True
Salb
double
True
CN2Bare
double
True
CNRed
double
True
CNCov
double
True
DischargeWidth
m
double
True
CatchmentArea
m2
double
True
PSIDul
cm
double
True
Depth
Models.Core.SummaryAttribute
mm
String
True
Thickness
double
True
Water
double
True
SW
double
True
PSI
cm
double
False
K
cm/h
double
True
PoreInteractionIndex
-
double
True
Runon
double
True
PotentialInfiltration
double
True
LateralFlow
double
False
LateralOutflow
double
False
Runoff
double
True
Infiltration
double
True
Drainage
double
False
SubsurfaceDrain
double
False
Evaporation
double
False
WaterTable
double
True
Flux
double
True
Flow
double
True
PotentialRunoff
double
False
Properties
Soil
False
SWmm
double
False
ESW
double
False
Eos
double
False
Es
double
False
T
double
False
Eo
double
True
SWCON
Models.Core.SummaryAttributeBetween (SAT and DUL) soil water conductivity constant for each soil layer.At thicknesses specified in "SoilWater" node of GUI.Use Soil.SWCON for SWCON in standard thickness
/d
double
True
KLAT
Models.Core.SummaryAttributeLateral flow soil water conductivity constant for each soil layer.At thicknesses specified in "SoilWater" node of GUI.Use Soil.KLAT for KLAT in standard thickness
mm/d
double
True
LeachNO3
double
False
LeachNH4
double
False
LeachUrea
double
False
LeachCl
double
False
PrecipitationInterception
double
True
Pond
double
False
PAW
mm/mm
double
False
PAWmm
mm
double
False
Links (Dependencies)
Name
Type
IsOptional?
soil
Soil
False
soilPhysical
IPhysical
False
water
Water
False
summary
ISummary
False
lateralFlowModel
LateralFlowModel
False
runoffModel
RunoffModel
False
saturatedFlow
SaturatedFlowModel
False
unsaturatedFlow
UnsaturatedFlowModel
False
evaporationModel
EvaporationModel
False
waterTableModel
WaterTableModel
False
Methods (callable from manager)
Name
Description
RemoveWater
void RemoveWater(double amountToRemove)
Remove water from the profile
SetWaterTable
void SetWaterTable(double InitialDepth)
Sets the water table.
Reset
void Reset()
Tillage
void Tillage(TillageType Data)
Perform tillage
Tillage
void Tillage(String tillageType)
Perform tillage
SetPhysical
void SetPhysical(Physical physical)
Set the physical node.I'm not sure why this is necessary
7 References
Bond, JJ, Willis, WO, 1970. Soil Water Evaporation: First Stage Drying as Influenced by Surface Residue and Evaporation Potential. Soil Science Society of America Proceedings 34, 924-928.
Huth, N. I., Carberry, P. S., Cocks, B., Graham, S., McGinness, H. M., O'Connell, D. A., 2008. Managing drought risk in eucalypt seedling establishment: An analysis using experiment and model. Forest Ecology and Management 255 (8-9), 3307-3317.
Libardi, PL, Reichardt, Klaus, 2001. Libardi's method refinement for soil hydraulic conductivity measurement. Australian Journal of Soil Research 39, 851-860.
