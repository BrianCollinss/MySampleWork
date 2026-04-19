# STRUM documentation

Source URL: https://docs.apsim.info/validation/STRUM
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:53:21.246314+00:00

1 The APSIM STRUM Model
The STRUM model
A simple representation of the shape and mass of rows of fruit trees which can be used in a multizone simulation with a row of mown grass to model the water N and C balance of an orchard system.
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
STRUM: the Simple Tree Resource Uptake Model
Hamish Brown, Edith Khaembah, Rogerio Cichota, Xiumei Yang and Jo Sharp, New Zealand Institute for Bioeconomy Science
Neil Huth and Dean Holzworth, Commonwealth Scientific and Industrial Research Organisation
Steven Reeves, Queensland Department of the Environment, Tourism, Science and Innovation
The STRUM model is designed and built along the same lines and the SCRUM and SPRUM models.  Its primary purpose is the represent the contributions of orchard trees to the nitrogen and water balance of an orchard.  It can also be used to assess the impacts of water and nitrogen supply on tree stress and potential fruit production.  It can be used where a simple plant model is required to reduce structural uncertainty in simulations or where simulations of tree crop systems are required but no detailed tree crop model exists.
The trees phenology is represented in a very simple manner with a series of phases that represent dormancy; a period of canopy expansion; a period or full canopy and a period of leaf fall returning to the Dormant phase:  The dates that the model transitions between these phases are user inputs provide in the model configuration.  Date of flowering and subsequent fruit growth are also provided as user inputs.  As such it is easy to parameterize the timing canopy and fruit development to occur at times that are sensible for the given context.  However, the crops phenology does not respond to temperature or photoperiod so the model is not appropriate where users are wanting to simulate the effects of environment and management on tree development.
In most cases STRUM is used in a multizone simulation with the STRUM model growing in a “Row” zone along-side an “Alley” zone that may have a crop or pasture model growing in it.  A schematic representation of a typical Row/Alley structure is shown below.
Attention has been paid to modelling the interception of solar radiation by the tree row zone and the interaction of roots with other plants growing in the alley.  A modified implementation of the
Gou et al., 2017
radiation interception model has been implemented and tested in the APSIM Microclimate module to simulate the radiation of a row of tree canopies and a shorter alley understory.  Briefly, the tree canopy is treated as a cube and the proportion of diffuse radiation that would be intercepted by this cube is calculated firstly assuming it is a black body.  Then transmission through the cube is calculated based on the fractional radiation interception of the canopy (based on LAI and extinction coefficient).  Radiation that is not captured by the tree canopy is then partitioned between the Row and Alley understories.  The STRUM canopy intercepts radiation with both green leaves (which drives photosynthesis and transpiration) and trunk area (which shades the understory).  An example of a typical seasonal pattern of radiation interception is shown in the figure below.  While the tree has no green leaves it is intercepting a small proportion of the total radiation over the simulation area (Tree_FRad, as shown by the black line).  At Bud break the leaf area starts expanding and the fractional radiation interception of the green canopy increases (Canopy_FRad as shown by the green line).  Canopy_FRad remains constant from the start of full canopy until the start of leaf fall but Tree_FRad continues to increase during this time as the trees hight and width increases and it captures more of the total radiation.
The STRUM model consists of 4 organs:
Leaf
which represents leaves on a tree.
Fruit
which represents the biomass and nitrogen that may be removed from the orchard each year as harvested product
Trunk
which represents the perennial biomass of the trees and will grow a small amount each year but is mostly pruned out at the end of the year and may be returned to the soil surface.
Root
which extracts water and nitrogen from the soil for plant growth, grows a small amount of biomass each year and senesesses a proportion of this to the soil.
Using STRUM in simulations
STRUM is programmed to work with rectangular zones so users must ensure this zone type is used. It also requires the TreeRow strip crop radiation model to be selected for each zone.
While the STRUM model will run as a stand alone model, in most cases fruit trees and vines are grown in rows with a grass or herb ley growing in the alleys.  As such it is important to set up a simulation with two rectangular zones, one containing the STRUM fruit tree model and another with the understory crop or pasture.  In most cases the SPRUM model is adequate to represent the energy, water and nitrogen balance of the alleys.
STRUM has been designed to repeat through annual growth cycles over any number of years.  If STRUM is included in a simulation it will establish itself on the first day of the simulation.  The annual cycle is based around the winter solstice so it is best to start simulations including STRUM on the 21st of June (Southern Hemisphere) or 21st of December (Northern Hemisphere).
Some working examples are provided in the STRUM.apsimx file  in the released example folder.
The zone representing the tree row needs to have the STRUM model included and a STRUMTreeInstance where the parameters for the orchard layout and tree behaviour are set.  BiomassRemovalEvents models must also be included to specify the dates and amounts of biomass removed by picking and pruning.
The STRUMTreeInstance has the following parameters:
Tree Type:  The user specifies if the tree is deciduous or evergreen which influences leaf senescence and winter cover patterns
Distance between rows, Distance between trees within rows and Relative alley width: These parameters are used to set the size of the Row and Alley zones.  Note that STRUM overwrites the length and width properties on the Rectangular zone model when it is established so those properties have not effect in simulations containing STRUM.
Tree age at start of simulation:  STRUM simulates an increase in its dimensions annually until it reaches its maximum dimension.  This parameter is used to determine the size of the tree relative to its maximum on the day of initialisation.
Age of tree at maximum dimension:  The trees dimensions will no longer increase annually once this age is reached
Trunk mass when maximum dimension is reached.  Trunk is defined as all above ground material that is not leaf or fruit.  Set this as the weight that a tree would typically reach when it is at the maximum dimension that it would be pruned back to annually.
Date for Budbreak, Start of full canopy, Start of leaf fall and End of leaf fall represent typical dates for these canopy development stages for the location where the tree is being simulated.  Some experience with the tree crop being modelled is required to ensure sensible values are set here.
Grow roots into Alley zone: The trees roots will be present in the Row zone, this option specifies if they also grow into the alley zone.
Root depth when mature:  Sets the root depth that the tree will achieve at the Age of maximum dimension (see above).  If the Age of the tree at the start of the simulation is less that the age at maximum dimension the root system will start shallower that this and grow down to this depth over time.  If XF parameters are set to zero at any depth the roots will only achieve that depth and not reach the depth specified here.
Root, Leaf and Trunk biomass proportions set what proportion of the total photosynthesis is demanded by these organs each day.  Note that fruit biomass demand is determined by fruit size, number and growth pattern parameters (see below) and its demands are given a higher priority so Root, Leaf and Trunk might not get the full proportion of biomass demanded when fruit are growing.
Height of the bottom of the canopy is the height at which tree leaves are held above the ground.  This should be higher than the top of the alley crop to avoid errors in the radiation interception predictions.
Height and Width of the mature tree before pruning is the maximum hight and width that the trees reach before they are pruned each year.
Height and Width of the mature tree after pruning is the height and width the tree is pruned down to each year.  Note, the model adjusts its height and width independent of the Pruning Biomass removal event and it is assumed that pruning occurs annually on the winter solstice.
Nitrogen content of Leaf, Trunk, Root and Fruit are used to determine the N demands for the growth of each of these organs
Extinction coefficient is used to derive maximum LAI from cover.
Winter cover of the tree canopy is only relevant for evergreen trees and sets the cover that is maintained through the winter
Maximum cover sets the cover that the tree crop achieves between Start of full canopy and Start of leaf fall.  Note for this (and the winter cover) it is the fractional radiation interception of the area of the canopy.  The radiation interception model also uses the width of the tree (relative to row spacing) and the height of the canopy to determine total radiation interception by the tree canopy.
Radiation use efficiency is the amount of biomass (g) that is produced by photosynthesis per unit of radiation (MJ) intercepted by the canopy.  This is then partitioned to leaf, root, trunk and fruit.  The base, optima and maximum temperatures specify the seasonal response of photosynthesis to temperature.  Note that temperatures represent daily mean temperatures not maximums.
Maximum canopy conductance and Net radiation at 50% canopy conductance determine the trees water demand.  Typical values for trees are 0.005 and 50 respectively.
KL in the top soil layer specified the kl in the top 30 CM of soil.  Note that STRUM overwrites the KL values specified on the STRUMSoil node on the Soil.Physical Node so there is no need to pay attention to setting these values.  Instead STRUM calculates KL for each soil layer assuming an exponential decline from the top layer values to zero at the maximum root depth.
It is also possible to turn off the crops response to water stress which will stop the canopy and biomass production being affected by any water shortages.
Fruit number retained is the number of fruit that will be left on the tree after thinning.  It is specified per m^2 of canopy so the total number of fruit per tree will increase as the tree grows
Potential Fruit weight is the average fresh weight of individual fruits.
Fruit DM concentration is the dry matter concentration of fresh fruit which is used to calculate fruit dry weight from fresh weight
Fruit Density is used to calculate fruit size assuming fruit are spherical.
Date of maximum bloom is the date that flowering occurs and is when slow fruit growth starts.  An approximate sigmoidal increase in fruit mass is assumed with 1% of the final mass reached at Start Linear Growth and 90% reached on end linear growth.  Users need to have some experience with the crop to ensure appropriate dates are entered for flowering and the points of fruit growth to ensure sensible fruit growth patterns.
STRUM sets the area of Row and Alley zones so that a simulatoin is representing a single tree and the area or row and alley zone that goes with it.  The user needs to be careful when considering STRUM model outputs becaue the outputs are generally on a per tree basis.
The model is constructed from the following list of software components. Details of the implementation and model parameterisation are provided in the following sections.
1.1 Plant Model Components
Component Name
Component Type
Arbitrator
Models.PMF.BiomassArbitrator
RootUptakesArbitrator
Models.PMF.RootUptakesArbitrator
Phenology
Models.PMF.Phen.Phenology
Root
Models.PMF.Organ
Leaf
Models.PMF.Organ
Trunk
Models.PMF.Organ
Fruit
Models.PMF.Organ
AccumulatedWaterStress
Models.Functions.AccumulateFunctionGeneral
AboveGround
Models.PMF.CompositeStates
AboveGroundDead
Models.PMF.CompositeStates
BelowGround
Models.PMF.CompositeStates
Total
Models.PMF.CompositeStates
TotalLive
Models.PMF.CompositeStates
TotalDead
Models.PMF.CompositeStates
SeedMortalityRate
Models.Functions.Constant
MaxPrunedHeight
Models.Functions.Constant
MortalityRate
Models.Functions.Constant
Height
Models.Functions.AddFunction
Width
Models.Functions.AddFunction
RelativeAnnualDimension
Models.Functions.LinearInterpolationFunction
RelativeSeasonalDimension
Models.Functions.LinearInterpolationFunction
RadiationIntercepted
Models.Functions.AddFunction
RowWidth
Models.Functions.Constant
TranspirationPerTree
Models.Functions.MultiplyFunction
InterRowSpacing
Models.Functions.Constant
CanopyArea
Models.Functions.ProtectedDivideFunction
TranspirationPerLeafArea
Models.Functions.ProtectedDivideFunction
1.2 Child Components
1.2.1 Arbitrator
The Arbitrator class determines the allocation of dry matter (DM) and Nitrogen between each of the organs in the crop model. Each organ can have up to three different pools of biomass:
Structural biomass
which is essential for growth and remains within the organ once it is allocated there.
Metabolic biomass
which generally remains within an organ but is able to be re allocated when the organ senesces and may be retranslocated when demand is high relative to supply.
Storage biomass
which is partitioned to organs when supply is high relative to demand and is available for retranslocation to other organs whenever supply from uptake, fixation, or re allocation is lower than demand.
The process followed for biomass arbitration is shown in Figure [FigureNumber]. Arbitration calculations are triggered by a series of events (shown below) that are raised every day.  For these calculations, at each step the Arbitrator exchange information with each organ, so the basic computations of demand and supply are done at the organ level, using their specific parameters.
doPotentialPlantGrowth
.  When this event occurs, each organ class executes code to determine their potential growth, biomass supplies and demands.  In addition to demands for structural, non structural and metabolic biomass (DM and N) each organ may have the following biomass supplies:
Fixation supply
.  From photosynthesis (DM) or symbiotic fixation (N)
Uptake supply
.  Typically uptake of N from the soil by the roots but could also be uptake by other organs (eg foliage application of N).
Retranslocation supply
.  Storage biomass that may be moved from organs to meet demands of other organs.
Reallocation supply
. Biomass that can be moved from senescing organs to meet the demands of other organs.
doPotentialPlantPartitioning.
On this event the Arbitrator first executes the DoDMSetup() method to gather the DM supplies and demands from each organ, these values are computed at the organ level.  It then executes the DoPotentialDMAllocation() method which works out how much biomass each organ would be allocated assuming N supply is not limiting and sends these allocations to the organs.  Each organ then uses their potential DM allocation to determine their N demand (how much N is needed to produce that much DM) and the arbitrator calls DoNSetup() to gather the N supplies and demands from each organ and begin N arbitration.  Firstly DoNReallocation() is called to redistribute N that the plant has available from senescing organs.  After this step any unmet N demand is considered as plant demand for N uptake from the soil (N Uptake Demand).
doNutrientArbitration.
When this event occurs, the soil arbitrator gets the N uptake demands from each plant (where multiple plants are growing in competition) and their potential uptake from the soil and determines how much of their demand that the soil is able to provide.  This value is then passed back to each plant instance as their Nuptake and doNUptakeAllocation() is called to distribute this N between organs.
doActualPlantPartitioning.
On this event the arbitrator call DoNRetranslocation() and DoNFixation() to satisfy any unmet N demands from these sources.  Finally, DoActualDMAllocation is called where DM allocations to each organ are reduced if the N allocation is insufficient to achieve the organs minimum N concentration and final allocations are sent to organs.
Figure [FigureNumber]:
Schematic showing the procedure for arbitration of biomass partitioning.  Pink boxes represent events that occur every day and their numbering shows the order of calculations. Blue boxes represent the methods that are called when these events occur.  Orange boxes contain properties that make up the organ/arbitrator interface.  Green boxes are organ specific properties.
The partitioning of biomass to fodder beet organs follows a "Relative Allocation" routine (see Arbitrator module documentation) guided by coefficients derived from the October sowing date data of the test dataset
1.2.2 RootUptakesArbitrator
Interface between soil arbitrator and Plant model instance
All supplies, demands and uptakes passed to and from the Soil Arbitrator are in liters for water and kg for N.
This is because the Soil Arbitrator work independently of area.
This interface needs to adjust uptakes back to the appropriate units to send them back to plan.
1.2.3 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
1.2.4 Root
This is the basic organ class that contains biomass structures and transfers
1.2.5 Leaf
This is the basic organ class that contains biomass structures and transfers
1.2.6 Trunk
This is the basic organ class that contains biomass structures and transfers
1.2.7 Fruit
This is the basic organ class that contains biomass structures and transfers
1.2.8 AccumulatedWaterStress
Accumulates a child function between a start and end stage or start and end events.
Water Stress is accumulated while the tree has a canopy and reset each year at the start of canopy expansion.  This value is referenced by NextYearsFruitingBudSurvival when the tree goes dormand to determine to set the proportion of buds that will have survived this year.  That value is held over into the following season so stress effects on fruiting bud number from this season are carried over to affect fruit load for next season.
1.2.9 AboveGround
This is a composite biomass class, representing the sum of 1 or more biomass objects.
1.2.10 AboveGroundDead
This is a composite biomass class, representing the sum of 1 or more biomass objects.
1.2.11 BelowGround
This is a composite biomass class, representing the sum of 1 or more biomass objects.
1.2.12 Total
This is a composite biomass class, representing the sum of 1 or more biomass objects.
1.2.13 TotalLive
This is a composite biomass class, representing the sum of 1 or more biomass objects.
1.2.14 TotalDead
This is a composite biomass class, representing the sum of 1 or more biomass objects.
1.2.15 SeedMortalityRate
A constant function (name=value)
1.2.16 MaxPrunedHeight
A constant function (name=value)
1.2.17 MortalityRate
A constant function (name=value)
1.2.18 Height
A class that returns the sum of its child functions.
1.2.19 Width
A class that returns the sum of its child functions.
1.2.20 RelativeAnnualDimension
A linear interpolation model, where an
1.2.21 RelativeSeasonalDimension
A linear interpolation model, where an
1.2.22 RadiationIntercepted
A class that returns the sum of its child functions.
1.2.23 RowWidth
A constant function (name=value)
1.2.24 TranspirationPerTree
A class that returns the product of its child functions.  Performance note: This function returns zero as soon as any of its child functions return zero.  Therefore, speed gains can be achieved by placing children that are likely to return zero values at the top of the list of children
1.2.25 InterRowSpacing
A constant function (name=value)
1.2.26 CanopyArea
Returns special values if
the numerator is 0 or if the denominator is 0.
Currently used in sorghum/maize code to mimic divide functions
in old apsim which return 10 if the denominator is 0 or 0 if
the numerator is 0.
1.2.27 TranspirationPerLeafArea
Returns special values if
the numerator is 0 or if the denominator is 0.
Currently used in sorghum/maize code to mimic divide functions
in old apsim which return 10 if the denominator is 0 or 0 if
the numerator is 0.
2 PatternTests
Experiment Name
Design (Number of Treatments)
SingleZoneCanopySize
Factor (4)
TwoZoneCanopySize
Factor (4)
TwoZoneSizeRootsConfined
Factor (4)
TotalWidth
Factor (2)
RootInNeighoursZone
Factor (2)
SingleZoneRootSize
Factor (3)
TwoZoneInRowTreeSpacing
Factor (4)
I this test canopy size is held constant and the size of the soil zone is changed.
As Zone gets larger supply of nitrogen and water (kg) to the soil arbitrator should increase in proportion to zone size.  Demand for water and nitrogen should remain constant as they are driven by canopy size rather than zone size.  Uptakes should be the minumu of supply and demand
I this test canopy size is held constant and the size of the soil zone is changed.
As Zone gets larger supply of nitrogen and water (kg) to the soil arbitrator should increase in proportion to zone size.  Demand for water and nitrogen should remain constant as they are driven by canopy size rather than zone size.  Uptakes should be the minumu of supply and demand
I this test canopy size is held constant and the size of the soil zone is changed.
As Zone gets larger supply of nitrogen and water (kg) to the soil arbitrator should increase in proportion to zone size.  Demand for water and nitrogen should remain constant as they are driven by canopy size rather than zone size.  Uptakes should be the minumu of supply and demand
3 NZ AppleExperiments
Experiment Name
Design (Number of Treatments)
Manawatu_1994
Factor (3)
Manawatu_1997
Factor (2)
Based on Green et al 2013 - PFR report for the Hawkes Bay Regional Council
Location: Hastings, NZ
Cultivar: Pink Lady
Transpiration measured using sap flow sensors.
Based on Green et al 2013 - PFR report for the Hawkes Bay Regional Council
Location: Hastings, NZ
Cultivar: Pink Lady
Transpiration measured using sap flow sensors.
4 USA AppleExperiments
Experiment Name
Design (Number of Treatments)
Prosser_Apple
_Soil x _Irr (18)
Based on Ebel e al (2001) - HORTSCIENCE, VOL. 36(7)
Trial site: Roza irrigation district in the lower Yakima valley near Prosser, Wash. (lat. 45°N; long. 119°W and 380 m above sea level)
Soil: Warden fine sandy loam (coarse, silty, mixed, mesic, Xerollic Camborthids) that was uniform down the profile and overlaid fractured basalt bedrock
The permanent wilting and field capacity were 8% (80 mm·m–1) and 28% (280 mm·m–1) by volume, respectively - total available soil water was 200 mm·m–1 for the two orchards.
The shallow soil (mean depth = 0.8 m) and deeper soil (average depth = 1.2 m) had 160 and 240 mm of total water available at field capacity (FC), respectively.
Irrigation treatments: withholding irrigation all season or from 3, 5, 7, 9, 11, 13, 15 or 17 weeks before harvest.
Total pan evaporation was 1005 mm and precipitation was negligible from May through September.
Apples grown in Washington, e.g. Red/Golden Delicious, Gala, and Granny Smith, are in season from August to early November.
Apple trees in Washington should be sown in early spring.
5 NZ Avocado
Experiment Name
Design (Number of Treatments)
OmokaroaAvocado
Water (2)
6 NZ Kiwifruit Expts
Correct soils entered
Correct climate data entered
Approx start growth and season-end added
This is a Gold3 orchard
This is a Gold3 orchard
This is a Gold3 orchard
7 Interface
7.1 STRUM
Properties (Outputs)
Name
Description
Units
Type
Settable?
Structure
IStructure
True
AboveGroundHarvestable
IBiomass
False
SowingData
SowingParameters
True
CultivarNames
String
False
SowingDate
datetime
True
Population
/m2
double
True
IsAlive
boolean
True
IsEmerged
boolean
False
IsReadyForHarvesting
boolean
False
DaysAfterSowing
d
int32
False
CoverGreen
-
double
False
CoverTotal
-
double
False
LAI
m
2/m
2
double
False
WaterUptake
double
False
NitrogenUptake
double
False
Links (Dependencies)
Name
Type
IsOptional?
summary
ISummary
False
clock
IClock
False
mortalityRate
IFunction
False
seedMortalityRate
IFunction
False
Phenology
Phenology
False
Arbitrator
IArbitrator
True
structure
Structure
True
Leaf
ICanopy
True
Root
IRoot
True
Events published
Name
Type
Sowing
Void Sowing (Object sender, EventArgs e)
PlantSowing
Void PlantSowing (Object sender, SowingParameters e)
Harvesting
Void Harvesting (Object sender, EventArgs e)
PostHarvesting
Void PostHarvesting (Object sender, HarvestingParameters e)
PlantEnding
Void PlantEnding (Object sender, EventArgs e)
Flowering
Void Flowering (Object sender, EventArgs e)
StartPodDevelopment
Void StartPodDevelopment (Object sender, EventArgs e)
Methods (callable from manager)
Name
Description
Sow
void Sow(String cultivar, double population, double depth, double rowSpacing, double maxCover, double budNumber, double rowConfig, double seeds, int32 tillering, double ftn)
Sow the crop with the specified parameters.
Harvest
void Harvest(boolean removeBiomassFromOrgans)
Harvest the crop.
EndCrop
void EndCrop()
ReducePopulation
void ReducePopulation(double newPlantPopulation)
Reduce the plant population.
AddCultivar
void AddCultivar(Cultivar cultivar)
Add a cultivar.
7.2 SowingParameters
Parameters which control how a plant is sown.
Properties (Outputs)
Name
Description
Units
Type
Settable?
Cultivar
String
True
Population
/m2
double
True
Seeds
double
True
Depth
mm
double
True
RowSpacing
mm
double
True
MaxCover
double
True
BudNumber
double
True
SkipType
double
True
SkipRow
double
True
SkipPlant
double
True
SkipDensityScale
double
True
TilleringMethod
int32
True
FTN
double
True
7.3 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
Properties (Outputs)
Name
Description
Units
Type
Settable?
Structure
IStructure
True
StageNames
String
False
StageCodes
int32
False
AccumulatedTT
double
True
AccumulatedEmergedTT
double
True
Emerged
boolean
False
Stage
double
True
CurrentPhaseName
String
False
CurrentStageName
String
False
FractionInCurrentPhase
double
False
CurrentPhase
IPhase
False
Zadok
double
False
Links (Dependencies)
Name
Type
IsOptional?
plant
Plant
False
thermalTime
IFunction
False
zadok
ZadokPMFWheat
True
age
Age
True
Events published
Name
Type
PhaseChanged
Void PhaseChanged (Object sender, PhaseChangedType e)
StageWasReset
Void StageWasReset (Object sender, StageSetType e)
PlantEmerged
Void PlantEmerged (Object sender, EventArgs e)
PostPhenology
Void PostPhenology (Object sender, EventArgs e)
Methods (callable from manager)
Name
Description
IndexFromPhaseName
int32 IndexFromPhaseName(String name)
Look for a particular phase and return it's index or -1 if not found.
StartStagePhaseIndex
int32 StartStagePhaseIndex(String stageName)
Look for a particular stage and return it's index or -1 if not found.
EndStagePhaseIndex
int32 EndStagePhaseIndex(String stageName)
Look for a particular stage and return it's index or -1 if not found.
SetToEndStage
void SetToEndStage()
SetToStage
void SetToStage(String newStage)
A function that resets phenology to a specified stage
SetToStage
void SetToStage(double newStage)
A function that resets phenology to a specified stage
SetAge
void SetAge(double newAge)
Allows setting of age if phenology has an age child
OnStartDayOf
boolean OnStartDayOf(String stageName)
A utility function to return true if the simulation is on the first day of the specified stage.
InPhase
boolean InPhase(String phaseName)
A utility function to return true if the simulation is currently in the specified phase.
Between
boolean Between(int32 startPhaseIndex, int32 endPhaseIndex)
A utility function to return true if the simulation is currently between the specified start and end stages.
Between
boolean Between(String start, String end)
A utility function to return true if the simulation is currently between the specified start and end stages.
Beyond
boolean Beyond(String start)
A utility function to return true if the simulation is at or past the specified startstage.
BeyondPhase
boolean BeyondPhase(int32 phaseIndex)
A utility function to return true if the simulation is at or past the specified startstage.
BeforePhase
boolean BeforePhase(int32 phaseIndex)
A utility function to return true if the simulation is before the specified phaseIndex.
PhaseStartingWith
IPhase PhaseStartingWith(String start)
A utility function to return the phenological phase that starts with the specified start stage name.
PhaseBetweenStages
boolean PhaseBetweenStages(String startStage, String endStage, IPhase checkPhase)
Helper function to check if a particular phase is present between specifice start and end stages.
ResetCampVernParams
void ResetCampVernParams(FinalLeafNumberSet overRideFLNParams)
Resets the Vrn expression parameters for the CAMP model
OnCreated
void OnCreated()
SetEmergenceDate
void SetEmergenceDate(String emergenceDate)
Force emergence on the date called if emergence has not occurred already
SetGerminationDate
void SetGerminationDate(String germinationDate)
Force germination on the date called if germination has not occurred already
GetPhaseTable
DataTable GetPhaseTable()
8 References
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Gou, Fang, van Ittersum, Martin K., Simon, Elisabeth, Leffelaar, Peter A., van der Putten, Peter E.L., Zhang, Lizhen, van der Werf, Wopke, 2017. Intercropping wheat and maize increases total radiation interception and wheat RUE but lowers maize RUE. European Journal of Agronomy 84, 125-139.
