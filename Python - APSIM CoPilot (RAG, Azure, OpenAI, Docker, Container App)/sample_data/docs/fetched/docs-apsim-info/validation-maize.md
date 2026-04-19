# Maize documentation

Source URL: https://docs.apsim.info/validation/Maize
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:01.333814+00:00

1 The APSIM Maize Model
The APSIM Maize model has been tested on a range of datasets from around the world to provide tests across a range of environmental conditions.  Tests include ranges in plant population, nitrogen management and irrigation levels, as well as including historical datasets from africa using cultivars developed during the 1960's, through to modern varieties grown in the American mid-West.
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
Brown, H.E., Teixeira, E.I., Huth, N.I. and Holzworth, D.P.
###Future Development requirements
Phenology not responding to stress events.  Requires better data sets for quantifying the effects of water and nitrogen on leaf apperance and the timing of development stages.
Phosphorus response
Improved parameterisation of supply and demand for N and DM from organs and arbitration of these resources.
Heat Stress responses in grain number
More validation under a wider range of environments with more detailed datasets
Seedling mortality
Water demand (MicroClimate) needs validation
The model is constructed from the following list of software components. Details of the implementation and model parameterisation are provided in the following sections.
1.1 Plant Model Components
Component Name
Component Type
Arbitrator
Models.PMF.OrganArbitrator
Phenology
Models.PMF.Phen.Phenology
Structure
Models.PMF.Struct.Structure
Grain
Models.PMF.Organs.ReproductiveOrgan
Root
Models.PMF.Organs.Root
Leaf
Models.PMF.Organs.Leaf
Husk
Models.PMF.Organs.GenericOrgan
Rachis
Models.PMF.Organs.GenericOrgan
Stem
Models.PMF.Organs.GenericOrgan
MortalityRate
Models.Functions.Constant
SeedMortalityRate
Models.Functions.Constant
1.2 Composite Biomass
Component Name
Component Type
AboveGround
Models.PMF.CompositeBiomass
BelowGround
Models.PMF.CompositeBiomass
Total
Models.PMF.CompositeBiomass
TotalLive
Models.PMF.CompositeBiomass
TotalDead
Models.PMF.CompositeBiomass
EarLive
Models.PMF.CompositeBiomass
AboveGroundLive
Models.PMF.CompositeBiomass
AboveGroundDead
Models.PMF.CompositeBiomass
Spike
Models.PMF.CompositeBiomass
1.3 Cultivars
Cultivar Name
Alternative Name(s)
Hycorn_53
Hycorn_53
Pioneer_33M54
Pioneer_33M54
Pioneer_38H20
Pioneer_38H20
Pioneer_34K77
Pioneer_34K77
Pioneer_39V43
Pioneer_39V43
Atrium
Atrium
Laila
Laila
GH_5019WX
GH_5019WX
Hycorn_40
Hycorn_40
GH_5009
GH_5009
Dekalb_XL82
Dekalb_XL82
malawi_local
malawi_local
mh19
mh19
mh17
mh17
mh16
mh16
mh12
mh12
sc623
sc623
sc625
sc625
sc601
sc601
CG4141
CG4141
mh18
mh18
r215
r215
Melkassa
Melkassa
sr52
sr52
sc501
sc501
r201
r201
sc401
sc401
NSCM_41
NSCM_41
Makueni
Makueni
Katumani
Katumani
Pioneer_3153
Pioneer_3153
Pioneer_39G12
Pioneer_39G12
B_80
B_80
B_90
B_90
B_95
B_95
B_100
B_100
B_103
B_103
B_105
B_105
B_108
B_108
B_110
B_110
B_112
B_112
B_115
B_115
B_120
B_120
B_130
B_130
A_80
A_80
A_90
A_90
A_95
A_95
A_100
A_100
A_103
A_103
A_105
A_105
A_108
A_108
A_110
A_110
A_112
A_112
A_115
A_115
A_120
A_120
A_130
A_130
HY_110
HY_110
LY_110
LY_110
P1197
P1197
1.4 Child Components
1.4.1 Arbitrator
The Arbitrator class determines the allocation of dry matter (DM) and Nitrogen between each of the organs in the crop model. Each organ can have up to three different pools of biomass:
Structural biomass
which is essential for growth and remains within the organ once it is allocated there.
Metabolic biomass
which generally remains within an organ but is able to be re allocated when the organ senesces and may be retranslocated when demand is high relative to supply.
Storage biomass
which is partitioned to organs when supply is high relative to demand and is available for retranslocation to other organs whenever supply from uptake, fixation, or re allocation is lower than demand.
The process followed for biomass arbitration is shown in the figure below. Arbitration calculations are triggered by a series of events (shown below) that are raised every day.  For these calculations, at each step the Arbitrator exchange information with each organ, so the basic computations of demand and supply are done at the organ level, using their specific parameters.
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
1.4.2 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
1.4.3 Structure
The structure model simulates morphological development of the plant to inform the Leaf class
when and how many leaves and branches appear and provides an estimate of height.
Though tillering is common in some maize varieties under certain agronomic conditions, no tillering is accounted for within this model.  Therefore all branching has been parameterised out of the current maize model.
1.4.4 Grain
This organ uses a generic model for plant reproductive components.  Yield is calculated from its components in terms of organ number and size (for example, grain number and grain size).
1.4.5 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
1.4.6 Leaf
The leaves are modelled as a set of leaf cohorts and the properties of each of these cohorts are summed to give overall values for the leaf organ.
A cohort represents all the leaves of a given main  stem node position including all of the branch leaves appearing at the same time as the given main stem leaf (
Lawless et al., 2005
).
The number of leaves in each cohort is the product of the number of plants per m
2
and the number of branches per plant.
The
Structure
class models the appearance of main stem leaves and branches.  Once cohorts are initiated the
Leaf
class models the area and biomass dynamics of each.
It is assumed all the leaves in each cohort have the same size and biomass properties.  The modelling of the status and function of individual cohorts is delegated to
LeafCohort
classes.
1.4.7 Husk
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.8 Rachis
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.9 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.10 MortalityRate
A constant function (name=value)
1.4.11 SeedMortalityRate
A constant function (name=value)
2 Validation
2.1 Combined Results
Simulation results for the combined datasets from the various countries are shown in the following graphs.  The model is able to adequately capture the influence of growing conditions (soil, climate) and management (population, Nitrogen, irrigation, sowing date).
2.2 Africa
A selection of data from Kenya has been included from the work of
KEATING et al., 1992
.  These include the trial originally labelled JMW2 by the original authors and was labelled Experiment 6 in
KEATING et al., 1992
.  This trial includes maize (cultivar Katumani Composite B) sown at 5 populations (1.1, 2.2, 4.4, 6.6 and 8.8 plants/m2) under low (0 kg N/ha applied) and high (120 kgN/ha) fertiliser conditions during the Long Rains of 1988.  This same experimental design was conducted at two locations, Katumani (1o 35' S, 37o 14' E) and Kiboko (2o 13' S, 37o 43' E). The trial was rainfed at Katumani but fully irrigated at Kiboko.  Yields ranged from 2000 to 5400 kg/ha at Katumani and 1000 to 6000 kg/ha at Kiboko.  There were strong population x nitrogen interactions at both sites.  A second trial refered to as BMW1 or Experiment 1 in
KEATING et al., 1992
.  A range of populations (2.0-6.5 plants/m2) and irrigation treatments (6-176 mm) where planted under high (80 kgN/ha) and low (0kgN/ha) fertiliser conditions at Katumani. Individual replicates are modelled separately due to variability in soil (depth to rock), establishment and irrigation application.  Grain yields varied from 1600 to 8000 kg/ha.
Note.  The BMW1 trail consists of data from unreplicated plots.
Experiment Name
Design (Number of Treatments)
JMW2Katumani
Popn x Fert (10)
JMW2Kiboko
Popn x Fert (10)
BMW1
Site (24)
2.3 Australia
The data for two trials from near Katherine, Northern Territory, Australia, have been included from
CARBERRY et al., 1989
and [Carberry_&_Aberecht1991].  These include two planting dates for the summer of 1983/1984 (labelled as K841, K842).  At each planting date, cultivar Dekalb XL82 was sown at 4 populations (3, 5, 7, 9 plants/m
2
) under low and high irrigation conditions.  The low irrigation treatments only included preliminary applications to ensure crop establishment.
Experiment Name
Design (Number of Treatments)
K841
Popn x Irrigation (8)
K842
Popn x Irrigation (8)
DRK1
Sowing (7)
DRK2
Sowing (7)
DRK3
Sowing (7)
Angelo98
Fert (3)
Angelo99
Fert (3)
Angelo00
Fert (3)
This data set has been published by
Massignam et al., 2009
. Three sowings were conducted at the CSIRO Cooper field station (27o34'S, 152o20'E), Gatton, Australia. The soil was a Lockyer clay loam for sowing 1 and 2 and a Hooper clay loam for sowing 3.  Maize (Hycorn53) was sown on three dates to provide a range of growing environments.  Each sowing was provided with 3 rates of nitrogen.  To ensure moisture was non-limiting, the plots were irrigated weekly by trickle irrigation, except when rainfall occurred. Plots were weeded regularly and insect pests and foliar diseases controlled as required.
2.4 USA
Testing of APSIM Maize, Soil Water, Soil Nitrogen, Manure, and Soil Temperature Modules in the Midwestern United States was undertaken by [Archontoulis_etal_2014] using APSIM version 7.4.  A subset of these data has been chosen for testing of the maize model. Datasets involving application of compost have not been included so as to concentrate on tests of maize model performance, rather than overall soil organic matter dynamics.
Experiment Name
Design (Number of Treatments)
Ames2011
NRate (3)
2.5 New Zealand
Testing of APSIM Maize under New Zealand conditions to determine the perormance of the model under temperate conditions with cool springs and mild summers.  A range of trials have been conducted and are detailed below
Experiment Name
Design (Number of Treatments)
Lincoln2012
Nit x Irr (6)
Lincoln2010
Nitrogen (2)
Lincoln2008
Sow x Cover (8)
HawksBay2010
Sow x Cv (9)
Lincoln2012 (Rain-Shelter Trail)
Testing of APSIM Maize under New Zealand conditions was undertaken using the data of
Teixeira et al., 2014
.  This dataset includes the impact of three N (0 to 250 kg/ha N) and two water regimes (dryland and fully irrigated) using a rain-shelter structure.  Observations include biomass growth and nitrogen content of individual organs, soil water contents, leaf area index, phenology and yield components. Total biomass ranged from 8000 kg/ha for dryland nil N crops to up to 28000kg/ha for fully irrigated and N fertilised crops.  Dryland crops recovered 25 percent less N from applied fertilizer than irrigated crops.
Lincoln2010 (Leaf properties trial)
This trial was conducted to provide data for parametersising the Leaf Area model and is described in detail by
Teixeira et al., 2011
.  Detailed measurements were conducted on leaf apperance and expansion with some measurements of biomass accumulation.  Treatments of plus and minus Nitrogen were applied but there was sufficient N in the soil than N responses were very small.
3 Sensibility Tests
Experiment Name
Design (Number of Treatments)
Bugesera
NRate (2)
Maize is grown in the Bugesera region of southern Rwanda in Central Africa.  The region has a bimodal rainfall distribution which allows two plantings per year.  For low input situations the maize yields should vary between 1 and 3 t/ha per crop.  Under fertilised conditions the yield should increase up to 5 t/ha per crop.
4 Interface
4.1 Maize
Properties (Outputs)
Name
Description
Units
Type
Settable?
Structure
IStructure
True
AboveGround
IBiomass
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
4.2 SowingParameters
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
4.3 Phenology
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
5 References
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
CARBERRY, PS, MUCHOW, RC, MCCOWN, RL, 1989. TESTING THE CERES-MAIZE SIMULATION-MODEL IN A SEMI-ARID TROPICAL. FIELD CROPS RESEARCH 20 (4), 297-315.
KEATING, BA, WAFULA, BM, WATIKI, JM, 1992. DEVELOPMENT OF A MODELING CAPABILITY FOR MAIZE IN SEMIARID EASTERN KENYA. ACIAR PROCEEDINGS SERIES, 26-33.
Lawless, Conor, Semenov, MA, Jamieson, PD, 2005. A wheat canopy model linking leaf area and phenology. European Journal of Agronomy 22 (1), 19-32.
Massignam, AM, Chapman, SC, Hammer, GL, Fukai, S, 2009. Physiological determinants of maize and sunflower grain yield as affected by nitrogen supply. Field crops research 113 (3), 256-267.
Teixeira, E. I., George, M., Brown, H. E., Fletcher, A. L., 2011. A framework for quantifying maize leaf expansion and senescence at the individual leaf level. Agronomy New Zealand 41, 59-65.
Teixeira, Edmar I., George, Michael, Herreman, Thibault, Brown, Hamish, Fletcher, Andrew, Chakwizira, Emmanuel, de Ruiter, John, Maley, Shane, Noble, Alasdair, 2014. The impact of water and nitrogen limitation on maize biomass and resource-use efficiencies for radiation, water and nitrogen. Field Crops Research 168, 109-118.
