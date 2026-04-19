# Peanut documentation

Source URL: https://docs.apsim.info/validation/Peanut
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:12.554790+00:00

1 The APSIM Peanut Model
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
The model consists of:
a phenology model to simulate development between growth phases
a structure model to simulate plant morphology
a collection of organs to simulate the various plant parts
an arbitrator to allocate resources (N, biomass) to the various plant organs
The model is constructed from the following list of software components. Details of the implementation and model parameterisation are provided in the following sections.
1.1 Plant Model Components
Component Name
Component Type
Arbitrator
Models.PMF.OrganArbitrator
Phenology
Models.PMF.Phen.Phenology
Leaf
Models.PMF.Organs.SimpleLeaf
Grain
Models.PMF.Organs.ReproductiveOrgan
Root
Models.PMF.Organs.Root
Nodule
Models.PMF.Organs.Nodule
Shell
Models.PMF.Organs.GenericOrgan
Stem
Models.PMF.Organs.GenericOrgan
AflotoxinRisk
Models.Functions.BoundFunction
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
AboveGroundLive
Models.PMF.CompositeBiomass
Total
Models.PMF.CompositeBiomass
TotalLive
Models.PMF.CompositeBiomass
TotalDead
Models.PMF.CompositeBiomass
Pod
Models.PMF.CompositeBiomass
1.3 Cultivars
Cultivar Name
Alternative Name(s)
EarlyBunch
EarlyBunch
VirginiaBunch
VirginiaBunch
TMV2
TMV2
Tapir
Tapir
VB97
VB97
Florunner
Florunner
Redvale
Redvale
Taabinga
Taabinga
Holt
Holt
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
In the new model we simplified phenology by taking out stages that are not measurable (e.g. end of juvenile stage) and by adding new stages that are measurable (e.g. start pod).
The new phenology follows the V/R staging system
1.4.3 Leaf
This organ is simulated using a SimpleLeaf organ type.  It provides the core functions of intercepting radiation, producing biomass
through photosynthesis, and determining the plant's transpiration demand.  The model also calculates the growth, senescence, and
detachment of leaves.  SimpleLeaf does not distinguish leaf cohorts by age or position in the canopy.
Radiation interception and transpiration demand are computed by the MicroClimate model.  This model takes into account
competition between different plants when more than one is present in the simulation.  The values of canopy Cover, LAI, and plant
Height (as defined below) are passed daily by SimpleLeaf to the MicroClimate model.  MicroClimate uses an implementation of the
Beer Lambert equation to compute light interception and the Penman Monteith equation to calculate potential evapotranspiration.
These values are then given back to SimpleLeaf which uses them to calculate photosynthesis and soil water demand.
SimpleLeaf has two options to define the canopy: the user can either supply a function describing LAI or a function describing canopy cover directly.  From either of these functions SimpleLeaf can obtain the other property using the Beer Lambert equation with the specified value of extinction coefficient.
The effect of growth rate on transpiration is captured by the Fractional Growth Rate (FRGR) function, which is passed to the MicroClimate model.
1.4.4 Grain
This organ uses a generic model for plant reproductive components.  Yield is calculated from its components in terms of organ number and size (for example, grain number and grain size).
1.4.5 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
1.4.6 Nodule
This organ simulates the root structure associate with symbiotic N fixing bacteria.  It provides the core functions of determining
N fixation supply and related costs.  It also calculates the growth, senescence and detachment of nodules.
1.4.7 Shell
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.8 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.9 AflotoxinRisk
Bounds the child function between lower and upper bounds
1.4.10 MortalityRate
A constant function (name=value)
1.4.11 SeedMortalityRate
A constant function (name=value)
2 Validation
The Peanut model has been tested across a range of planting and agronomic conditions.  These include datasets covering a wide range of weather conditions (See graphs that follow) and agronomic factors (irrigation, fertiliser, sowing dates).
2.1 Map
2.2 Combined Results
Simulation results for the combined datasets from the various countries are shown in the following graphs.  The model is able to adequately capture the influence of growing conditions (soil, climate) and management (population, Nitrogen, irrigation, sowing date).
2.3 Australia
Experiment Name
Design (Number of Treatments)
Kingaroy8384
Sow x Cv (33)
Kingaroy8485
Sow x Pop (6)
Kingaroy8990
Cv (3)
Kingaroy1819
Cv x Irrigation (4)
Kunnunura8182
Pop (2)
Kunnunura8283
Sow (5)
Bundaberg8990
Cv (4)
Loxton9697
Pop (6)
Roseworthy96
Sow x Cv (9)
This dataset (
Bell, 1992
) provides data on growth for three cultivars grown under irrigated conditions in at Kingaroy in Queensland. The soil type was a deep, red clay-loam with a friable structure and increasing clay content with depth.  Supplementary irrigations were made to avoid moisture stress.
This dataset (
Bell, 1986
) includes the cultvar "EarlyBunch" grown on 5 sowing dates under irrigated conditions in a monsoonal tropical environment at Kunnunura in Northern Australia.  Plants were sampled regularly for biomass partitioning and canopy development.  The soil is known locally as a normal phase Cockatoo Sand - an acid, coarse-textured and
well-drained soil.  Supplementary irrigations were made to avoid moisture stress.
This dataset (
Bell, 1994
) studied N fixation of various Peanut cultivars at Bundaberg, Queensland, Australia.  The soil type was a deep, red clay-loam with a friable structure and increasing clay content with depth.  The experiment was grown under irrigated conditions. Measurements were undertaken for biomass and nitrogen contents, and estimates of the contribution of N fixation to plant N contents.
This dataset (
Porter, 2000
) studied the impact of plant population on growth and productivity.  Here we consider the cultvar EarlyBunch only (the comparison cultivar failed during the trial).  The experiment was grown under irrigated conditions in at Loxton in South Australia. The soil type was sandy loam with good drainage but low water holding capacity.
Supplementary irrigations were made to avoid moisture stress.
This dataset (
Porter, 2000
) studied the impact of sowing date on flowering and maturity for 24 germplasm lines.  Here we consider the cultvars EarlyBunch, Florunner, and VB97.  The experiment was grown under irrigated conditions in at Roseworthy in South Australia. The soil type was clay-loam with good water holding capacity.
well-drained soil.  Supplementary irrigations were made to avoid moisture stress.
2.4 USA
2.4.1 Headland Florunner
This dataset (
Davis, 1991
) studied the differences in canopy development for florunner cultivars.  The experiment was grown under irrigated conditions at the Wiregrass Substation in Headland, Alabama. Supplementary irrigations were made to avoid moisture stress.  Plants were sampled for plant vegetative stage, reproductive stage, height, number of leaves, leaf area, leaf dry weight, number of pods, pod dry weight, stem dry weight, and stand density.
Experiment Name
Design (Number of Treatments)
Headland88
Cv (1)
2.4.2 Gainsville EarlyBunch
This dataset (from Hammer_etal_QNut) studied the differences in growth for two cultivars.  We include only the cultivar "EarlyBunch". The experiment was grown under irrigated conditions at Gainesville, Florida. Supplementary irrigations were made to avoid moisture stress.  Plants were sampled for total biomass, pod biomass and leaf area index.
Experiment Name
Design (Number of Treatments)
Gainsville90
Cv (1)
3 Sensibility
3.1 DetailedDynamics
Experiment Name
Design (Number of Treatments)
DetailedDynamics
Sow (1)
3.2 Aflotoxin Risk
Experiment Name
Design (Number of Treatments)
Aflotoxin
Cv x Sow (4)
This sensibility test demonstrates predicted pod yields for rainfed crops for two sowing dates (mid November and mid December) for Kingary, Queensland, Australia. Pod yields should range from 2t/ha to 5 t/ha with decreased risk from Aflotoxin for later sowing and later maturing cultivars.
3.3 NewZealand
Experiment Name
Design (Number of Treatments)
NewZealand
Water (2)
Peanuts are currently being evaluated as a crop for the Northlands region of northern New Zealand.  Test crops in the region are currently yielding up to 6-7 t/ha (Fresh Weight), which would correspond to a dry pod weight of 5-6 t/ha (16%-20% moisture).
4 UnderDevelopment
Experiment Name
Design (Number of Treatments)
Roseworthy9798
Pop x SR (16)
This dataset (
Porter, 2000
) studied the impact of spatial arrangement on peanut crop microenvironment for the cultvar VB97.  The experiment was grown under irrigated conditions in at Roseworthy in South Australia. The soil type was clay-loam with good water holding capacity.
well-drained soil.  Supplementary irrigations were made to avoid moisture stress.
Treatments included a full factorial of population (8.8, 27.3, 39.4 and 58.8 plants/m^2) and Spatial Ratio (1:1, 1:2.2, 1:4.6 and 1:7.2 Inter-row:Intra-row).
5 Interface
5.1 Peanut
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
5.2 SowingParameters
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
5.3 Phenology
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
6 References
Bell, M., 1986. Effect of sowing date on growth and development of irrigated peanuts, Arachis hypogaea L. cv. Early Bunch, in a monsoonal tropical environment.. Australian Journal of Agricultural Research 37, 361-373.
Bell, MJ, Wright, GC, 1994. The N2-fixing capacity of peanut cultivars with differing assimilate partitioning characteristics. Australian Journal of Agricultural Research 45, 1455-1468.
Bell, MJ, Wright, GC, Hammer, GL, 1992. Night Temperature Affects Radiation-Use Efficiency in Peanut. Crop Science 32, 1329-1335.
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Davis, David P., Mack, Timothy P., 1991. Relations Between Leaf Area Index and Growth Characteristics of Florunner, Southern Runner, and Sunrunner Peanut. Peanut Science 18, 30-37.
Porter, Wade, 2000. Potential for peanut production in Southern Australia..
