# Canola documentation

Source URL: https://docs.apsim.info/validation/Canola
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:51:50.071862+00:00

1 The APSIM Canola Model
The APSIM Canola Model
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
The model is constructed from the following list of software components. Details of the implementation and model parameterisation are provided in the following sections.
1.1 Plant Model Components
Component Name
Component Type
Arbitrator
Models.PMF.OrganArbitrator
Phenology
Models.PMF.Phen.Phenology
Root
Models.PMF.Organs.Root
Leaf
Models.PMF.Organs.SimpleLeaf
Grain
Models.PMF.Organs.ReproductiveOrgan
Stem
Models.PMF.Organs.GenericOrgan
Shell
Models.PMF.Organs.GenericOrgan
MortalityRate
Models.Functions.Constant
SeedMortalityRate
Models.Functions.Constant
1.2 Composite Biomass
Component Name
Component Type
Pod
Models.PMF.CompositeBiomass
AboveGround
Models.PMF.CompositeBiomass
AboveGroundLive
Models.PMF.CompositeBiomass
BelowGround
Models.PMF.CompositeBiomass
Total
Models.PMF.CompositeBiomass
TotalLive
Models.PMF.CompositeBiomass
TotalDead
Models.PMF.CompositeBiomass
1.3 Cultivars
Cultivar Name
Alternative Name(s)
GenericWinter
GenericWinter
GenericEarly
GenericEarly
GenericLate
GenericLate
43C80_CL
43C80_CL
44Y84_CL
44Y84_CL
44Y87_CL
44Y87_CL
44Y89_CL
44Y89_CL
44Y90_CL
44Y90_CL
45Y25_RR
45Y25_RR
45Y86_CL
45Y86_CL
45Y88_CL
45Y88_CL
45Y91_CL
45Y91_CL
46Y78
46Y78
Arazzo
Arazzo
Archer
Archer
ATR_Bonito
ATR_Bonito
ATR_Gem
ATR_Gem
ATR_Marlin
ATR_Marlin
ATR_Stingray
ATR_Stingray
ATR_Wahoo
ATR_Wahoo
Crusher_TT
Crusher_TT
AV_Garnet
AV_Garnet
AV_Zircon
AV_Zircon
CB_Taurus
CB_Taurus
CB_Telfer
CB_Telfer
CBI_306
CBI_306
CSCH_01
CSCH_01
CSCH_02
CSCH_02
Drum_triazine
Drum_triazine
Dunkeld
Dunkeld
Grouse
Grouse
GT50_RR
GT50_RR
Hyola42
Hyola42
Hyola450_TT
Hyola450_TT
Hyola559_TT
Hyola559_TT
Hyola575_CL
Hyola575_CL,Hyola_575_CL_GT
Hyola577_CL
Hyola577_CL
Hyola600_RR
Hyola600_RR
Hyola635_CL
Hyola635_CL
Hyola650_TT
Hyola650_TT
Hyola725_RT
Hyola725_RT
Hyola750_TT
Hyola750_TT
Hyola970_CL
Hyola970_CL
Hyola971_CL
Hyola971_CL
IH30_RR
IH30_RR
IVT4510
IVT4510
K50055
K50055
K50058
K50058
Karoo_triazine
Karoo_triazine
Monty
Monty
NS_Diamond
NS_Diamond
NX953
NX953
Oscar
Oscar
Pinnacle_triazine
Pinnacle_triazine,Pinnacle
Range
Range
Sensation
Sensation
SF_Brazzil
SF_Brazzil
SF_Edimax
SF_Edimax
Victory_7001_CL
Victory_7001_CL
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
1.4.3 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
1.4.4 Leaf
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
1.4.5 Grain
This organ uses a generic model for plant reproductive components.  Yield is calculated from its components in terms of organ number and size (for example, grain number and grain size).
1.4.6 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.7 Shell
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.8 MortalityRate
A constant function (name=value)
1.4.9 SeedMortalityRate
A constant function (name=value)
2 Validation
A test dataset has been developed to test the APSIM Canola model for a range of environmental (soil and climate) conditions, management options (sowing dates, populations, nitrogen rates, irrigation) and genetic backgrounds (different regions, cultivar types).  These tests have been grouped into experiments focusing on phenological development or growth and resource use.  Graphs of model performance are provided for yield, biomass production, canopy development, phenological development, water and nitrogen uptake, and grain yield components.
2.1 Map
2.2 Phenology Experiments
2.2.1 OCP Phenology
GRDC funded Optimised Canola Profitability project had experiments at Gatton in 2015, and Gatton and Canberra in 2016 and 2017 which consisted of 3 to 5 sowing dates
Whish et al., 2020
. In 2016 and 2017 photoperiod treatments (daylength extension with lights) were also applied. Experiments were in the field in single rows of 1-3.5m in length and row spacing of 50cm. Plants were well watered and supplied with nutrients and were removed after flowering.  The total number of cultivars included across the 5 experiments was 37, with each cultivar being included 4 to 28 combinations of location, year, TOS, and photoperiod (most cultivars between 13 and 18 combinations.
Data collected:  plant growth stage and leaf number was recorded 2 to 3 x weekly between emergence and flowering for 8 plants in each plot. Data was summarised as average sowing, emergence, floral initation, green bud, flowering date for each cultivar/treatment.
Contact Jeremy Whish, Julianne Lilley (CSIRO)
Experiment Name
Design (Number of Treatments)
Gatton2015
TOS x Cv x PP (90)
Gatton2016
TOS x Cv x PP (228)
Gatton2017
TOS x Cv x PP (132)
Canberra2016
TOS x Cv x PP (252)
Canberra2017
TOS x Cv x PP (198)
2.2.2 Omics Phenology
GRDC funded "CSP1901-002RTX: Optimising Canola Production in Diverse Australian Growing Environments" project had experiments at Gatton in 2019, 2020, Boorowa 2019, 2021, Cummins 2020, 2021, York 2020, 2021 which consisted of 2-4 sowing dates.  Project aimed to use field observations and transcritptomic and genomic data to establish a genetic phenology model. This project assesses G x E effects on phenological traits (including flowering time) in a diverse set of modern Australian and globally important canola varieties (~350) across key growing environments and regions relevant to the Australian industry (14 combinations of site, year and TOS), as well as three controlled environments.
Data collected:  plant growth stage and leaf number was recorded 2 to 3 x weekly between emergence and flowering for 4 plants in each plot. Data was summarised as average sowing, emergence, floral initation, green bud, flowering date for each cultivar/treatment.
Contact Jeremy Whish, Julianne Lilley (CSIRO)
Experiment Name
Design (Number of Treatments)
Gatton2019
TOS x Cv (28)
Gatton2020
TOS x Cv (18)
Boorowa2019
TOS x Cv (24)
Cummins2020
TOS x Cv (18)
York2020
TOS x Cv (18)
2.3 Growth Experiments
2.3.1 Kojonup
Experiment Name
Design (Number of Treatments)
Kojonup2013
Cv x Fert (18)
Kojonup2014
Cv x Fert (20)
2.3.2 Cunderdin
Experiment Name
Design (Number of Treatments)
Cunderdin2013
Cv x Fert (36)
Cunderdin2014
Cv x Fert (20)
2.3.3 Wagga
Experiment Name
Design (Number of Treatments)
Wagga2008
Cv x Treat (12)
McCormick et al 2012. irrigated experiment,3 cultivars, 3 densities, grazed and ungrazed
2.3.4 Harden
Experiment Name
Design (Number of Treatments)
Harden1999
Cv (1)
2.3.5 Greenthorpe
Experiment Name
Design (Number of Treatments)
Greenethorpe2013_Ex1
Cv (1)
Greenethorpe2013_Ex2
Cv (1)
Greenethorpe2013_Ex3
Cv (3)
Greenethorpe2014_Ex1
Cv (1)
Greenethorpe2014_Ex2
Cv (1)
Greenethorpe2014_Ex3
Cv x TOS (24)
2.3.6 Merredin
Experiment Name
Design (Number of Treatments)
Merredin2014
Cv x Fert (20)
3 Sensibility
Experiment Name
Design (Number of Treatments)
DetailedDynamics
Cv (1)
CanolaGrazing
StockingRate (3)
Wagga
Cv x TOS (6)
This test demonstrates the detailed dynamics within a simple simulation of conola growth and resource use.  This includes:
Dry matter and Nitrogen supply and demand for individual organs
Dry matter and Nitrogen content for live and dead components of each organ
Light interception for live and dead components for each organ
This sensibility test performs a livestock grazing simulation for canola planted in March, and grazed in late May as a dual-purpose crop.
This test demonstrates the effect of time of sowing and cultivar (generic early, late or winter cultivars) on yield, biomass and time to harvest for long term simulations at Wagga Wagga, NSW.
4 Interface
4.1 Canola
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
5 Media
6 References
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Whish, JPM, Lilley, JM, Morrison, MJ, Cocks, B, Bullock, M, 2020. Vernalisation in Australian spring canola explains variable flowering responses. Field Crops Research 258, 107968.
