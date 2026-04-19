# Potato documentation

Source URL: https://docs.apsim.info/validation/Potato
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:25.220934+00:00

1 The APSIM Potato Model
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
Brown, H.E., Huth, N.I. and Holzworth, D.P.
Building the model.
The APSIM potato model has been described in part by
Brown et al., 2011
and developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a model in much the same way that models can be coupled to construct a simulation. This means that dynamic composition of lower level process and organ classes (e.g. photosynthesis, leaf) into larger constructions (e.g. maize, wheat, sorghum) can be achieved by the model developer without additional coding.
The model consists of:
a phenology model to simulate development between growth phases
a structure model to simulate plant morphology
a collection of organs to simulate the biomass (N and Weight) of various plant parts
an arbitrator to allocate biomass to the various plant organs
Pecularities of potato
Potato is a challenging crop to model because it is vegetatviely propigated.  For seed propigated crops, plants are always equally dormant at planting and their development begins from there.  With potato the physiological status of the seed tubers vary depending on:
the conditions the mother plant encountered late in its life (Stress, Desication etc)
the conditions that the tuber was stored in (Cool store, ground store, dry store etc)
treatment of the seed prior to planting (chitting, cutting etc).
All of these factors infuence the developmental behaviour of the crop that is produced including:
The timing of emergence
The number of stems produced per plant and subsequent tuber number and canopy development
The timing of tuber initiation and subsequent growth and partitioning
The duration of haulm growth and subsequent tuber filling duration
There are an enormous number of potato cultivars and they all differ in their responses of post planting behavour due to pre planting seed conditions, there is not a good quantitative understanding of how environment and managemet of seed potato prior to planting influences post planting behaviour and it is beyond the scope of the current APSIM infrastructure to provide environmental conditions during storage of seed.  We have tried but with a list of excuses this long we have not yet succeeded in building the effects of seed physiology into the potato model in anything other than a very elementry way.  However, it is possible to set the model up to capture seed effects for specific simulations.
Including a potato crop in an APSIM simulation
There is a Potato example simulation available by clicking the "Open an Example" icon available on the first tab displayed when APSIM Next Gen is opened.  This provides a useful demonstration of how to simulate a potato crop and provides some useful graphs for viewing models predictions.
To incluce a potato crop in a simulation the "Potato" model needs to be added to the paddock, field or zone in which it is to be grown.  This can be done by right clicking on the "Paddock", selecting "Add" from the drop down menu and then selecting "Potato" from the list that comes up.  A sowing action also needs to be set to start the crop off and a harvest event to remove the potato crop so the next crop can be simulated.  The "PotatoPlantAndHarvest" manager module that is included in the "Potato" example simulation has script written to instigate these events and can be copied from the example simulation into other simulations.
Setting sowing depth (emergence date)
The potato model predicts emergence date as a function of sowing depth and all subsequent phenology is dependent on this timing.  Because this date is effected by seed physiology and can't yet be modeled we recommend that planting depth is not set at the actual planting depth, but varied to ensure the model predicts a realistic emergence date.  Planting depth is set in the 'PotatoPlantAndHarvest' module
Setting stem number (tuber number and early canopy growth)
The 'PotatoPlantAndHarvest' script sets the "Number of stems per seed piece".  This varies enormiously due to seed physiology and variety and should be set to a number that is realistic for the cultivar being simulated and the pre-planting treatment of the seed tubers.  This parameter influences tuber number and the early development of the canopy so should be set carefully.
###Setting final leaf number (Crop duration)
The duration of a potato crop is determined by the number of leaves that the mainstem produces and how long it takes for these to appear and seness.  This responds to photoperiod in many varieties and is influenced by the pre-planting treatment of seed tubers.  As such this parameter is given a default value for each cultivar but can also be set in the "PotatoPlantAndHarvest" script if more is know about the expected determinany of the potatoes under the circumstances being simulated.  Please read
Brown et al., 2011
for a clear definition of final leaf number.
The model is constructed from the following list of software components. Details of the implementation and model parameterisation are provided in the following sections.
1.1 Plant Model Components
Component Name
Component Type
Phenology
Models.PMF.Phen.Phenology
Arbitrator
Models.PMF.OrganArbitrator
Structure
Models.PMF.Struct.Structure
Tuber
Models.PMF.Organs.GenericOrgan
Root
Models.PMF.Organs.Root
Leaf
Models.PMF.Organs.Leaf
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
1.3 Cultivars
Cultivar Name
Alternative Name(s)
Coliban
Coliban
Horizon
Horizon
IlamHardy
IlamHardy
JerseyBenny
JerseyBenny
Karaka
Karaka
Nadine
Nadine
Nooksac
Nooksac
RedRascal
RedRascal
Rua
Rua
RussetBurbank
RussetBurbank,Fianna,RussetRanger,Moonlight,Agria,Bondi,Crop39
1.4 Child Components
1.4.1 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
1.4.2 Arbitrator
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
1.4.3 Structure
The structure model simulates morphological development of the plant to inform the Leaf class
when and how many leaves and branches appear and provides an estimate of height.
1.4.4 Tuber
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
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
1.4.7 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.8 MortalityRate
A constant function (name=value)
1.4.9 SeedMortalityRate
A constant function (name=value)
2 Validation
A test dataset has been developed to test the APSIM Potato model for a range of environmental (soil and climate) conditions, management options (sowing dates, populations, nitrogen rates) and genetic backgrounds (different regions, cultivar types).  These tests have been grouped into various geographical regions to allow users to evaluate the suitability of the model for their particular region of interest.  Graphs of model performance are provided for yield, biomass production, canopy development and nitrogen uptake.
2.1 Map
2.2 Combined Results
Simulation results for the combined datasets from the various countries are shown in the following graphs.  The model is able to adequately capture the influence of growing conditions (soil, climate) and management (population, nitrogen,  sowing date).
2.3 New Zealand
Model development
A range of experimental data has been collected by Plant and Food Research over a long period and was available for fitting thet growth and development parameters for the potato model.
ABlock
experiment was used for fitting leaf appearance and size parameters and biomass production and partitioning
Baxters
experiment was used for fitting branching parameters
FUE
experiment was used for fitting nitrogen responses
FUE_NI
experiment was used for fitting nitrogen response in winter grown potato
FSP
experiment was used for fitting growth and development parameters in cooler situations
Experiment Name
Design (Number of Treatments)
ABlock
Cultivar (8)
Baxters
PlantingDate x PlantDensity (6)
FUE
Nitrogen (4)
FSP
Nitrogen (4)
FUE_NI
Nitrogen (3)
This trial was run in A Block of the Plant and Food Research farm on a templeton silt loam, which is a deep, stone free soil of high plant available water capacity (160 mm/m).  8 cultivars of potato were planted representing a range of commercially important varieties in New Zealand:
Russet Burbank is a main crop variety grown for processing into fries
Jersey is a short variety grown for early table potato production
Red Rascal is a main crop table potato variety
Horizon is a main crop tabel variety
Ilam hardy is commonly grown as an early table variety
Karaka is a crisping variety
Nooksac  is a main crop variety grown for processing into fries
Rua is a main crop table potato variety
All potatoes were planted on 20/10/2003 and irrigated, fertiliser and pesticide were applied to ensure the crop remaind unstressed and healthy.  Each variety was intensively sampled throughout the growth period to provide data for developing cultivar parameters for different varieties.
Sowing date and Sowing Density Experiment
This trial was run in Baxters block of the Plant and Food Research farm on a shallow templeton silt loam, which had a compacted layer from 30-60 cm above a stony sub soil below 60cm.  Russet Burbank potatoes were planted on 3 dates:
14 October 1999
2 November 1999
23 November 1999
Two populations were established at each planting:
2 Plants/m
2
4 Plants/m
2
Fertiliser and pesticide were applied to ensure the crop remaind unstressed and healthy but irrigation was somewhat limited and did constrain growth at times.  Each treatment intensively sampled throughout the growth period to provide data for developing cultivar parameters for different varieties.  The same seed line was used for each sowing date so seed was becoming progressively more mature with leater plantings.  As such the stem number per plant increaesed from 3 - 5.7 from the early to the late sowing and and the final leaf number decreased.  The differences in canopy and production between the population treatments were very small becasue the low population treatment branched extensively.  This combination of different stem numbers and populations provided an excelent data set for parameterising branching.
Fertiliser Use Efficiency experiment (FUE)
This trial was run in A Block of the Plant and Food Research farm on a templeton silt loam, which is a deep, stone free soil of high plant available water capacity (160 mm/m).  Russet burbank potatos were planted on the 21st of October 1999 and irrigation and pesticide were applied to avoid stresses.  Different nitrogen fertiliser treatments were applied:
Nil
150 kg/ha applied at planting
300 kg/ha applied at planting
275 kg/ha in split application of 25kg/ha weekly
Fertiliser Use Efficiency experiment (FUE)
This trial was run in A Block of the Plant and Food Research farm on a templeton silt loam, which is a deep, stone free soil of high plant available water capacity (160 mm/m).  Russet burbank potatos were planted on the 21st of October 1999 and irrigation and pesticide were applied to avoid stresses.  Different nitrogen fertiliser treatments were applied:
Nil
150 kg/ha applied at planting
300 kg/ha applied at planting
275 kg/ha in split application of 25kg/ha weekly
2.4 Australia
Model development
A range of experimental data has been collected by SARDI at Parilla, Penola and Glenroy in South Australia.
Experiment Name
Design (Number of Treatments)
Parilla
PlantingDate x Nitrogen (9)
Penola_0607
Nitrogen (4)
Glenroy_0607
Nitrogen (4)
This experiment was conducted within a commercial potato field in the South Australia Murrylands region.  The fresh potato variety 'Coliban' was planted on three different dates (23/11, 3/12, 17/12) in 2007 on a free draining sandy soil.  Treatments recieved 10 mm of irrigation daily and 260 kg/ha of N as fertigation throughout the experiment.  In addition three rates of side dressed N (0, 100 and 200 kg) were applied during the season.
This experiment was conducted within a commercial field of 'Russet Burbank' potatos grown at Penola west in South Australia.  The paddock contained a Parapanic, Humic-Humosesquic, Semiaquic, Podsol (sandy loam) soil and the crop was sown on the 10/10/2006.  Nitrogen treatments were applied at different rates (0, 250, 500 and 750 kg/ha) in 8 split applications.
This experiment was conducted within a commercial field of 'Russet Burbank' potatos grown at Glenroy in South Australia.  The paddock contained a Melanic, Petrocalcic, Black Dermosol which had 30 cm of black clay over a compacted calcitrate sub soil.  The crop was sown on the 18/10/2005.  Nitrogen treatments were applied at different rates (0, 125, 250 and 500 kg/ha) in 8 split applications.
3 Interface
3.1 Potato
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
3.2 SowingParameters
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
3.3 Phenology
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
4 References
Brown, H.E, Huth, N, Holzworth, D., 2011. A potato model build using the APSIM Plant.NET framework., 961-967.
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Lawless, Conor, Semenov, MA, Jamieson, PD, 2005. A wheat canopy model linking leaf area and phenology. European Journal of Agronomy 22 (1), 19-32.
