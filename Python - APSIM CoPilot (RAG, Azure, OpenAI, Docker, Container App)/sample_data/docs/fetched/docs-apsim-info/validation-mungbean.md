# Mungbean documentation

Source URL: https://docs.apsim.info/validation/Mungbean
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:03.757363+00:00

1 The APSIM Mungbean Model
The APSIM Mungbean Model
The APSIM Mungbean model has been developed using the Plant Modelling Framework (PMF) of (
Brown et al., 2014
). This new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a model in much the same way that models can be coupled to construct a simulation. This means that dynamic composition of lower level process and organ classes (e.g. photosynthesis, leaf) into larger constructions (e.g. maize, wheat, sorghum) can be achieved by the model developer without additional coding.
The model consists of:
a phenology model to simulate development between growth phases
a structure model to simulate plant morphology
a collection of organs to simulate the various plant parts
an arbitrator to allocate resources (N, biomass) to the various plant organs
•While, when extensively calibrated to specific sites as it was in many papers, the APSIM classic mungbean model can accurately simulate a limited number of targeted scenarios, overall, it has a low level of simulation accuracy (RMSDYield = 470 kg ha-1, r2yield = 0.13; RMSDBiomass = 1823 kg ha-1, r2Biomass = 0.2). Therefore, the classic model has limited functionality and performance in forecasting analyses. Likewise, the classic model was built using old cultivars (Berken and King) which bear limited resemblance to the current popular cultivars (e.g., Jade-AU, Crystal, and Celera II-AU). We decided to source some parameter values from the classic model, but otherwise built the new model off of the template of the soybean APSIM NextGen model and Plant Modelling Framework. Parameter-wise, the following are the major ones that were changed:
oThere was an added phase for thermal time accumulation to delay emergence in the spring. The shoot lag was extended from 10 to 25GDD, but now days to emergence is more temperature dependent.
oThe base and optimal temperatures for vegetative thermal time were kept the same, but the optimal temperature window was extended to 43C and the max was at 45C, not 40C. This was to reflect the consistency in growth at the October, November, and January sowing dates. Reproductive had the same base, but a lower optimal window and max than Vegetative.
oThe node appearance rate was 100 before, but is now 70 (in line with Geetika’s work)
oThe k and RUE values are higher, sourced from Geetika’s work
oA water deficit stress induced senescence was added
oA photosynthesis respond to higher VPD was added
oRoot depth rate was changed to reflect the collected soil water extraction data
oWithout root:shoot biomass data, we kept the 0.2 biomass fraction to the root included in the soybean model which had extensive root data included in the model building dataset
oThe critical nitrogen concentrations are a smaller range with a lower max and higher min. This data was drawn from Muchow et al., 1993 paper on accumulation (see model documentation)
oThis is not a comprehensive list: We are sure that we are missing other items that were changed (likely the maximum fixation rate and the stem allocation fraction), but it was not always a simple adjustment to the parameters as the model framework itself has changed. The classic model required a good amount of further calibration by the model users in a way that we hope the new model won’t. That said, it is still customisable and we welcome any feedback about how it runs or what needs to be added or improved.
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
Berken
Berken
Emerald
Emerald
CeleraII
CeleraII
Crystal
Crystal
Jade
Jade
Onyx
Onyx
Opal
Opal
King
King
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
For Preemergence thermal time accumulation, a temperature dependent delay in emergence was drawn from Drs. Wenham's recent TOS trial.
For the Vegetative and Reproductive phase thermal time accumulation, the base, optimal, and maximum temperatures from the original generic legume model (
Robertson et al., 2002
)and a recent paper by Geetika Geetika (
Geetika et al., 2021
) were 7.5, 30, and 40C, respectively. The values were slightly adjusted to reflect the phenology recorded in Drs. Grant and Pasley's more recent trials.
Photoperiod sensitivity parameters were sourced from a study using the king cultivar (
Imrie et al., 1990
).
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
Simple leaf approach.
RUE value increased form that of Robertson to reflect higher production levels of more modern varieties in Dr. Kylie Wenham's time of sowing trials (
Muchow et al., 1993
,
Thomas, 2000
,
Geetika et al., 2021
).
The extinction coefficient (k) was sourced from (
Thomas, 2000
) and (
Geetika et al., 2021
).
The relative leaf area was sourced form (
Geetika et al., 2021
).
FVPD decreases at Fw=0.15: Mungbeans have been found to be particularly sensitive to variability in the vapour pressure deficit (VPD), potentially driving their reduced biomass accumulation and yield when sown in the spring in Dr. Kylie Wenham.
The Phyllochron and branching rates were sourced from trials conducted by Dr. Pasley included in the model testing and from (
Geetika et al., 2021
).
Specific area and area largest leaf were sources from the APSIM NextGen soybean model.
N concentrations were sourced form (
Muchow et al., 1993
).
There are two drivers of leaf senescence included in the model: age-driven and water stress-driven. The dominant driver of senescence is the one with the highest rate. Age-driven senescence of the leaves occurs at a constant rate from the end of pod development to ripening. Meanwhile, water stress-driven senescence rate increases with water-deficit stress as quantified by Fw ≤ 0.15, starting at the end of canopy development and continuing to ripening. The water-stress driven senescence rate was derived using data from (
Thomas, 2000
)
1.4.4 Grain
This organ uses a generic model for plant reproductive components.  Yield is calculated from its components in terms of organ number and size (for example, grain number and grain size).
Muchow and Charles-Edwards 1982 found that the fraction proportioned to the grain is 0.17-0.31 (
Muchow et al., 1993
). The potential harvest index was estimated to be slightly higher than these values.
N concentrations were sourced form (
Muchow et al., 1993
).
1.4.5 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
Root front velocity values were sourced from Thomas Dissertation that includes 3 experiments that look at different irrigation regimes/timings/cultivars and compares mungbean to soybean.
These experiments were used to build the original generic legume model (
Robertson et al., 2002
,
Thomas, 2000
).
All other root parameters were sourced from the soybean APSIM NextGen model.
1.4.6 Nodule
This organ simulates the root structure associate with symbiotic N fixing bacteria.  It provides the core functions of determining
N fixation supply and related costs.  It also calculates the growth, senescence and detachment of nodules.
The maximum fixation rate (35 g g-1) was sourced from (
Herridge et al., 2005
) and the daily potential fixation rates were sourced from the soybean model for APSIM NextGen. The accuracy of these parameter values was tested via time-series N fixation from the DAF ACRI experiment in the validation dataset.
1.4.7 Shell
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Muchow and Charles-Edwards 1982 found that the fraction proportioned to the pods is 0.63-0.68 (
Muchow et al., 1993
).
1.4.8 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Values for maximum, critical, and minimum N concentrations sourced from Muchow et al., 1993 (
Muchow et al., 1993
).
Values sourced from graphing leaf vs. stem biomass data from Dr. Kylie Wenham's Time of Sowing trial and Thomas' dissertation (
Thomas, 2000
).
1.4.9 MortalityRate
A constant function (name=value)
1.4.10 SeedMortalityRate
A constant function (name=value)
2 Validation
2.1 Map
Additional small experiment at Gatton UQ field station on a vertisol. Sown on October 26, 2021. Node, leaf, and branch numbers were counted every week. After the 4th node, biomass samples were taken weekly too and partitioned into leaves and stems. Harvest data was not collected as the aim of the experiment was to nail down phenology.
2.2 Thomas Dissertation
Thomas Dissertation includes 3 experiments that look at different irrigation regimes/timings/cultivars and compares mungbean to soybean.
These experiments were used to build the original generic legume model (
Robertson et al., 2002
,
Thomas, 2000
).
Experiment Name
Design (Number of Treatments)
Experiment_1_Berken
Water (3)
Experiment_1_Emerald
Water (3)
Experiment_2
Water (6)
Experiment_3
_Sow x Cultivar (6)
Experiment_4
_Water x Cultivar (3)
Experiment at Gatton, QLD, sown on November 28 1996 on CSIRO Cooper field station. Soil type was a deep alluvial, weakly-cracking vertisol. There were 3 treatments: 1) well-watered, 2) rainfed and 3) rainfed+irrigation during pod filling. The cultivar used was Berken.
Experiment at Gatton, QLD, sown on November 28 1996 on CSIRO Cooper field station. Soil type was a deep alluvial, weakly-cracking vertisol. There were 3 treatments: 1) well-watered, 2) rainfed and 3) rainfed+irrigation during pod filling. The cultivar used was Emerald.
Experiment at Gatton, QLD, sown on January 7 1997 on CSIRO Cooper field station. Soil type was a deep alluvial, weakly-cracking vertisol. There were 3 treatments: 1) well-watered, 2) rainfed and 3) terminal water stress with a rainout shelter. The cultivars used were Berken and Emerald.
Experiment at Gatton, QLD, CSIRO Cooper field station. Soil type was a deep alluvial, weakly-cracking vertisol. There were 3 treatments: 1) well-watered, 2) rainfed and 3) stress with a rainout shelter for 4 weeks starting at different stages of mungbean growth imposed by having 3 sowing dates (3, 17, 30 December 1997). The cultivar used was Emerald.
Experiment at Gatton, QLD, CSIRO Cooper field station. Soil type was a deep alluvial, weakly-cracking vertisol. There were 3 treatments: 1) well-watered, 2) rainfed and 3) rainfed+irrigated for first 3 weeks and at podfilling. The cultivar used was Emerald.
2.3 TOS
These trials took place in Gatton on a vertisol at the Gilbert Site 2018-2021 looking at 6 cultivars (Jade, Crystal, CeleraII, Opal, Onyx, Berken) planted at 5-6 times in rainfed and irrigated (one year). The data has not been published yet, but was collected and will be published by Kylie Wenham. Berken was likely not a pure seed and cannot be compared to the results of Thomas' dissertation and so was removed from the model-building dataset. The first and second years of the experiment were entirely or patrially excluded from the model dataset because the data was concluded to be unreliable.
Experiment Name
Design (Number of Treatments)
TOSyear2Opal
Sow x Cv (2)
TOSyear3
Sow x Cv (25)
2.4 Muchow
This dataset (
Muchow et al., 1993
) includes the cultvar "King" grown under irrigated conditions in a subtropical environment at the Gatton Research Station in southeastern Queensland, Australia and Kimberly, NT, Australia.  Plants were sampled almost weekly for biomass partitioning, nitrogen and plant development.
Experiment Name
Design (Number of Treatments)
Kimberly1980
Dens (3)
2.5 Mungbean_DAF
These trials were done by DAF in 2019 and 2020, largely looking at the impact of applying N and innoculating seeds. Data is recorded in annual reports but not published. Jade was the cultivar used on all. It is unclear why the recorded harvest date is so much later than the simulated as the simulated was confirmed within 10 days by the Thomas Dissertation and Dr. Wenham's TOS trials. Perhaps, like the Yield Gap studies, the crop was harvested much later than the earliest harvest date possible or else, as was see in one experiment in the Thomas Dissertation, the crop was semi-indeterminant and didn't reach a natural harvest ripe point when anticipated (the crop keeps growing if there is enough water) and so the date of the harvest recorded in the field corresponds to a date at which the crop was chemically or mechanically desiccated.
Experiment Name
Design (Number of Treatments)
ACRI
N (2)
Emerald2020
N (2)
HopelandSpring
N (2)
HopelandSummer
N (2)
ACRI trial in Narrabri with cultivars Opal and Jade. Detailed soil water measurements. Was supposed to be an N fertilizer trial, but not a priority in the model so we just looked at 0N rainfed.
DAF trial in Emerald, QLD with cultivar Jade. Was supposed to be an N fertilizer trial, but not a priority in the model so we just looked at 0N.
DAF trial in Hopeland, QLD with cultivar Jade planted in Spring. Was supposed to be an N fertilizer trial, but not a priority in the model so we just looked at 0N.
DAF trial in Hopeland, QLD with cultivar Jade planted in Summer. Was supposed to be an N fertilizer trial, but not a priority in the model so we just looked at 0N.
DAF trial on the Liverpool plains at the Liverpool Plains Research Station with cultivar Jade. Was supposed to be an N fertilizer trial, but not a priority in the model so we just looked at 0N.
DAF trial in Emerald, QLD with cultivar Jade. Was supposed to be an N fertilizer trial, but not a priority in the model so we just looked at 0N.
DAF trial in Irvingdale, QLD with cultivar Jade planted. Was supposed to be an N fertilizer trial, but not a priority in the model so we just looked at 0N.
2.6 Yield_Gap
CSIRO large-scale, primarily single year field studies that were looking to see what the leading yield limiters were. The data was organized by Brook Anderson (CSIRO) and is being written up by Marisa Collins (LaTrobe) and Lindsay Bell (CSIRO). The cultivar used was Jade with the exception of one site year where they used Crystal. Total biomass and days to maturity data were removed from the model-building dataset as well as a few treatments from the Pampas site after discussion with Lindsay Bell about the unreliability of this data. In some of the studies, the harvesting date in the field is much later than the simulated dates. These discrepancies are for the same reason the biomass and days to maturity data was removed: the crop was left in the ground for later than its maturity or earliest harvesting date, so the harvest date recorded by the field technicians are for that later date.
Experiment Name
Design (Number of Treatments)
Billa_Billa
Sow (3)
Emerald
Sow (6)
HRS
Sow (3)
Pampas
Sow (5)
Warra
Sow (2)
3 Sensibility
A factorial simulation of different sowing times by initial soil water level at three sites was run for 55 years, from 1963 to 2018, resetting the soil organic matter, N, and water on July 1 of each year. The baseline simulations for Gatton, Katherine, and Narrabri (-30.2 S, 147.6 E) were sourced from the calibration/testing dataset. Jade-AU was sown at early (September 15), mid (December 15), and late (February 15) in the growing season with initial soil water profile at 20%, 60%, and 100% of full representing a low, mid, high water status, respectively. The site descriptions of the three sites can be categorised according to their soils and climate: Gatton is a vertisol (195mm plant available water when the profile is full), with a temperate climate where the hot temperatures (20-30°C) ideal for mungbeans occur during a short window (early sowing results in potential heat stress at flowering and grain fill, late sowing results in low temperatures hindering grain fill) and so is termed in this study as “Short Season Temperate Vertisol.” Narrabri is also a vertisol (321mm plant available water when the profile is full), with a long hot (20-30°C) and dry growing season for mungbeans and so, is termed “Long Season Arid Vertisol.” Katherine soil is an alfisol (195mm plant available water when the profile is full), with very hot (30-35°C) conditions where average monthly rainfall only exceeded 40mm starting in November, suggesting that mungbeans need to be planted later there, and so, is termed “Late Season Arid Alfisol.” Probability analyses were then run on the resulting grain yields to test if the model outputs changed in a sensible way relative to the environment and growing conditions and in their response to more extreme environments. We expect the model to output a greater sensitivity to sowing date irrespective of water conditions in a “Short Season Temperate Vertisol,” to water conditions irrespective of sowing date in a “Long Season Arid Vertisol,” and to not be able to grow mungbeans at all at the early sowing date and when there is little initial water in the “Late Season Arid Alfisol.”
Experiment Name
Design (Number of Treatments)
Gatton
Water x Sow (9)
Katherine
Water x Sow (9)
Narrabri
Water x Sow (9)
4 Interface
4.1 Mungbean
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
5 Media
6 References
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Geetika, G, Hammer, G, Smith, Millicent, Singh, V, Collins M, Mellor, V, Rachaputi RCN, 2021. Quantifying physiological determinants of potential yield in mungbean (Vigna raiata (L.) Wilczek. Field Crops Research.
Herridge, D F, Robertson, M J, Cocks, B, Peoples, M B, Holland, J F, Heuke, L, 2005. Low nodulation and nitrogen fixation of mungbean reduce biomass and grain yields. Australian Journal of Experimental Agriculture 45 (3), 269-277.
Imrie, B C, Lawn, R J, 1990. Time to flowering of mung bean (Vigna Radiatia) genotypes and their hybrids in response to photoperiod and temperature. Experimental Agriculture 26 (1), 307-318.
Muchow, R C, Robertson, M J, Pengelly, B C, 1993. Accumulation and partitioning of biomass and nitrogen by soybean, mungbean and cowpea under contrasting environmental conditions. Field Crops Research 33 (1), 13-36.
Muchow, R C, Robertson, M J, Pengelly, B C, 1993. Radiation-use efficiency of soybean, mungbean and cowpea under different environmental conditions. Field Crops Research 32 (1), 1-16.
Muchow, R.C., Robertson, M.J., Pengelly, B.C., 1993. Accumulation and partitioning of biomass and nitrogen by soybean, mungbean and cowpea under contrasting environmental conditions. Field Crop Research 33, 13–36.
Robertson, M. J., Carberry, P. S., Huth, N. I., Turpin, J. E., Probert, M. E., Poulton, P. L., Bell, M., Wright, G. C., Yeates, S. J., Brinsmead, R. B., 2002. Simulation of growth and development of diverse legume species in APSIM. Australian Journal of Agricultural Research 53 (4), 429-446.
Thomas, 2000. Grain and nitrogen accumulation by mungbean under various water supply conditions., 274.
