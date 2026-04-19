# SCRUM documentation

Source URL: https://docs.apsim.info/validation/SCRUM
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:38.237444+00:00

1 The APSIM SCRUM Model
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
SCRUM: the Simple Crop Resource Uptake Model
Hamish Brown, Edith Khaembah, Rogerio Cichota, Xiumei Yang and Jo Sharp, New Zealand Institute for Bioeconomy Science
Neil Huth and Dean Holzworth, Commonwealth Scientific and Industrial Research Organisation
Steven Reeves, Queensland Department of the Environment, Tourism, Science and Innovation
The first version of SCRUM was released in APSIM in December 2015 to provide a simple model for simulating a wide range of annual crops where water and nitrogen balance are of interest but a fully mechanistic plant model is not needed or is not available. It was a daily time step implementation of the crop model that is used in the Overseer nutrient balance model
Cichota et al., 2010
and was parameterised for over 50 different crop types.  However, a number of limitations had been identified with the model and the crop parameterisations were location and sowing time specific so it did not achieve wide spread use.  An updated version was put into release in June 2025 which retains the same underlying concepts but has a different approach for modelling phenology, making it easy to parameterise the model for any situation.  This makes it relevant for modelling any annual crop at any point on the globe providing the user setting the crop parameters has a good understanding of the crops behaviour in the situations being simulated.
Note:  The updated version is different to the first version in its underlying model structure, the way it is parameterised and the way in integrates into simulations.  As such it is not possible to maintain backward compatibility of simulations configured prior to June 2025.  Unfortunately, users will need to update these simulations manually be removing the old version of SCRUM for simulations and replacing it with the new version by following the instructions below.
The SCRUM model serves two main purposes:
To represent annual crops in APSIM simulations that we do not yet have a detailed model for and/or lack the experimental data required to parametrise one.  SCRUM has been designed to be parameterised with some basic crop information that an expert in a specific crop type will have close to hand.  This provides an alternative to using another crop model as a proxy when we are simulating rotations that include crops with no detailed crop model.
For studies that are not primarily interested in the crop and don’t want to invest a lot of time in setting the crop component of simulations but still need something to provide sensible boundaries to the nitrogen and water balance.
To be able to represent a broad range of annual crops, SCRUM makes a number of simplifications:
It does not predict potential yield.  This is a user input and a value must be provided that is sensible for the context being simulated.  Harvest index and root proportion coefficients are used to determine stover and root biomass.  Crop yields may be less than the yield specified if stress events occur.
Daily patterns of biomass accumulation, cover, root depth and height are calculated from basic linear and sigmoidal functions anchored to phenological stages as shown below.
The model does not predict when harvests occur.  Instead, the user provides dates and crop stages for establishment and harvest as inputs.  Scrum scales its development patterns (shown above) between the user specified dates and stages to give daily patterns.  An example for biomass accumulation patterns is shown in the figure below.  Dotted, dashed and solid lines represent seedling, emergence and seed establishment stages respectively.   All of the patterns displayed are possible depending on which stage is specified for establishment and harvest. So  a wide range of development patterns can be simulated by changing the establishment and harvest dates and stages to represent different situations.
Establishment Stages:
Seed
is selected to represent a crop that is sown and emerges after establishment date.
Emergence
is selected if the crop is to start growing on the day of establishment
Seedling
is selected to represent transplanted crops that already have some biomass and leaf area on the day of establishment.
Harvest Stages:
Vegetative
is selected for crops such as lettuce that are harvested while still in rapid growth.
Early
,
Mid
and
Late Reproductive
are other options for crops that are harvested before maturity such as whole crop silage, fresh legumes and oil seeds (respectively)
Maturity
is selected for a crop that is harvested as soon as biomass accumulation has finished
Ripe
is selected for crops that are harvested after maturity and a period of field drying as for most cereals.
Water stress responses may be switched on or off by the user depending on the simulation needs.  When water stress is switched on it will affect crop cover and biomass accumulation.
Nitrogen stress reduces biomass accumulation if there is insufficient N to meat minimum N concentration of growing organs.  Nitrogen stress does not affect cover.
SCRUM is not released with sets of coefficients to represent different crop types.  Instead a ScrumCropInstance model is included in a simulation where the user sets coefficients to represent the crop required.  This provides great flexibility for parameterising crops but puts the responsibility for having sensible parameters on the model user.  The name of a ScrumCropInstance is arbitrary and you may include as many as are needed to provide all the crops for a simulation.
SCRUM is not established with a .Sow() command the same as other crops.  Instead, it is established with an .Establish() command that includes harvest date, expected yield and other data (see below for more detail).
There is a SCRUM rotation component that can be used for specifying rotations of SCRUM crops without needing to use a manager.  It is possible to trigger these events from manager if more sophisticated set ups are required.
The SCRUM model is built using the Plant Modelling Framework (PMF) of
Brown et al., 2014
so it interfaces with other APSIM models in the same way as other plant models.
SCRUM has 4 organ classes to represent different biomass components. The real biomass components that these classes represent change from crop to crop:
A Simple leaf class called
Stover
which represents the unharvested parts of the plant.  Generally, this represents the leaf and stem components of the crop but for crops where stem and leaf are part of the harvested product (e.g forages and leafy vegetables) than stover is the residual fraction of leaf and stem that is not harvested.
A Generic organ class called
Product
which represents the plant parts that are harvested and removed from the field.  This could represent grain, fruits, tubers, leaf or stem depending on what sort of crop is being represented.
A Root organ which extracts water and nitrogen from the soil for plant growth and returns biomass to the soil on harvest
A Nodule organ which is only activated and fixes nitrogen for the legume crops.
Using SCRUM in simulations.
Some working examples and a number of parameterised ScrumCropInstances are provided in the SCRUM simulation in Examples.  These are meant to provide a start point and users should carefully review these parameterisations and adapt them to best represent the crops being simulated.
To use SCRUM in a simulation you must add the SCRUM model, at least one ScrumCropInstance and a manager model to establish the crop.  The parameters in the ScrumCropInstance are:
Harvest Index - The proportion of total biomass that is in the harvested product (0-1).
Product Moisture Content - The proportion of the harvested yield that is moisture (g/g).  A value of zero is used if input yields are in dry weight and a value between zero and 1 is used if the input yields include some moisture.  A value of up to 0.95 is possible for crops like lettice that have very high moisture contents.
Root biomass proportion - The proportion of total biomass that is in the roots (0-1).  The sum of the root proportion and harvest index must be less than 1.
Root depth at maturity (mm) - SCRUM grows roots from sowing depth to Root Depth at maturity so an appropriate value for the mature crop must be specified.  This will interact with soil limitations (XF) so roots may not reach the depth specified.  Also note that roots don't reach mature depth until the EarlyReproductive stage so crops harvested at the Vegetative stage will not reach the root depth specified.
Crop height at maturity (mm)- is the ultimate hight of the crop.
Maximum green cover (0-1) is the highest value of cover that the crop would be expected to achieve depending on row spacing and crop width.  Note that cover will be reduced in response to water stress so may not reach the values specified.
Extinction coefficient (0.4-1) is a parameter that describes how quickly cover increases with increased LAI.  A value of 1.0 represents a prostrate leaved crop and this decreases for crops with more erect leaves.
Stage for Nconc parameters.  Nconc declines as the crop develops so a crop harvested at the vegetative stage (e.g. forage cereals) would have higher Nconc than the same crop if it were taken to maturity.  The Nconc parameters for product and stover (below) represent the value at the development stage selected here.  The model then scales them up or down for a given simulation if the harvest stage used differs to this stage.  This allow us to achieve sensible Nconc at harvest with a single ScrumCropInstance when harvesting at different stages.
Nitrogen concentration at seedling stage (g/g).  At an early stage a crop is predominantly leaf and an Nconc of 0.05 is typical for most plants.  Nconc declines from this value at the start of simulations to lower values at harvest.
Nitrogen concentration of product at harvest (g/g).  This is the nitrogen concentration of the harvested product at the stage specified for Nconc parameters.  The model may grow to Nconcs higher or lower than this if it is harvested before or after the specified Stage for Nconc parameters.
Nitrogen concentration of Stover at harvest (g/g).  This is the nitrogen concentration of any above ground biomass not removed by harvest, at the stage specified for Nconc parameters.  The model may grow to Nconcs higher or lower than this if it is harvested before or after the stage specified for Nconc parameters.
Nitrogen concentration of Roots (g/g).  As the name infers.
Base, Optimum and Maximum temperatures (
o
C) are specified for the accumulation of thermal time.  Although harvest dates are provided as a user input, patterns of biomass accumulation, cover, root depth and height are all driven by thermal time so it is important that appropriate values are specified so seasonal growth and development patterns are sensible.
Thermal Time required from sowing to emergence.  If the crop is established at the seed stage this is how much thermal time it will take to emerge.
Maximum Canopy Conductance - This controls crop transpiration demand.
Net radiation at 50% of maximum conductance - This also controls transpiration demand and how it responds to radiation levels.
Does the crop respond to water stress?  If checked biomass and cover accumulation will decrease if water stress occurs.
The SCRUM crop instance sets the basic parameters for a Scrum crop.  Additional parameters must also be sent when the crop is established.  To establish a SCRUM crop a manager model must be included that links to the appropriate ScrumCropInstance as shown below:
ScrumCropInstance SCRUMCrop = zone.FindDescendant
(CropName);
It must then create a ScrumManagementInstance that contains the SCRUM management parameters:
ScrumManagementInstance management = new ScrumManagementInstance(
cropName: "SCRUMCereal",
establishDate: clock.Today,
establishStage: "Seed",
plantingDepth: 20,
harvestStage: "Ripe",
expectedYield: 1000,
harvestDate: "23/4/2021,
ttEstablishmentToHarvest: "",
fieldLoss: 0.05,
residueRemoval: 0,
residueIncorporation: 0,
residueIncorporationDepth: 0
);
the meaning of the management parameters is:
cropName: Name corresponding exactly to the name of the ScrumCropInstance to be established
establishDate: The date the crop is established on
establishStage: The development stage at the time of establishment
plantingDepth: in mm,
harvestStage: The stage the crop is harvested at,
expectedYield: The yield expected if the crop does not encounter nitrogen or water stress.
harvestDate: The date the crop is harvested (This is not needed if ttEstablishmentToHarvest is specified)
ttEstablishmentToHarvest: Thermal time from establishment to harvest (This is not needed if harvest date specified)
fieldLoss: (0-1) The proportion of gross product yield that is not harvested and remains in the field as residues.  For perfect harvesting this would be 0 and if the entire crop goes rotten this would be 1.
residueRemoval: (0-1) The proportion of stover biomass that is removed from the field at harvest (e.g if straw is bailed)
residueIncorporation: (0-1) The proportion of residues that are incorporated at harvest.  0 unless some form of cultivation of soil disturbance burries residues on day of harvest.
residueIncorporationDepth: (mm) Depth to incorporate residue to.
The manager scrip must then call the ScrumCropInstances Establish method and pass in the management class set up above
SCRUMCrop.Establish(management)
The model is constructed from the following list of software components. Details of the implementation and model parameterisation are provided in the following sections.
1.1 Plant Model Components
Component Name
Component Type
Phenology
Models.PMF.Phen.Phenology
Arbitrator
Models.PMF.OrganArbitrator
Product
Models.PMF.Organs.GenericOrgan
Stover
Models.PMF.Organs.SimpleLeaf
Root
Models.PMF.Organs.Root
Nodule
Models.PMF.Organs.Nodule
MortalityRate
Models.Functions.Constant
SeedMortalityRate
Models.Functions.Constant
LegumeFactor
Models.Functions.Constant
AbovegroundDMAtHarvest
Models.Functions.DivideFunction
TotalDMAtHarvest
Models.Functions.AddFunction
MaxNConcAtSeedling
Models.Functions.Constant
NDilutionConstant
Models.Functions.Constant
1.2 Composite Biomass
Component Name
Component Type
Total
Models.PMF.CompositeBiomass
AboveGround
Models.PMF.CompositeBiomass
1.3 Child Components
1.3.1 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
Phenological phases for SCRUM do not correspond to objective development stages as they do for detailed crop models.  Instead they represent subjective stages in the development of an annual crop when they may be established or harvested and so provide weigh points for scalling biomass, cover, root depth and height between.  As ThermalTime accumulates the crop progresses through the following phases:
1.3.2 Arbitrator
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
1.3.3 Product
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
This organ represents the biomass that is typically removed from the crop at harvest. It may include grain, root, leaf, stem, pod, tuber, or any other organ depending on the type of crop and how it is harvested.
1.3.4 Stover
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
This organ represents the biomass that is typically not removed at harvest. It may include leaf, stem, or any other organ depending on the type of crop and how it is harvested. The processes defining biomass accumulation over time (akin to photosythesis) are accounted for in this organ. It also holds the information about the canopy (cover, height, etc.).
1.3.5 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
This organ represents the biomass below ground that is typically not removed at harvest. The processes related to water and nitrogen uptake are handled through this organ, which also contains the information about the distribution or roots over the soil profile.
The SCRUM model assumes that a fixed proportion of biomass is allocated to roots as the crop grows, and that it has a fixed N concentration. No retranslocation for either biomass or N is enabled from roots.
The roots grow over time is controlled by thermal time, reaching is maximum(MaximumRootDepth) at the end of the EarlyReproductive phase.
1.3.6 Nodule
This organ simulates the root structure associate with symbiotic N fixing bacteria.  It provides the core functions of determining
N fixation supply and related costs.  It also calculates the growth, senescence and detachment of nodules.
This is parameterised as a dummy organ, it can supply nitrogen if the plant is a legume, but has no biomass (DM or N) nor any other function. The organ is 'activated' using a switch, which should be turned on for legume crops (so N fixation can be added as supply to the Arbitrator to help covering the crop N demand). To turn this feature on, the value for LegumeFactor should be set to a non-zero value, this parameter represents the proportion of the potential N fixation rate that the nodules are capable of supplying. The potential fixation is linked to the daily fixation and controlled by the MaximumFixation parameter. Nitrogen fixation is switched off by setting LegumeFactor to zero.
1.3.7 MortalityRate
A constant function (name=value)
1.3.8 SeedMortalityRate
A constant function (name=value)
1.3.9 LegumeFactor
A constant function (name=value)
This parameter controls the potential N fixation for the crop. It varies from zero, no fixation, to one when fixation is at its maximum.
SCRUM gets the value for this parameter during run-time, when the crop is established. The value is set on ScrumCropInstance or CropManagement script.
1.3.10 AbovegroundDMAtHarvest
A class that divides all child functions.
Returns zero if nominator is zero, returns double.maxValue if denominator is zero.
1.3.11 TotalDMAtHarvest
A class that returns the sum of its child functions.
1.3.12 MaxNConcAtSeedling
A constant function (name=value)
This represents the maximum N concentration for Stover and Product at Seedling stage. SCRUM assumes that the value for naximum N concentration decreases as plants grow, strating with the value given here and reaching values specific for each organ at Maturity.
SCRUM gets the value for this parameter during run-time, when the crop is established. The value is set on ScrumCropInstance or CropManagement script.
1.3.13 NDilutionConstant
A constant function (name=value)
This parameters controls how the maximum N concentration for Stover and Product decrease over time. Follows a exponential decrease from MaxNConcAtSeedling.
SCRUM gets the value for this parameter during run-time, when the crop is established. The value is set on ScrumCropInstance or CropManagement script.
2 Sensibility tests
Experiment Name
Design (Number of Treatments)
StageTest
EstabAt x HarvAt (18)
ForageVsSeed
HarvestStage (2)
TypicalHarvestStage
HarvestStage (2)
EstablishmentMethodTest
EstablishAt x HarvestAt x _EstablishAt1 (3)
ResouceLimitationTest
_Irr x _Nit (4)
DefoliationTest
Cuts (5)
Aim of this simulation
Test the different stages at which SCRUM can be establish and check the variations in biomasss accumulation, as well as canopy cover and height.
There are three possible stages at which SCRUM can be established, Seed, Emergence, and Seedling. If established at Seed stage, SCRUM will take some time to start growing (simulating germination), wich is a function of thermal time and soil moisture. If stablished at Emergence, SCRUM will grow straigthaway, starting with minimum biomass and zeo cover, whereas, if is established at Seedling, SCRUM will start with greater biomass and already some cover. This implies that growth starts earlier for crops stablished at Seedling, then for those at Emergence, and will take longer to start for Seed. However, the harvest time does not change automatically due to change in establishment stage.
SCRUM defines its growing period from establishment till harvest (either by the user supplying dates or thermal time). All the sigmoid functions used to compute biomass accumulation, canopy cover, and height are scaled so to compute valid values for that period. Thus, when the establishment stage is changed, say from Seed to Seedling, and no change is made to the harvest time, those functions are 'stretched' so that the crop growth time is the same, even though the crop established at Seedling has a head-start at the beggining. If this is not desired, the duration of crops has to be adjusted by the user, e.g. by changing the harvest time.
This simulation is used to demostrate how the shape of the funtions describing crop development change as different phenological stages are used for establishment. Also, included are alternative ways to set up the model so that the crop development is not affected much by the change in establishment stage.
Aim of this simulation
Test the variation over time of canopy cover (and height) of SCRUM following defoliations.
Defoliations are possible in SCRUM, either set using a BiomassRemovalEvent (SCRUM_Cut in ths simulation), or any other manager with the appropriate code...
Resetting phenology should be avoided when doing biomass removal as this causes problems with cover predictions.
3 Interface
3.1 SCRUM
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
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Cichota, R., Brown, H., Snow, V. O., Wheeler, D. M., Hedderley, D., Zyskowski, R., Thomas, S., 2010. A nitrogen balance model for environmental accountability in cropping systems. New Zealand Journal of Crop and Horticultural Science 38 (3), 189-207.
