# SPRUM documentation

Source URL: https://docs.apsim.info/validation/SPRUM
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:53:16.896017+00:00

1 The APSIM SPRUM Model
A few validation sets have been included to ensure predictions from SPRUM are in a sensible range and to provide a check point for performance testing.  However, no effort has been put into optimising the coefficients used in the SprumPastureInstance models for these tests so they should not be taken as a set of validated coefficients that are suitiable to represent the associated pasture types.  Further optimisation of the coefficients is planned and this note will be removed once that is done.  However the model remains useful regardless so has been put into release for general use.  The validation tests established here show a few issues that need to be resolved:
Need to make water stress sensitivity a variable so it can differ between species. Currently it is just on or off
Need to make inititial cover settable at the time or grazing so it can vary through the season.  Currently it is constant
Need to be able to vary the patter of kl over depth for different species.  Currenlty constant.
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
SPRUM: the Simple Pasture Resource Uptake Model
Hamish Brown, Rogerio Cichota and Edith Khaembah, New Zealand Institute for Bio-economy Science
The SPRUM model is built to the same principals as the SCRUM and STRUM models and can be used in similar context:
It provides a model that can be quickly parameterised to represent pasture species of differing traits where a more detailed model is currently lacking.  SPRUM has been designed to be parameterised with some basic pasture information that an expert would have close to hand.
For studies that are not primarily interested in the pasture and don’t want to invest a lot of time in setting the pasture components of simulations, but still need something to provide sensible boundaries to the nitrogen and water balance.  The SPRUM model can be included and work sensibly in a simulation without need to include additional manager modules to control it which helps users who are non-expert in simulating pasture management to include realistic pasture components in simulations.
SPRUM represents key functions of pasture as follows.  Phenology is represented by two phases: a Regrowth phase where cover increases from the 'Green Cover post grazing' to the 'Maximum Green Cover' (both specified by the user) between 'Start Regrowth' and 'Full Canopy' stages.  This is followed by a 'Full Canopy' Phase when cover remains at the maximum value.  Radiation interception is estimated using a sigmoid function that is scaled horizontally between 'Start Regrowth' and 'Full Canopy' stages and vertically between initial and maximum covers.  Different patterns of canopy recovery can be achieved by changing the initial cover and 'Regrowth duration' (in oCd) of the Regrowth phase as shown in the figure below.  Further variation in the pattern of canopy cover can be achieved by changing the length of the Full Canopy phase when cover remains at the maximum value.  Potential biomass accumulation is modelled using a radiation use efficiency which the user adjusts to achieve the expected pasture growth rates.  Users may also specify the nitrogen fixation capacity of the pasture to represent different legume activities.
Water stress responses may be switched on or off by the user depending on the simulation needs. When water stress is switched on it will affect crop cover and biomass accumulation.
Nitrogen stress reduces biomass accumulation if there is insufficient N to meet minimum N concentration of growing organs. Nitrogen stress does not affect cover.
SPRUM is not released with sets of coefficients to represent different pastures types. Instead a SprumPastureInstance model is included in a simulation where the user sets coefficients to represent the pasture required. This provides great flexibility for parameterising pastures but puts the responsibility for having sensible parameters on the model user. The name of a SprumPastureInstance is arbitrary and you may include as many as are needed to provide all the pastures for a simulation.
The SPRUM model is built using the Plant Modelling Framework (PMF) of Brown et al., 2014 so it interfaces with other APSIM models in the same way as other plant models. SPRUM has 4 organ classes to represent different biomass components:
A Simple leaf class called Leaf which represents the harvested parts of the pasture. Generally, this represents the leaf and stem components in different proportions.  SPRUM makes on consideration of the change in the composition of the "Leaf" component over time and how this would influence feed quality.  The proportion of total biomass partitioned to the leaf is 1 - RootProportion - ResidueProportion.  No senescence is modelled so leaf biomass is only reduced by BiomassRemovalEvents.
A Generic organ class called residual which represents the plant parts that are not harvested and removed from the field. This could represent seed heads, stem and leaf that are not removed.  The amount of biomass partitioned to the residual and its rate of senescence is set by the user to ensure residual biomass accumulates and turns over at appropriate rates.
A Root organ which extracts water and nitrogen from the soil for plant growth and returns biomass to the soil on harvest.  The user specifies the total biomass is partitioned to the root and senescence is modelled as a function of temperature so realistic patterns of root biomass accumulation and degradation are simulated.
A Nodule organ which is only activated and fixes nitrogen for the legume crops.
Including SPRUM in a simulation
Some working examples and a few parameterised SprumPastureInstances are provided in the SPRUM simulation in Examples. These are meant to provide a start point and users should carefully review these parameterisations and adapt them to best represent the pastures they want to simulate.
To use SPRUM in a simulation you must add the SPRUM model, at least one SprumPastureInstance and a BiomassRemovalEvents. The parameters in the SprumPastureInstance are:
Establishment Date - the date the SprumPastureInstance will be established in the simulation.  If left blank it will be established on the first day of the simulation.
Age At Start of Simulation - in Years.  If the simulation is to be started with an established pasture, this parameter should be set to a higher value that 'Years from establishment to reach Maximum root depth'.  If seedling this should be zero.  Age dimension parameters are calculated relative age between establishment and years from establishment to reach Maximum root depth.  These are used to scale pasture cover and root depth while the pasture is establishing.  Canopy will reach its maximum dimension 4 times faster than the roots reach their maximum depth
Years from establishment to reach Maximum root depth - in years - this parameter determines how long it takes the pasture to reach its maximum root depth
Maximum growth rate of pasture (g/MJ) - This is the radiation use efficiency of the pasture and can be altered to achieve the productivity required for the pasture in the simulations.  Typical values for this parameter are 0.5 - 2.0 g/MJ.
Residue biomass proportion (0-0.5) is the proportion of total biomass partitioned to the residue component
Residue senescence rate (0-1) is the proportion of residue biomass that senesses and is passed to surface organic matter (where it may decompose) each day.
Root Biomass proportion (0-0.5) is the proportion of total biomass that is partitioned to the roots
Base, optima and maximum temperatures for photosynthesis (oC) are used to set temperature response profile for radiation use efficiency and subsequent biomass production.
Grow roots into neighbouring zone (yes or no) determines if this models roots grow into a neighbouring zone for multi zone simulations
Root depth (mm) is the maximum rooting depth of the pasture when it is established
Pasture height at and after grazing (mm)" set the height range that the pasture will grow between Start Regrowth and FullCanopy.
Maximum and minimum green cover (0-0.97) set the cover range that the pasture will grow between Start Regrowth and FullCanopy.
Extinction coefficient (0.4-1) is a parameter that describes how quickly cover increases with increased LAI. A value of 1.0 represents a prostrate leaved pasture and this decreases for pastures with more erect leaves.
Regrowth duration  (oCd) is the thermal time it takes for the pasture to go from the 'Start Regrowth' to 'Full Canopy' stages
Full Canopy duration  (oCd) is the thermal time that the pasture will spend at full canopy before "Grazing". Note that SPRUM "Grazes" its self buy triggering a biomass removal event and rewinding its phenology to the 'Start Regrowth' when it reaches the end of the full canopy phase.
Canopy expansion Temperature base, optima and maximum temperature thresholds determine how quickly the thermal time that drives canopy development is accumulated and so sets the temperature response of canopy development
Root Nitrogen Concentrations of leaf, residue and roots (g/g) determine how much nitrogen is required to grow these components
Proportion of pasture mass that is legume (0-1) -  determines how much N fixation the SprumPastureInstance is capable of.
Maximum Canopy Conductance - This controls crop transpiration demand.
Net radiation at 50% of maximum conductance - This also controls how transpiration demand responds to radiation levels.
Does the crop respond to water stress? If checked biomass growth, nitrogen uptake and cover expansion will decrease if water shortage occurs.
No manager component is required for a SPRUM simulation.  It will simply establish itself on the date set in the SprumPastureInstance and graze and rewind its self each time phenology reaches the end of the Full Canopy Phase as shown in the figure below.   The amount of biomass that is removed each grazing is specified in a BiomassRemovalEvents object.
If greater control over grazing times is required the user should set the 'Full Canopy Duration' to a very high number so the model does not reach the rewind stage and trigger grazing by one of the following options:
Include a manager which calls the .Remove() method of the BiomassRemovalEvent on the exact dates that defoliation is required.
Include an opperations model that calls the .Remove() method of the BiomassRemovalEvent on the exact dates required.
Specify the exact dates grazing is required in the 'List of dates for removal events' on the BiomassRemovalEvents model.
It is important to ensure the 'Stage to Set Phenology to On Removal' field on the BiomassRemovalEvent is set to StartRegrowth.  The user must also specify the amounts of biomass to be removed from each SPRUM organ on the BiomassRemovalEvents.
The model is constructed from the following list of software components. Details of the implementation and model parameterisation are provided in the following sections.
1.1 Plant Model Components
Component Name
Component Type
Phenology
Models.PMF.Phen.Phenology
Arbitrator
Models.PMF.BiomassArbitrator
RootUptakesArbitrator
Models.PMF.RootUptakesArbitrator
Residue
Models.PMF.Organ
Leaf
Models.PMF.Organ
Nodule
Models.PMF.Organ
Root
Models.PMF.Organ
Total
Models.PMF.CompositeStates
AboveGround
Models.PMF.CompositeStates
MortalityRate
Models.Functions.Constant
LegumePropn
Models.Functions.Constant
SeedMortalityRate
Models.Functions.Constant
RelativeAnnualDimension
Models.Functions.LinearInterpolationFunction
Height
Models.Functions.MultiplyFunction
WaterStressFactor
Models.Functions.LinearInterpolationFunction
1.2 Child Components
1.2.1 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
As ThermalTime accumulates the crop progresses through the following phases:
1.2.2 Arbitrator
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
1.2.3 RootUptakesArbitrator
Interface between soil arbitrator and Plant model instance
All supplies, demands and uptakes passed to and from the Soil Arbitrator are in liters for water and kg for N.
This is because the Soil Arbitrator work independently of area.
This interface needs to adjust uptakes back to the appropriate units to send them back to plan.
1.2.4 Residue
This is the basic organ class that contains biomass structures and transfers
1.2.5 Leaf
This is the basic organ class that contains biomass structures and transfers
1.2.6 Nodule
This is the basic organ class that contains biomass structures and transfers
1.2.7 Root
This is the basic organ class that contains biomass structures and transfers
1.2.8 Total
This is a composite biomass class, representing the sum of 1 or more biomass objects.
1.2.9 AboveGround
This is a composite biomass class, representing the sum of 1 or more biomass objects.
1.2.10 MortalityRate
A constant function (name=value)
1.2.11 LegumePropn
A constant function (name=value)
1.2.12 SeedMortalityRate
A constant function (name=value)
1.2.13 RelativeAnnualDimension
A linear interpolation model, where an
1.2.14 Height
A class that returns the product of its child functions.  Performance note: This function returns zero as soon as any of its child functions return zero.  Therefore, speed gains can be achieved by placing children that are likely to return zero values at the top of the list of children
1.2.15 WaterStressFactor
A linear interpolation model, where an
2 Sensibility tests
Experiment Name
Design (Number of Treatments)
EstablishTest
EstablishAge x AgeToMaxD (12)
RegrowthTest
Duration x ResidualCover (9)
FullCanopyTest
Size x Duration (9)
WaterStressTest
IrrigResponse x Irrigation (4)
UserGrazeTest
Grazing (3)
Examining the effects of different combinations of age at establishment and years to max root depth parameters
These simulations compare differen initial cover and regrowth durations on Sprum predictions
Demonstrates the effects of changing the duration of the full canopy phase
This Simulation demonstrates the effects of having water stress response turned on or off in low and high stress situations
This simulation tests using a biomassRemovalEvents model to do grazing and phenology rewind
By setting a large value for the FullCanopyDurtion parameter, SPRUM does not reach the end of this phase so it does not self graze and rewind.  Instead we set defoliation dates and the stage to rewind to in the biomass removal event so we can specify defoliations on specific dates.
3 Interface
3.1 SPRUM
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
PlantType
String
True
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
