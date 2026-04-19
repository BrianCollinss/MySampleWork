# FodderBeet documentation

Source URL: https://docs.apsim.info/validation/FodderBeet
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:51:55.410635+00:00

1 The APSIM FodderBeet Model
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
Khaembah E.N., Brown H.E., Zyskowski R., Chakwizira E., de Ruiter J.M., Teixeira E.I.
The New Zealand Institute for Plant & Food Research Limited
Private Bag 4704, Christchurch, New Zealand
The APSIM Fodder Beet Model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a model in much the same way that models can be coupled to construct a simulation. This means that dynamic composition of lower level process and organ classes (e.g. photosynthesis, leaf) into larger constructions (e.g. maize, barley, sorghum) can be achieved by the model developer without additional coding.
The model consists of:
A phenology model to simulate development between growth phases
A structure model to simulate plant morphology
A collection of organs to simulate the various plant parts
An arbitrator to allocate resources (N, biomass) to various plant organs.
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
StorageRoot
Models.PMF.Organs.GenericOrgan
Leaf
Models.PMF.Organs.Leaf
Petiole
Models.PMF.Organs.GenericOrgan
Root
Models.PMF.Organs.Root
MortalityRate
Models.Functions.LinearInterpolationFunction
SCS
Models.Functions.AddFunction
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
Brigadier
Brigadier
Rivage
Rivage
Jamon
Jamon
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
The partitioning of biomass to fodder beet organs follows a "Relative Allocation" routine (see Arbitrator module documentation) guided by coefficients derived from the October sowing date data of the test dataset
1.4.2 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
The fodder beet model is described for growth and dry matter production using solar radiation and temperature
as driving functions. The effect of temperature is quantified using a thermal-time accumulation function.
Thermal-time is calculated in degree days for ambient temperature above a base temperature (Tbase). Tbase is
assumed to be 0°C (
Chakwizira et al., 2016
).
1.4.3 Structure
The structure model simulates morphological development of the plant to inform the Leaf class
when and how many leaves and branches appear and provides an estimate of height.
1.4.4 StorageRoot
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.5 Leaf
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
1.4.6 Petiole
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.7 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
1.4.8 MortalityRate
A linear interpolation model, where an
1.4.9 SCS
A class that returns the sum of its child functions.
Non structural carbohydrates and sugars
1.4.10 SeedMortalityRate
A constant function (name=value)
2 FodderBeetValidation
2.1 NewZealand
2.1.1 Lincoln
Experiment Name
Design (Number of Treatments)
Lincoln2011
Nit (5)
Lincoln2012
Irr (4)
Lincoln2014
Cv x SD (8)
LincolnRS2016
Irr x Nit (6)
This was a nitrogen fertiliser treatment trial conducted at Lincoln in 2011 (
Chakwizira et al., 2014
).
This was an irrigation treatment trial conducted at Lincoln in 2012 (
Chakwizira et al., 2014
).
The data used here is from a sowing date experiment conducted at Lincoln, Canterbury, New Zealand. The experiment was established in the field as a Randomised Complete Block Design with four replicates. Two culivars ("Rivage" and "Brigadier") were evaluated over four sowing dates (19 September, 17 October, 17 November and 15 December in 2014). The first phase i.e. calibration/parameterisation of a potential yield model was completed using cultivar "Rivage" data from the October sowing date (
Khaembah et al., 2017
).
This trial was conducted in the rainshelter at Plant and Food Research in Lincoln, New Zealand in 2016. The objective was to evaluate the effect of nitrogen and irrigation on the development and growth of fodder beet crops.
Details:
â€¢	Three nitrogen treatments:  0 kg N/ha, 50 kg N/ha & 300 kg N/ha applied as dissolved urea with fertigation
â€¢	Two irrigation treatments: Nil and full irrigation
Fodder beet (cultiva "Rivage") was precision drilled on 18 October 2016. Sowing density was 11 plants/m2 and row spacing was 0.45m.
2.1.2 AshleyDene
This is a nitrogen fertiliser by irrigation trial conducted at Ashley Dene in 2013 (
Chakwizira et al., 2016
).
Experiment Name
Design (Number of Treatments)
Ashley2013
Irr x Nit (8)
2.1.3 SFF
The data (unpublished)used here is from a SFF experiment managed by de Ruiter et al.
2.1.3.1 Canterbury
Experiment Name
Design (Number of Treatments)
Orari
Nit x Splits (8)
Rakaia
Nit x Splits (8)
The data used here is from a SFF experiment (Batty; Winchester)managed by de Ruiter et al.
2.1.3.2 Southland
Experiment Name
Design (Number of Treatments)
Gore
Nit x Splits (8)
Riverton
Nit x Splits (8)
2.1.3.3 NorthIsland
Experiment Name
Design (Number of Treatments)
Taranaki
Nit x Splits (8)
Waikato
Nit x Splits (8)
Whanganui
Nit x Splits (8)
2.1.4 P21EXT
This was an extension of the P21 project
2.1.5 FRNL_NCRS
The data used here is from the FRNL trials managed by de Ruiter et al.
Experiment Name
Design (Number of Treatments)
FRNL_NCRS2014
Nit (2)
FRNL_NCRS2015
SD (4)
2.2 Australia
Fodder beet population experiment based on Pembleton & Rawnsley (2011) report. Three sowing (precision)rates (4, 8, 12 plants/m^2) were evaluated.
Experiment Name
Design (Number of Treatments)
Irishtown
Pop (3)
2.3 NewZealand_Sensitivity
2.3.1 LincolnSens
Experiment Name
Design (Number of Treatments)
Lincoln2016Temp
Temperature (5)
Lincoln2016Extinc
KCoefficent (8)
This trial was conducted in the rainshelter at Plant and Food Research in Lincoln, New Zealand in 2016. The objective was to evaluate the effect of nitrogen and irrigation on the development and growth of fodder beet crops.
Details:
â€¢	Three nitrogen treatments:  0 kg N/ha, 50 kg N/ha & 300 kg N/ha applied as dissolved urea with fertigation
â€¢	Two irrigation treatments: Nil and full irrigation
Fodder beet (cultiva "Rivage") was precision drilled on 18 October 2016. Sowing density was 11 plants/m2 and row spacing was 0.45m.
This trial was conducted in the rainshelter at Plant and Food Research in Lincoln, New Zealand in 2016. The objective was to evaluate the effect of nitrogen and irrigation on the development and growth of fodder beet crops.
Details:
â€¢	Three nitrogen treatments:  0 kg N/ha, 50 kg N/ha & 300 kg N/ha applied as dissolved urea with fertigation
â€¢	Two irrigation treatments: Nil and full irrigation
Fodder beet (cultiva "Rivage") was precision drilled on 18 October 2016. Sowing density was 11 plants/m2 and row spacing was 0.45m.
2.4 GrazingExample
This represents a Grazing trial at Ashley Dene, Canterbury, New Zealand
Management and Data from
Edwards, G.R., J.M., d.R., Dalley, D.E., Pinxterhuis, J.B., Cameron, K.C., Bryant, R.H., Di, H.J., Malcolm, B.J.and Chapman, D.F. 2014. Dry matter intake and body condition score change of dairy cows grazing fodder beet, kale and kale-oat forage systems in winter. Proceedings of the New Zealand Grassland Association 76: 81-88.
This represebts a Grazing trial at Ashley Dene, Canterbury, New Zealand
Management and Data from
Edwards, G.R., J.M., d.R., Dalley, D.E., Pinxterhuis, J.B., Cameron, K.C., Bryant, R.H., Di, H.J., Malcolm, B.J.and Chapman, D.F. 2014. Dry matter intake and body condition score change of dairy cows grazing fodder beet, kale and kale-oat forage systems in winter. Proceedings of the New Zealand Grassland Association 76: 81-88.
3 Interface
3.1 FodderBeet
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
Chakwizira, E. de Ruiter, J. M. Maley, S. Dellow, S. J. George, M.J., Michel, A.J., 2014. Water use efficiency of fodder beet crops. Proceedings of New Zealand Grassland Association 76, 125-134.
Chakwizira, E. de Ruiter, J. M., Maley, S., 2014. Growth, nitrogen partitioning and nutritive value of fodder beet crops grown under different application rates of nitrogen fertiliser. New Zealand Journal of Agricultural Research 57, 75-89.
Chakwizira, E., Dellow, S. J., Teixeira, E. I., 2016. Quantifying canopy formation processes in fodder beet (Beta vulgaris subsp. vulgaris var. alba L.) crops. European Journal of Agronomy 74, 144-154.
Khaembah, E. N. Brown, H. E. Zyskowski, R. Chakwizira, E. de Ruiter, J. M., Teixeira, E. I., 2017. Development of a fodder beet potential yield model in the next generation APSIM. Agricultural Systems 158, 23-38.
Lawless, Conor, Semenov, MA, Jamieson, PD, 2005. A wheat canopy model linking leaf area and phenology. European Journal of Agronomy 22 (1), 19-32.
