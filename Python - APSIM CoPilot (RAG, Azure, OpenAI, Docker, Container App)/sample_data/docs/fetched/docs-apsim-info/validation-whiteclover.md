# WhiteClover documentation

Source URL: https://docs.apsim.info/validation/WhiteClover
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:53:43.801567+00:00

1 The APSIM WhiteClover Model
Acknowledgements
This model was developed with support and datasets provided by the Forages for Reduced Nitrogen Leaching (FRNL) programme, funded by the New Zealand Ministry of Business, Innovation and Employment.  The programme is a partnership between DairyNZ, AgResearch, Plant & Food Research, Lincoln University, the Foundation for Arable Research, and Landcare Research.
It should be recognised that the model was also build upon the work of several people, especially at AgResearch, that have been developing the understanding and modelling of pastural systems over many years.  The author is also thankfull to Ellen Hume for her help on gathering plant parameters and organising the dataset.
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
Rogerio Cichota, Plant and Food Research, New Zealand
Preamble
White clover (
Trifolium repens
) is a herbaceous perennial herb in the Fabaceae family (former Leguminosae) native of Europe and west Asia.  It can now be be found in swards around the world in places with temperate and subtropical climates (
Thomas, 1987
;
Brock et al., 1989
).  The plant aboveground consists of prostrate segmented stolons, from which tri-foliate leaves grow on top of a 5-15 cm petiole.  From the axils of each leaf emerges either a flower stalk (peduncle) or a branching stolon.  Roots are quite thick and relatively shallow; the initial taproot dies after a year or so, but roots can also grow from each branching stolon.  These rooted stolon behave and can become independent plants. Cultivated varieties are commonly classified into small-, medium- and large-leaved cultivars, which besides leaf size also reflects variations in plant height and proportion of stolons (
Caradus et al., 1989
).  The inflorescence is typically of white colour and grows about one third higher than the leaves.  As a legume, white clover can host N-fixing bacteria (
Rhizobium
) in a symbiotic relationship. N fixation by rhyzobia can tranfer large quantities of N from the atmosphere into the soil.  This is a major reason why white clover is typically sown alongside grasses in pastures.  Nonetheless, white clover is also a valuable forage, producing very high quality feed and is highly palatable to ruminants.
Objective:
The model presented here was built to simulate the growth of white clover as a forage crop.  The current model focus on describing biomass accumulation and regrowth after defoliation in monocultural swards.  The model was developed using the Plant Modelling Framework (PMF) and thus, it is readily possible to use the WhiteClover model in mixed swards, but this usage has not been properly tested yet and should be done with caution.
The simulation of variations in N concentration over the growing season can also be considered as incipient and only reasonably good. The model should be updated and its performance revised in the future when more data becomes available.
Presentation
This model has been built using APSIM's Plant Modelling Framework (PMF), as introduced in
Brown et al., 2014
and described in more detail in
www.apsim.info
.  It aims to simulate the development and growth of white clover (
Trifolium repens
) as a forage crop.  The model focus primarily on describing the vegetative growth with only a simplified account of the reproductive phase.  Only an inflorescence organ is defined, so flowers and seeds and their development are not considered separately.  This was done because the reproductive phase of white clover stands is quite long and undefined: different plants may start flowering at different times and single plants may have flower buds as well as ripen seed at the same time (e.g.
Hyde et al., 1959
;
Thomas, 1987
;
Medeiros et al., 2000
); more details in the phenology section below.  With more comprehensive data, the implementation of this phase and the reproductive organs may be revisited in the future.  The white clover model simulates the aboveground plant structure, including the photosynthesis process, using the SimpleLeaf procedure of PMF, which does not consider explicitly leaf age or their placement in the canopy.  The model describes a perennial crop, with phenology rewinding to the vegetative stage at the end of the reproductive phase.
Inclusion in APSIM simulations
To include white clover in a simulation the procedure is the same as any other APSIM crop:
The white clover object can be dragged or copied from the Crop folder in the toolbox into a Field in any simulation;
To become active and grow, the white clover crop needs to be sown using a manager script with a sowing rule. e.g.:
WhiteClover.Sow(cultivar: Huia, population: 450, depth: 5, rowSpacing: 100);
If a specified cultivar is not available, a fatal error will be thrown.
Harvest and biomass removal
White clover biomass can be removed by raising one of the valid methods: Harvest, Cut, Graze, or Prune; this is done using a manager script, similarly to other crops.  The proportion of the biomass of each organ that is removed from the system and/or added to the residue pools may be specified; otherwise defaults will be used.  Note that the sum of fractions removed and added to the residue should be <= 1.0.  To specify the proportions for removal in a manager script, use a RemovalFractions class as shown below:
[EventSubscribe("Commencing")]
private void OnSimulationCommencing(object sender, EventArgs e)
{
RemoveFraction = new RemovalFractions(WhiteClover.Organs);
}
[EventSubscribe("DoManagement")]
private void OnDoManagement(object sender, EventArgs e)
{
if (Clock.Today.Date == HarvestDate)
{
RemoveFraction.SetFractionToRemove("Leaf", 0.80, "Live");
RemoveFraction.SetFractionToRemove("Petiole", 0.50, "Live");
RemoveFraction.SetFractionToResidue("Leaf", 0.05, "Live");
RemoveFraction.SetFractionToResidue("Petiole", 0.01, "Live");
WhiteClover.Harvest(RemoveFraction);
}
}
The default values for the fractions (%) to be removed or transferred to residue are shown below in the description of each organ.
Crop termination
To fully terminate a crop the EndCrop event should be raised:
WhiteClover.EndCrop();
Once a crop has been ended the field is open to be used by another APSIM plant model, or another white clover crop.  Note that ending white clover is not necessary before sowing another crop, competition for resources will take place between crops when there is more than one in the field.
The model is constructed from the following list of software components. Details of the implementation and model parameterisation are provided in the following sections.
1.1 Plant Model Components
Component Name
Component Type
Phenology
Models.PMF.Phen.Phenology
Arbitrator
Models.PMF.OrganArbitrator
Leaf
Models.PMF.Organs.SimpleLeaf
Petiole
Models.PMF.Organs.GenericOrgan
Stolon
Models.PMF.Organs.GenericOrgan
Inflorescence
Models.PMF.Organs.GenericOrgan
Nodule
Models.PMF.Organs.Nodule
Taproot
Models.PMF.Organs.GenericOrgan
Root
Models.PMF.Organs.Root
ShootRootRatio
Models.Functions.PhaseLookup
TargetShootRootRatio
Models.Functions.PhaseLookup
TargetAboveGroundFraction
Models.Functions.DivideFunction
MortalityRate
Models.Functions.Constant
SeedMortalityRate
Models.Functions.Constant
1.2 Composite Biomass
Component Name
Component Type
AboveGround
Models.PMF.CompositeBiomass
AboveGroundLive
Models.PMF.CompositeBiomass
AboveGroundDead
Models.PMF.CompositeBiomass
BelowGround
Models.PMF.CompositeBiomass
BelowGroundLive
Models.PMF.CompositeBiomass
BelowGroundDead
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
Huia
Huia
Kopu
Kopu
Tahora
Tahora
Apex
Apex
1.4 Child Components
1.4.1 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
The duration of each phenological phase in white clover is controlled either by the accumulation of thermal time or photoperiod.
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
This organ represents all the leaves in the plant and has only a basic representation of the canopy structure, with all leaves considered to be distributed in one layer.  The total leaf biomass is used to determine the height of the canopy, using a function that can vary for different cultivars but that currently it is not affected by competition with other plants.
1.4.4 Petiole
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note that this represents all the petioles as well as the flower peduncles in the plant, an approach taken due to the apparent similarities between them and lack of specific data to parameterise them separately.  There are no distinctions between ages or placement in the canopy either.
1.4.5 Stolon
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
This organt is parameterised to represent all the stolons in the plant, without distinction between ages or placement in the canopy.
1.4.6 Inflorescence
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note that this organ represents all the reproductive parts of the plant: flowers, pods, seeds, etc.  No distinction is made between ages or placement in the canopy.
The main reason for grouping all flowers and seeds together is the complexity of the reproductive phase of white clover.  There are variations in phenology between plants in the same sward, and even in the same plant there can be new flowers being initiated while others contain mature seeds (e.g.
Hyde et al., 1959
;
Thomas, 1987
;
Medeiros et al., 2000
).  A comprehensive and specific dataset is necessary to support the implementation of distinct organs within the inflorescence, this may be revisited in the future.
1.4.7 Nodule
This organ simulates the root structure associate with symbiotic N fixing bacteria.  It provides the core functions of determining
N fixation supply and related costs.  It also calculates the growth, senescence and detachment of nodules.
Note that this represents all the nodules containing the symbiotic N-fixing bacteria (
Rhizobium
) of the plant, without distinction between ages or placement in the root system.
Nodules are considered plant organs that result from the infection of fine roots by
Rhizobium
bacteria (
Crush, 1987
;
Hirsch et al., 1997
;
Puppo et al., 2005
;
Russelle et al., 2008
). These structures are composed by a mixture of infected and non infected plant cells that behave as isolated organs, but under control of the host plant.  This is a symbiotic relationship whereby the plant supplies carbon and energy to the the bacteria which in turn provide the plant with N fixed from the air.  Nodules of white clover are of indeterminate type, these are more complex than determinate types, including a meristematic zone and have a much longer life span (
Puppo et al., 2005
;
Russelle et al., 2008
).  Different strains of the
Rhizobium
bacteria can vary on their capacity to infect white clover (with some plant cultivars also showing selection to different strains) as well as on their effectiveness to fix N
2
from the air.  However, the model currently does not simulate variations in nodule development nor on N fixation rate due to varying bacteria strains.
The host plant controls the supply of sugars and oxygen to the nodules, thus regulating the N fixation rate and by extention on the nodule's growth and senescence (
Puppo et al., 2005
;
Russelle et al., 2008
;
Haag et al., 2013
). The model simulates this interaction by linking the respective processes in the nodules to the plants N status.
1.4.8 Taproot
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note that this represents the taproots of all the plant in the population.  These are primarily considered as storage organs, its researves can be made available to boost plant growth in spring and/or following a defoliation.
Taproots in white clover are generally efemerous.  They develop from seeds and are ubiquitous in young plants, but these generally die out about a year after germination and adult plants are often described as having no taproots (
Hart, 1987
;
Brock et al., 2000
).  However, some secondary roots can be taproot-like, especially in especific cultivars, e.g. ladino (
Thomas, 1987
;
Caradus et al., 1998
). Given that, and because the model simulates the a whole population, it is assumed that taproots are always present, but typically constitute a minor fraction of the root systems in most cultivars.
1.4.9 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
Note that this organs is parameterised to represents all the fine roots of the plant.  The root organ is reponsible for uptake but can also supply both N and DM from its non-structural biomass, although this typically represents only a small contribution to plant growth and only happen upon senescence.  The taproot organ is used for simulating biomass storage and remobilisation.  The root depth increases through time, as described below, but currently it does not retreat in case of root depth, something that will have to be revisited in the future.
1.4.10 ShootRootRatio
Look up a value based upon the current growth phase.
1.4.11 TargetShootRootRatio
Look up a value based upon the current growth phase.
The target shoot-to-root ratio is used to alter the allocation of biomass when the balance between the various organs is disturbed, such as after a defoliation.
Published data for the shoot-to-root ratio in white clover show a considerable variability, with values between 1.5 to 8.0 for a wide range of conditions (
Hart, 1987
;
Gourley et al., 1993
;
Almeida et al., 1999
;
de Neergaard et al., 2004
;
den Hollander et al., 2007
;
Murray, 2012
;
Crush et al., 2015
). For typical enviromental conditions the data suggest a mean value around 2.5-3.5 for adult plants.  This value can be affected by environmental conditions, with water or nutrient deficiency inducing greater root growth, while low light conditions lead to a higher allocation above ground. The current model does not explicitly consider these variations, this should be upgraded when more specific data becomes available.
1.4.12 TargetAboveGroundFraction
A class that divides all child functions.
Returns zero if nominator is zero, returns double.maxValue if denominator is zero.
The model attempts to keep the proportion of the plant biomass above and below ground at a given value.  The 'target' value is the 'ideal' relative proportion of biomass above and below ground for the plant at a given growth phase. This is in accord with the biomass allocation plasticity approach (e.g.
Wilson, 1988
;
Levang-Brilz et al., 2002
). The approach ensures that biomass allocation is shifted to the appropriate organs when the balance is disturbed, especially important for recovery after defoliation.  Studies have show that white clover, unlike most plants, has a relatively fixed proportion of biomass above ground, even during reproductive phase (e.g.
Thomas, 1980
). The model uses the target shoot-to-root ratio to compute the proportion of biomass above ground (e.g.
Johnson et al., 1987
).
1.4.13 MortalityRate
A constant function (name=value)
1.4.14 SeedMortalityRate
A constant function (name=value)
2 Validation
The performance of the white clover model is evaluated in simulations based on experiments conducted in New Zealand.
2.1 New Zealand
All data from New Zealand
####Discussion
The results show that the model was able to capture the general growth pattern and the cumulative amounts of herbage harvested.  Given the large uncertainties that measurements of plant biomass have, it is generally more appropriate to compare season cumulative values instead of values for individual measurements.  The experimental results showed no significant effect of N fertilisation on biomass accumulation, which was expected as white clover can compensate for those variations through biological N fixation.
The values for N content were also highly variable, and they did show some response to fertilisation.  The model results also shows similar response, but the general pattern was not we described by the model.  The way the model computes demand and remobilisation of N seems to be a specific part of the model that still needs improvement.
Experiment Name
Design (Number of Treatments)
FRNLLincoln
NRate (6)
FRNLRuakura
NRate (6)
####Presentation
The simulation setup was based on a field trial performed at Lincoln University Research Dairy Farm, New Zealand, between 2014 and 2016.  The experiment was part of the FRNL (Forages for Reduced Nitrogen Leaching) program (for more details see
Martin et al., 2017
).  Here only plots with pure white clover are used.
The experiment consisted of six fertiliser treatments with four replicates.  The nominal N rates were 0, 50, 100, 200, 350 and 500 kg/ha/yr, but the actual amounts applied varied as each application was linked to a defoliation event, the plan was to have 10 defoliation per year, but this was not possible, with seven events actually happening for white clover.  The plots were mown at a height of about 4 cm and the harvested herbage was removed from the plots.  The measurements comprised of dry matter yield and quality indicators (here herbage N content is used).
Data and general management of the experiment was supplied by Grant Edwards, Lincoln University.
The soil at the experimental site was a Templeton fine sandy loam (an Immature Pallic soil, USDA: Udic Haplustept), the required parameters were inferred based on data from the New Zealand National Soils Database (Landcare Research).
Weather data was obtained from nearby Broadfield weather station (NIWA).
####Presentation
This simulation setup was based on field trials performed at DairyNZ's Scott Farm, in Ruakura, New Zealand, between 2014 and 2016.  The experiment was part of the FRNL (Forages for Reduced Nitrogen Leaching) program and the data is basically unpublished (references will be added when this come about).  Here only plots with pure white clover are used.
The experiment was set up with six fertiliser treatments and three replicates, with nominal N rates of 0, 50, 100, 200, 350 and 500 kg/ha/yr (actual rate varied).  The swards were mown at a a height of approximately 4 cm and the herbage was removed from the plots.  The measurements comprised of herbage yield and quality indicators (here N content is used).
Observed data supplied by Grant Edwards, Lincoln University.
The soil at the experimental site was a Horotiu silt loam (an Orthic Allophanic soil; USDA: Typic Udivitrand), the parameters required were inferred using data from the New Zealand National Soils Database (Landcare Research).
Weather data was obtained from nearby Ruakura weather station (NIWA).
3 Interface
3.1 WhiteClover
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
Almeida, J. P., Lüscher, A., Frehner, M., Oberson, A., Nösberger, J., 1999. Partitioning of P and the activity of root acid phosphatase in white clover (Trifolium repens L.) are modified by increased atmospheric CO2 and P fertilisation. Plant and Soil 210 (2), 159.
Brock, J. L., Caradus, J. R., Hay, M. J. M., 1989. Fifty years of white clover research in New Zealand. Proceedings of the New Zealand Grassland Association 50, 25-39.
Brock, J., Albrecht, K., Tilbrook, J., Hay, M., 2000. Morphology of white clover during development from seed to clonal populations in grazed pastures. The Journal of Agricultural Science 135 (2), 103-111.
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Caradus, J. R., MacKay, A. C., Woodfield, D.R., Van den Bosch, J., Wewala, S., 1989. Classification of a world collection of white clover cultivars. Euphytica 42 (1-2), 183-196.
Caradus, J., Woodfield, D., 1998. Genetic control of adaptive root characteristics in white clover. Root Demographics and Their Efficiencies in Sustainable Agriculture, Grasslands and Forest Ecosystems, 651-662.
Crush, J. R., 1987. Nitrogen fixation. White Clover, Eds: Baker, M. J. and Williams, W. M., 185-202.
Crush, J. R., Ouyang, L., Nichols, S. N., 2015. Root morphology and architecture, and internal phosphate use efficiency, in related white clover cultivars of different ages. New Zealand Journal of Agricultural Research 58 (3), 302-310.
de Neergaard, A., Gorissen, A., 2004. Carbon allocation to roots, rhizodeposits and soil after pulse labelling: a comparison of white clover (Trifolium repens L.) and perennial ryegrass (Lolium perenne L.). Biology and Fertility of Soils 39 (4), 228-234.
den Hollander, N. G., Bastiaans, L., Kropff, M. J., 2007. Clover as a cover crop for weed suppression in an intercropping design: I. Characteristics of several clover species. European Journal of Agronomy 26 (2), 92-103.
Gourley, C. J. P., Allan, D. L., Russelle, M. P., 1993. Differences in response to available phosphorus among white clover cultivars. Agronomy journal 85 (2), 296-301.
Haag, A. F., Arnold, M. F. F., Myka, K. K., Kerscher, B., Dall'Angelo, S., Zanda, M., Mergaert, P., Ferguson, G. P., 2013. Molecular insights into bacteroid development during Rhizobium-legume symbiosis. FEMS microbiology reviews 37 (3), 364-383.
Hart, A. L., 1987. Physiology. White Clover, Eds: Baker, M. J. and Williams, W. M., 125-152.
Hirsch, A. M., Larue, T. A., Doyle, J., 1997. Is the legume nodule a modified root or stem or an organ sui generis?. Critical Reviews in Plant Sciences 16 (4), 361-392.
Hyde, E. O. C., Allison McLeavey, M., Harris, G. S., 1959. Seed development in ryegrass, and in red and white clover. New Zealand journal of agricultural research 2 (5), 947-952.
Johnson, I. R., Thornley, J. H. M., 1987. A model of shoot:root partitioning with optimal growth. Annals of Botany 60 (2), 133-142.
Levang-Brilz, N., M. E. Biondini, 2002. Growth rate, root development and nutrient uptake of 55 plant species from the Great Plains Grasslands, USA. Plant Ecology 165 (1), 117-114.
Martin, K., Edwards, G., Bryant, R., Hodge, M., Moir, J., Chapman, D., Cameron, K., 2017. Herbage dry-matter yield and nitrogen concentration of grass, legume and herb species grown at different nitrogen-fertiliser rates under irrigation. Animal Production Science 57 (7), 1283-1288.
Medeiros, R. B., Steiner, J. J., 2000. White clover seed production: III. Cultivar differences under contrasting management practices. Crop Science 40 (5), 1317-1324.
Murray, L. K., 2012. Early development characteristics of different clover species during establishment.Honours.Lincoln University.
Puppo, A., Groten, K., Bastian, F., Carzaniga, R., Soussi, M., Lucas, M. M., De Felipe, M. R., Harrison, J., Vanacker, H., Foyer, C. H., 2005. Legume nodule senescence: roles for redox and hormone signalling in the orchestration of the natural aging process. New Phytologist 165 (3), 683-701.
Russelle, M. P., Schepers, J., Raun, W., 2008. Biological dinitrogen fixation in agriculture. AGRONOMY 49, 281.
Thomas, R. G., 1980. Growth of the white clover plant in relation to seed production. Herbage seed production, 56-63.
Thomas, R. G., 1987. Reproductive development. White Clover, Eds: Baker, M. J. and Williams, W. M., 63-124.
Thomas, R. G., 1987. The structure of the mature plant. White Clover, Eds: Baker, M. J. and Williams, W. M., 1-30.
Wilson, J. B., 1988. A review of evidence on the control of shoot:root ratio, in relation to models. Annals of Botany 61 (4), 433-449.
