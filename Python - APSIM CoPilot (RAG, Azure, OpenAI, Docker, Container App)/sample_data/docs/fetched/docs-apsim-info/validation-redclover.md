# RedClover documentation

Source URL: https://docs.apsim.info/validation/RedClover
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:31.401827+00:00

1 The APSIM RedClover Model
Acknowledgements
This model was developed with support and datasets provided by the Forages for Reduced Nitrogen Leaching (FRNL) programme, funded by the New Zealand Ministry of Business, Innovation and Employment.  The programme is a partnership between DairyNZ, AgResearch, Plant & Food Research, Lincoln University, the Foundation for Arable Research, and Landcare Research.
The author is also thankful to Drs. Hamish Brown, Wen Feng Cong, and Grant Edwards for supplying the experimental data, acknowledging that many other people also contributed to their collection.
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
Rogerio Cichota, Plant and Food Research, New Zealand
Introduction
Red clover (
Trifolium pratense
) is a herbaceous perennial herb in the Fabaceae family (former Leguminosae) native of Europe, west Asia, and north Africa (
Hay, 1985
;
Frame et al., 1998
;
Black et al., 2009
).  It can now be be found in swards around the world in places with temperate and subtropical climates.  Although considered a perennial, red clover plants are typically short-lived in grazed conditions (2-4years).  The plant has deep taproots and long segmented semi-erect stems. From each stem segment tri-foliate leaves grow alternately and from their axils a flower stalk (peduncle) may grow, petioles are 1-4 cm long.  Roots are quite thick and can reach depths greater than 2.0 m.  The inflorescence is typically pink/red and grows from peduncles that are less than 1cm long.  As a legume, red clover can host N-fixing bacteria (
Rhizobium
) in a symbiotic relationship; from this, large quantities of N from the atmosphere can be incorporated to the soil (
Heichel et al., 1985
;
Chmelikova et al., 2015
).  This is a major reason why red clover is sown alongside grasses in pastures, although red clover is also a valuable forage grown in monoculture, producing very high quality feed and is highly palatable to ruminants.
Objective:
The model presented here was built using the Plant Modelling Framework (PMF) of
Brown et al., 2014
to simulate the growth of red clover as forage crop.  The model current focus is on describing biomass accumulation and regrowth after harvest on monocultural swards. As a derivate of APSIM's PMF, it is readily possible to use the red clover model for mixed swards, but this has not been properly tested yet and should be done with caution.  The number of datasets available for the inital development of the model was quite limited, including data from only two environments and from experiments varying irrigation or fertiliser rates.  It is expected that more datasets will be added in the future to broaden the tests and support model improvements.
The simulation of variations in N concentration over the growing season can also be considered as incipient and only reasonably good. The model should be updated and its performance revised in the future when more data becomes available.
Note:
The model structure and paramaters were developed based on findings and data collated from a literature review and supported by analyses of the datasets presented in the validation section. Thus, the data from these datasets were not explicitly separated for calibration and validation.  This was done firstly because calibration of any parameter against these data was minimal, and secondly because there was a limited amount of data and thus dividing them would be counter-productive.  This would limit even more the amount of data and increase uncertainties, the method used to separate the data could become more important to define the success of the model calibration or validation than the model itself(e.g.
Sinclair et al., 2000
;
Mourad et al., 2005
).
Presentation
This model has been built using the Plant Modelling Framework (PMF) of
Brown et al., 2014
to simulate the growth of red clover (
Trifolium pratense
) as a forage crop.  The model focus primarily on describing the vegetative growth, with a simplified account of the reproductive phase, without explicit considering flowers and seeds. The red clover model simulates the above ground plant structure, including the photosynthesis process, using the SimpleLeaf procedure of PMF, which does not consider explicitly leaf age or placement in the canopy.  The model describes a perennial crop, with phenology rewinding to the vegetative stage at the end of the reproductive phase.
Inclusion in APSIM simulations
To include red clover in a simulation the procedure is the same as any other APSIM crop:
The red clover object can be dragged or copied from the Crop folder in the toolbox into a Field in your simulation;
To become active and grow, the red clover crop needs to be sown using a manager script with a sowing rule. e.g.:
RedClover.Sow(cultivar: Colenso, population: 450, depth: 5, rowSpacing: 100);
If a specified cultivar is not available, a fatal error will be thrown.
Harvest and biomass removal
Red clover biomass can be removed by raising one of the valid methods: Harvest, Cut, Graze, or Prune; this is done using a manager script, similarly to other crops.  The proportion of the biomass of each organ that is removed from the system and/or added to the residue pools may be specified; otherwise defaults will be used.  Note that the sum of fractions removed and added to residue should be <= 1.0.  To specify the proportions for removal in a manager script, use a RemovalFractions class as shown below:
[EventSubscribe("Commencing")]
private void OnSimulationCommencing(object sender, EventArgs e)
{
RemoveFraction = new RemovalFractions(RedClover.Organs);
}
[EventSubscribe("DoManagement")]
private void OnDoManagement(object sender, EventArgs e)
{
if (Clock.Today.Date == HarvestDate)
{
RemoveFraction.SetFractionToRemove("Leaf", 0.80);
RemoveFraction.SetFractionToRemove("Stem", 0.50);
RemoveFraction.SetFractionToResidue("leaf",0.05);
RedClover.Harvest(RemoveFraction);
}
}
The RemovalFractions class can be sent with Harvest, Cut, Graze, or Prune events. All parameters are optional, defaults are used whenever any value is not specified.
Crop termination
To fully terminate a crop the EndCrop event should be raised:
RedClover.EndCrop();
Once a crop has been ended the field is open to be used by another APSIM plant model, or another red clover crop.  Note that ending red clover is not necessary before sowing another crop, competition for resources will take place between crops when there is more than one in the field.
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
Stem
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
Models.Functions.PhaseLookup
LowerAllocationFractionAboveG
Models.Functions.MaximumFunction
UpperAllocationFractionAboveG
Models.Functions.MinimumFunction
DeltaAnnualDayLength
Models.Functions.LinearInterpolationFunction
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
Colenso
Colenso
Pawera
Pawera
Rajah
Rajah
1.4 Child Components
1.4.1 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
The duration of each phenological phase in red clover is controlled either by the accumulation of thermal time or photoperiod.
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
Note:
this organ represents all the leaves in the plant, it has only a basic representation of canopy structure and does not distinguish leaves for ages or placement in the canopy.
1.4.4 Petiole
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note:
this represents all the petioles as well as the flower peduncles in the plant, without distinction between ages or placement in the canopy.
1.4.5 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note:
this represents all the stems in the plant, without distinction between ages or placement in the canopy.
1.4.6 Inflorescence
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note:
this represents all the reproductive parts of the plant, flowers, pods, seed, etc. No distinction is made between ages or placement in the canopy.
1.4.7 Nodule
This organ simulates the root structure associate with symbiotic N fixing bacteria.  It provides the core functions of determining
N fixation supply and related costs.  It also calculates the growth, senescence and detachment of nodules.
Note:
this represents all the nodules containing the symbiotic N-fixing bacteria (
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
).  These organs are composed by a mixture of infected and non infected plant cells that behave as an isolated organ, but under general control of the host plant.  This is a symbiotic relationship whereby the plant supplies carbon and energy to the the bacteria which in turn provide the plant with N fixed from the atmosphere.  Nodules of red clover are of indeterminate type, these are more complex than determinate types (including a meristematic zone) and have a much longer life span (
Puppo et al., 2005
;
Russelle et al., 2008
).  However, the model currently does not simulate variations in nodule development nor on N fixation capacity due to varying bacteria strains.
The host plant controls the supply of sugars and oxygen to the nodules, thus regulating the N fixation rate and, by extention, the growth and senescence of nodules (
Puppo et al., 2005
;
Russelle et al., 2008
;
Haag et al., 2013
). The model simulates this by linking the respective processes to the plants N status.
1.4.8 Taproot
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note:
this represents all the taproots of the plant.
Red clover has pronounce taproots that enable it reach deep soil layers and act as a major storage organ (
Bennett et al., 1960
;
Frame et al., 1998
;
Thomas, 2003
).  The reserves can then be used to enhance growth in spring as well as after defoliation.  Given that, the proportion that taproots contribute to the total plant biomass should vary considerably, similar to other taprooted species (
Evans, 1977
;
Teixeira et al., 2008
).
1.4.9 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
Note:
this represents all the fine roots of the plant.  The root organ demands and can supply both N and DM, although the later is typically samll and due to remobilisation of non-structural biomass upon senescence. The root depth increases through time, but currently it does not retreat.  Roots provide N and water uptake supply.
1.4.10 ShootRootRatio
Look up a value based upon the current growth phase.
1.4.11 TargetShootRootRatio
Look up a value based upon the current growth phase.
The target shoot-to-root is used to alter the allocation of biomass when the balance between the various organs is disturbed, such as after a defoliation.
Published data for the shoot-to-root ratio in red clover show considerable variability, with values between 1.5 to 8.0 for a wide range of conditions (
Evans, 1973
;
Evans, 1977
;
Heichel et al., 1985
;
Chmelikova et al., 2015
). For typical enviromental conditions (low stress levels) the data suggest a strong seasonality in biomass allocation.  Similar to other forages that are strongly taprooted, basically all growth in spring is allocated to above ground organs, whereas in summer-autumn the allocation is more even, replenishing the below ground reserves (e.g.
Evans, 1973
;
Heichel et al., 1985
;
Teixeira et al., 2008
).  The partition can be further affected by environmental conditions, with water or nutrient deficiency inducing greater root growth, while low light conditions lead to a higher allocation above ground (
Gist et al., 1957
;
Kendall et al., 1994
). The current model does not explicitly consider these variations, this should be upgraded when data becomes available.
1.4.12 TargetAboveGroundFraction
Look up a value based upon the current growth phase.
The model attempts to keep the proportion of the plant biomass above and below ground at a given value.  The 'target' value is the 'ideal' relative proportion of biomass above and below ground for the plant at a given growth phase.  The approach used here mimics the allocation pattern in strongly taprooted forages (e.g.
Teixeira et al., 2008
).  In spring, nearly all biomass is allocated to above ground organs, with the proportion reducing as the length of day increases.  In summer-autumn the allocation evenly distributed amoung all organs, favouring the replenishment of below ground reserves (
Evans, 1973
;
Heichel et al., 1985
).
The approach used in the RedClover model is based on that used for describing biomass partitioning in lucerne (
Teixeira et al., 2008
), which was parameterised for Lincoln, NZ, only.  This was generalised by assuming that miminum and maximum allocations are a function of the annual variation in day length (which is a function of latitude).  Thus, the variation in allocation to above ground organs is small in the tropics and reaches a maximum near the polar circle.  This function allows for smooth transition between seasons and describe the general pattern sensibly, but it should be revised when more data becomes available.
1.4.13 LowerAllocationFractionAboveG
This class calculates the minimum of all child functions.
This is the lower limit for biomass allocation in above ground organs, assumed to be a function of latitude.
1.4.14 UpperAllocationFractionAboveG
This class calculates the minimum of all child functions.
This is the upper limit for biomass allocation in above ground organs, assumed to be a function of latitude.  It is the values used for partitioning growth in early spring.
1.4.15 DeltaAnnualDayLength
A linear interpolation model, where an
This is the variation in day length within a year (i.e. the length of longest day minus that of the shortest).  It could, or should, be computed functions in 'Weather' as it is a function of latitude, but due to the lack of an accessor, a fitted function is used instead.  The value is used to generalise the computations of the target partitioning of new growth to above ground organs.
1.4.16 MortalityRate
A constant function (name=value)
1.4.17 SeedMortalityRate
A constant function (name=value)
2 Validation
The performance of the red clover model is evaluated in simulations based on experiments conducted in New Zealand and Denmark.  These represent significant differences in environment and cover variations in irrigation and fertiliser rates, but the number of interactions are limited and thus more tests should be done for areas and management not considered here.  It is expected that more datasets will be added in the future with colaboration with end-users.
Note
: The definition of the main values compared (predicted versus measured) are given below to improve clarity.
AboveGroundWt: total biomass weight of plant material aboveground at the end of a given day;
HarvestedWt: biomass removed from the field at a given day;
RotationHarvestedWt: biomass removed from the field at the end of a defoliation event (used when defoliation occured over a few days);
CumulativeHarvestedWt: amount of harvested biomass accumulated over a growing season (between winters);
HerbageNc: average nitrogen concentration in the harvested material;
NTakenOff: amount of nitrogen taken off filed with the harvested material;
CumNTakenOff: amount of N harvested accumulated over a growing season;
2.1 New Zealand
Experiment Name
Design (Number of Treatments)
FRNLLincoln
NRate (6)
Iversen
Irrigation (2)
####FRNL Lincoln
Simulation setup based on field trial performed at Lincoln University Research Dairy Farm, New Zealand, between 2014 and 2016.  The experiment was part of the FRNL (Forages for Reduced Nitrogen Leaching) program (for more details see
Martin et al., 2017
).  Here only plots with pure red clover are used.
The experiment consisted of six fertiliser treatments (N rates of 0, 50, 100, 200, 350 and 500 kg/ha/yr) in four replicates, and measurements comprised yield and quality indicators (here N content is used).
Data supplied by Grant Edwards, Lincoln University.
The soil at the experimental site was a Templeton fine sandy loam (an Immature Pallic soil, USDA: Udic Haplustept), the required parameters were inferred based on data from the New Zealand National Soils Database (Landcare Research).
Weather data was obtained from nearby Broadfields weather station (NIWA).
This simulation setup was based on a dataset supplied by Hamish Brown, which was collected for his PhD thesis.  The trial was conducted at the Iversen field of Lincoln University, in Lincoln, New Zealand between 1996 and 2002 (
Brown, 2004
;
Brown et al., 2005
).  The work was focused on understanding water use and its interaction with yield of dryland and irrigated forage crops (lucerne, chicory and red clover were used).
The trial was implemented in spring of 1996, after conventional cultivation, including sub soiling and liming.  Red clover (cultivar '
Pawera
') was sown at a rate of 10.0 kg/ha and was grazed regularly by sheep.  There were annual applications of superphosphate but no N fertilisation (except excreta from grazing sheep).  The treatments consisted of irrigation versus dryland conditions.  The major parameters analysed were herbage growth and yield, population, nutritive characteristics, and water utilisation.  N concentration was inferred from the few measurements of crude protein and interpolated over time using a sinusoidal function; these values are for reference only and cannot be used to actually validate the model.
Soil data was obtained from measurements presented by H. Brown Thesis (
Brown, 2004
) and complemented by S-MAP database from Landcare Research Limited. Weather data was obtained from the nearby Broadfield met station in Lincoln.
2.2 Denmark
Experiment Name
Design (Number of Treatments)
Foulum
NInputs (2)
####Foulum species competition
This simulation setup was based on field trials performed at the the Foulumgaard Experimental Station, Aarhus University, Denmark, between 2013 and 2015.  The trial was part of an experiment studying species competition in multi-species sward in a dairy organic farming context.  This comprises the establishement of plots sown with various species in monoculture and in a few combinations, either varying the number and type of species or the relative sowing rate.  The plots received either no N fertiliser or a nominal annual rate of 250 kgN/ha applied as cattle slurry.  Details of the site and trial can be found in publications (
Cong et al., 2017
;
Cong et al., 2018
; and
Cong et al., 2018
).  Here only a selection of plots with red clover are used, with each treatment having three replicates.
Observed data supplied by Wen Feng Cong and Jorgen Eriksen, Aarhus University.
The soil at the experimental site was a loamy sand (a Typic Hapludult on the USDA classification), the parameters required were derived from data published and slightly adjusted with measurements done at the experimental site ([JacobsenFoulum1989]; [HeidmannFoulum1989]).
Weather data was obtained from nearby weather station (Aarhus University).
3 Interface
3.1 RedClover
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
Bennett, O. L., Doss, B. D., 1960. Effect of soil moisture level on root distribution of cool-season forage species. Agronomy Journal 52 (4), 204-207.
Black, A. D., Laidlaw, A. S., Moot, D. J., O'Kiely, P., 2009. Comparative growth and management of white and red clovers. Irish Journal of Agricultural and Food Research 48 (2), 149-166.
Brown, H. E., 2004. Understanding yield and water use of dryland forage crops in New Zealand., 288.
Brown, H. E.; Moot, D. J., Pollock, K. M., 2005. Herbage production, persistence, nutritive characteristics and water use of perennial forages grown over 6 years on a Wakanui silt loam.. New Zealand Journal of Agricultural Research 48 (4), 423-439.
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Chmelikova, L., Wolfrum, S., Schmid, H., Hejcman, M., Hulsbergen, K. J., 2015. Seasonal development of above- and below-ground organs of Trifolium pratense in grass-legume mixture on different soils. Journal of Plant Nutrition and Soil Science 178 (1), 13-24.
Cong, W.-F., Eriksen, J., 2018. Forbs differentially affect soil microbial community composition and functions in unfertilized ryegrass-red clover leys. Soil Biology and Biochemistry 121, 87-94.
Cong, W.-F., Jing, J., Rasmussen, J., Søegaard, K., Eriksen, J., 2017. Forbs enhance productivity of unfertilised grass-clover leys and support low-carbon bioenergy. Scientific Reports 7 (1), 1422.
Cong, W.-F., Suter, M., Lüscher, A., Eriksen, J., 2018. Species interactions between forbs and grass-clover contribute to yield gains and weed suppression in forage grassland mixtures. Agriculture, Ecosystems & Environment 268, 154-161.
Crush, J. R., 1987. Nitrogen fixation. White Clover, Eds: Baker, M. J. and Williams, W. M., 185-202.
Evans, P. S., 1973. Effect of seed size and defoliation at three development stages on root and shoot growth of seedlings of some common pasture species. New Zealand Journal of Agricultural Research 16 (3), 389-394.
Evans, P. S., 1977. Comparative root morphology of some pasture grasses and clovers. New Zealand Journal of Agricultural Research 20 (3), 331-335.
Frame, J., Charlton, J. F. L., Laidlaw, A. S., 1998. Temperate forage legumes., 327.
Gist, G. R., Mott, G. O., 1957. Some effects of light intensity, temperature, and soil moisture on the growth of alfalfa, red clover and birdsfoot trefoil seedlings. Agronomy Journal 49 (1), 33-36.
Haag, A. F., Arnold, M. F. F., Myka, K. K., Kerscher, B., Dall'Angelo, S., Zanda, M., Mergaert, P., Ferguson, G. P., 2013. Molecular insights into bacteroid development during Rhizobium-legume symbiosis. FEMS microbiology reviews 37 (3), 364-383.
Hay, R. J. M., 1985. Variety X management interactions of red clover (Trifolium pratense L.).Thesis.Lincoln College, University of Canterbury.
Heichel, G. H., Vance, C. P., Barnes, D. K., Henjum, K. I., 1985. Dinitrogen fixation, and N and dry matter distribution during 4 year stands of birdsfoot trefoil and red clover. Crop Science 25 (1), 101-105.
Hirsch, A. M., Larue, T. A., Doyle, J., 1997. Is the legume nodule a modified root or stem or an organ sui generis?. Critical Reviews in Plant Sciences 16 (4), 361-392.
Kendall, W. A., Shaffer, J. A., Hill, R. R., 1994. Effect of temperature and water variables on the juvenile growth of lucerne and red clover. Grass and Forage Science 49 (3), 264-269.
Martin, K., Edwards, G., Bryant, R., Hodge, M., Moir, J., Chapman, D., Cameron, K., 2017. Herbage dry-matter yield and nitrogen concentration of grass, legume and herb species grown at different nitrogen-fertiliser rates under irrigation. Animal Production Science 57 (7), 1283-1288.
Mourad, M., Bertrand-Krajewski, J. L., Chebbo, G., 2005. Calibration and validation of multiple regression models for stormwater quality prediction: data partitioning, effect of dataset size and characteristics. Water Sci Technol 52 (3), 45-52.
Puppo, A., Groten, K., Bastian, F., Carzaniga, R., Soussi, M., Lucas, M. M., De Felipe, M. R., Harrison, J., Vanacker, H., Foyer, C. H., 2005. Legume nodule senescence: roles for redox and hormone signalling in the orchestration of the natural aging process. New Phytologist 165 (3), 683-701.
Russelle, M. P., Schepers, J., Raun, W., 2008. Biological dinitrogen fixation in agriculture. AGRONOMY 49, 281.
Sinclair, T. R., Seligman, N. a., 2000. Criteria for publishing papers on crop modeling. Field Crops Research 68 (3), 165-172.
Teixeira, E. I., Moot, D. J., Brown, H. E., 2008. Defoliation frequency and season affected radiation use efficiency and dry matter partitioning to roots of lucerne (Medicago sativa L.) crops. European Journal of Agronomy 28 (2), 103-111.
Thomas, R., 2003. Comparative growth forms of dryland forage legumes. Grassland Research and Practice Series No. 11, New Zealand Grassland Association, Wellington, NZ, 18-19.
