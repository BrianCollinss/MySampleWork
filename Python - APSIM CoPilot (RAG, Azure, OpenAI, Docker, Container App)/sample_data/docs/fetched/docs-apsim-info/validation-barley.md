# Barley documentation

Source URL: https://docs.apsim.info/validation/Barley
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:51:48.944710+00:00

1 The APSIM Barley Model
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
Brown, H.E., Huth, N.I., Khaembah E.N., Zyskowski, R., and Holzworth, D.P.
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
Spike
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
AboveGroundLive
Models.PMF.CompositeBiomass
AboveGroundDead
Models.PMF.CompositeBiomass
BelowGround
Models.PMF.CompositeBiomass
Total
Models.PMF.CompositeBiomass
TotalLive
Models.PMF.CompositeBiomass
TotalDead
Models.PMF.CompositeBiomass
Ear
Models.PMF.CompositeBiomass
StemPlusSpike
Models.PMF.CompositeBiomass
1.3 Cultivars
Cultivar Name
Alternative Name(s)
Booma
Booma
Bumpa
Bumpa
Boss
Boss
Cellar
Cellar
County
County
Dash
Dash
Doyen
Doyen
Hooded
Hooded
Omaka
Omaka
Omaka1
Omaka1
Optic
Optic
Pyramid
Pyramid
Quench
Quench
Retriever
Retriever
Sherwood
Sherwood
Tavern
Tavern
Triumph
Triumph
Valetta
Valetta
Vortex
Vortex
Alestar
Alestar
Banks
Banks
Baudin
Baudin
Bass
Bass
Biere
Biere
Buloke
Buloke
Cassiopee
Cassiopee
Capstan
Capstan
Commander
Commander
Compass
Compass
CSIROB1
CSIROB1
CSIROB3
CSIROB3
Dash
Dash
Fathom
Fathom
Fleet
Fleet
Flinders
Flinders
Franklin
Franklin
Gairdner
Gairdner
Granger
Granger
Grimmett
Grimmett
Grout
Grout
Hindmarsh
Hindmarsh
Keel
Keel
Lockyer
Lockyer
Mundah
Mundah
Navigator
Navigator
Oxford
Oxford
RGT_Planet
RGT_Planet,Planet
Rosalind
Rosalind
Schooner
Schooner
Scope
Scope
Shepherd
Shepherd
Spartacus_CL
Spartacus_CL,Spartacus
Stirling
Stirling
Unicorn
Unicorn
Urambie
Urambie
Westminster
Westminster
Yagan
Yagan
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
Barley exhibits a range of developmental responses to environment and these are strongly influences by genotype characteristics.
Temperature effects development increasing development rates and decreasing phase durations as temperatures increase.  Theset affects are captured by thermal time, .  However, Barley also exhibits vernalisation and photoperiod sensitivities in its Vegetative phase and further photoperiod sensitivity in the EarlyReproducivePhase.  Photoperiod responses are seen as a reduction in the length of a phase for a photoperiod sensitive genotype in response to a longer photoperiod.  For vernalisation sensitive varieties (Winter types) exposure to cool temperatures or short photoperiods during the Vegetative phase will reduce the thermal time duration of the vegetative phase.
This model draws on the Kirby Framework to capture these vernalisation and photoperiod responses.  This framework assumes that the timing of anthesis is a result of the timing of flag leaf and an additional thermal time passage from there to anthesis.  It also assumes the timing of flag leaf is a result of the Final Leaf Number which sets a target, and leaf appearance rate, which sets the rate of progress toward this target.  Leaf appearance rate is a function of Thermal time and a genotype specific Phyllochron which changes with Haun stage (
Jamieson et al., 1995
).
Final Leaf Number (FLN) is modeled as the sum of three numbers:
FLN = MinLeafNumber + VernalLeaves + PhotoLeaves
Where MinLeafNumber is the number of leaves that a barley crop will produce when vernalisation is satisified early in the crops duration (before 2nd true leaf) and it is grown in a long photoperiod.  VernalLeaves are the number of leaves that are added due to vernalisation effects.  For insensitive varieties this will always be zero but this is potentially a larger number for sensitivie varieties and the number progresively decreases as the crop encounters more vernalisation.  PhotoLeaves are the number of leaves that are added to the minimum leaf number as a result of short day exposure.  For insensitive varieties this will be zero but is potential larger for more sensitivie varieties and decreases as day length increases.  More detailed explinations of the components of phenology are provided below.
1.4.3 Structure
The structure model simulates morphological development of the plant to inform the Leaf class
when and how many leaves and branches appear and provides an estimate of height.
Barley exhibits a range of developmental responses to environment and these are influences by genotype characteristics.
Temperature effects development increasing development rates and decreasing phase durations as temperatures increase.  These affects are captured by thermal time.  However, barley also exhibits vernalisation and photoperiod sensitivities in its Vegetative phase and further photoperiod sensitivity in the EarlyReproducivePhase.  Photoperiod responses are seen as a reduction in the length of a phase for a photoperiod sensitive genotype in response to a longer photoperiod.  Vernalisation responses are more complicated as they are driven by temperature but interact with photoperiod.  For vernalisation sensitive varieties (Winter types) exposure to cool temperatures or short photoperiods during the Vegetative phase will reduce the thermal time duration of the vegetative phase.
We draw on the Kirby Framework to capture these vernalisation and photoperiod responses.  This framework assumes that the timing of anthesis is a result of the timing of flag leaf and an additional thermal time passage from there to anthesis.  It also assumes the timing of flag leaf is a result of the Final Leaf Number which sets a target, and leaf appearance rate, which sets the rate of progress toward this target.  Leaf appearance rate is a function of Thermal time and a genotype specific Phyllochron which changes with Haun stage as described by
Jamieson et al., 1995
.
Final Leaf Number (FLN) is modeled as the sum of three numbers:
FLN = MinLeafNumber + VernalLeaves + PhotoLeaves
Where MinLeafNumber is the number of leaves that a wheat crop will produce when vernalisation is satisified early in the crops duration (before 2nd true leaf) and it is grown in a long photoperiod..  VernalLeaves are the number of leaves that are added due to vernalisation effects.  For insensitive varieties this will always be zero but this is potentially a larger number for sensitivie varieties and the number progresively decreases as the crop encounters more vernalisation.  PhotoLeaves are the number of leaves that are added to the minimum leaf number as a result of short day exposure.  For insensitive varieties this will be zero but is potential larger for more sensitivie varieties and decreases as day length increases.  More detailed explinations of the components of phenology are provided below.
Wheat exhibits a range of developmental responses to environment and these are strongly influences by genotype characteristics.
Temperature effects development increasing development rates and decreasing phase durations as temperatures increase.  Theset affects are captured by thermal time, .  However, wheat also exhibits vernalisation and photoperiod sensitivities in its Vegetative phase and further photoperiod sensitivity in the EarlyReproducivePhase.  Photoperiod responses are seen as a reduction in the length of a phase for a photoperiod sensitive genotype in response to a longer photoperiod.  Vernalisation responses are more complicated as they are driven by temperature but interact with photoperiod.  For vernalisation sensitive varieties (Winter types) exposure to cool temperatures or short photoperiods during the Vegetative phase will reduce the thermal time duration of the vegetative phase.
We draw on the Kirby Framework to capture these vernalisation and photoperiod responses.  This framework assumes that the timing of anthesis is a result of the timing of flag leaf and an additional thermal time passage from there to anthesis.  It also assumes the timing of flag leaf is a result of the Final Leaf Number which sets a target, and leaf appearance rate, which sets the rate of progress toward this target.  Leaf appearance rate is a function of Thermal time and a genotype specific Phyllochron which changes with Haun stage as described by
Jamieson et al., 1995
.
Final Leaf Number (FLN) is modeled as the sum of three numbers:
FLN = MinLeafNumber + VernalLeaves + PhotoLeaves
Where MinLeafNumber is the number of leaves that a wheat crop will produce when vernalisation is satisified early in the crops duration (before 2nd true leaf) and it is grown in a long photoperiod..  VernalLeaves are the number of leaves that are added due to vernalisation effects.  For insensitive varieties this will always be zero but this is potentially a larger number for sensitivie varieties and the number progresively decreases as the crop encounters more vernalisation.  PhotoLeaves are the number of leaves that are added to the minimum leaf number as a result of short day exposure.  For insensitive varieties this will be zero but is potential larger for more sensitivie varieties and decreases as day length increases.  More detailed explinations of the components of phenology are provided below.
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
1.4.7 Spike
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.8 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.9 MortalityRate
A constant function (name=value)
1.4.10 SeedMortalityRate
A constant function (name=value)
2 Validation
A test dataset has been developed to test the APSIM barley model for a range of environmental (soil and climate) conditions, management options (sowing dates, populations, nitrogen rates, irrigation) and genetic backgrounds (different regions, cultivar types).  These tests have been groups into various geographical regions to allow the user to evaluate the suitability of the model for their particular region of interest.  Graphs of model performance are provided for yield, biomass production, canopy development, phenological development, water and nitrogen uptake, and grain yield components.
2.1 Map
2.2 NewZealand
Experiment Name
Design (Number of Treatments)
MCPD09_10
Cult x Nit x Irr (16)
MCPD10_11
Cult (8)
MCPD11_12
Cult x SD (9)
RS2014_15
Cult x Irr (6)
RS1988_89
Irr (6)
RS1995_96
Water x N (8)
LUDF2015_16
NProp (5)
ABlock99_00
Sow x Pop (12)
This experiment was conducted in A Block near Lincoln, New Zealand to determine if there were any differences in the water use efficiency of different commercial varieties of barley.  The results are un-published to date.
Four cultivars of Barley ('Omaka', 'Dash', 'Sherwood' and 'Booma') were sown on the 1st of October 2009 in a randomised complete block design with 4 replicates and two other factors, Irrigation and Nitrogen.  The irrigation treatments consisted of full irrigation (to replace measured evapotranspiration) or nil irrigation (recieving only rainfall).  The nitrogen treatments were 150 kg N broadcast in the early vegetative stage or nil fertiliser.
Biomass accumulation, organ N content, Leaf area index, radiation interception and soil water content were measured at regular frequencies throughout the experiment.
Following on from MCPD09_10, this experiment was conducted in A Block near Lincoln, New Zealand to test a wider range of barley genotypes for differences in water use efficiency.  The results are un-published to date.
Treatments were simply 8 commercially available varieties of barley ('Booma','County','Dash','Hooded','Omaka','Optic','Quench' and 'Retriever')
Biomass accumulation, Leaf area index, radiation interception and soil water content were measured at regular frequencies throughout the experiment.
Following on from MCPD10_11, this experiment was conducted in A Block near Lincoln, New Zealand to test the effects of sowing time on apparent water use efficiency of three varieties of Barley.  The results are un-published to date.
Treatments were simply 3 commercially available varieties of barley ('Booma','Dash' and 'Omaka') sown on three different sowing dates (13 Aug, 9 Sep and 15 Nov).
Biomass accumulation, Leaf appearance, leaf size, radiation interception and soil water content were measured at regular frequencies throughout the experiment.
This was a field experiment established at the New Zealand Institute for Plant & Food Research Limited rain-out shelter in October 2014. Barley cultivars cv Dash and cv Omaka, differing canopy characteristics and water extraction capabilities were established with a range of irrigation treatments aimed at producing a range of soil moisture deficits.
High =Full replacement irrigation: replace weekly, measured crop water use;
Mid =Intermediate replacement with reduced frequency and higher volume e.g. monthly application of Â½ measured crop water use;
Low =Nil (or near nil) irrigation, e.g.  irrigated once at anthesis.
The treatments were monitored for:
Soil water - this was measured continuously using auto logging TDR and weekly using a neutron probe;
Canopy light interception;
Biomass - quantified four times during crop growth and at final harvest
Leaf area index
This experiment was conducted in the rainshelter at Plant and Food Research in Lincoln, New Zealand. The objective was to evaluate the effect of drought on the yield of barley crops. The experiment is fully described by Jamieson et al. (1995).  Briefly, 'Triumph' barley was sown at 188kg/ha on 07 September 1988. This sowing rate resulted in a population of 291 Â± 6 plants/m2.  Plants were evaluated for their response to drought of varying duration. The treatments were:
Full irrigation â€“ measured ET replaced weekly (06 October to 29 December)
Early Drought 1 â€“ irrigated from 03 November to 05 January
Early Drought 2 â€“ irrigated from 17 November to 19 January
Early Drought 3 â€“ irrigated from 24 November to 19 January
Early Drought 3 â€“ irrigated from 01 December to 09 February
Middle Drought 1 â€“ irrigated (06/10/1988 to 03/11/1988) and (24/11/1988 to 29/12/1989)
Middle Drought 2 â€“ irrigated (06/10/1988 to 03/11/1988) and (01/12/1988 to 29/12/1989)
Middle Drought 3 â€“ irrigated (06/10/1988 to 03/11/1988) and (15/12/1988 to 29/12/1989)
Middle Drought 3 â€“ irrigated (06/10/1988 to 03/11/1988)
Late Drought 1 â€“ irrigated (06/10/1988 to 17/11/1988)
Late Drought 2 â€“ irrigated (06/10/1988 to 24/11/1988)
Late Drought 3 â€“ irrigated (06/10/1988 to 01/12/1989)
This is a water response trial conducted inside and outside the rain shelter at the New Zealand Institute for Plant & Food Research in Lincoln and described in De Ruiter et al (1999). Briefly, a vernalisation insensitive barley, variety "Valetta", was sown at 280 plant/m
2
on 20 October 1995 and subjected to five water treatments as follows:
Full drought (in rain shelter) â€“ was not irrigated except for 17 mm rainfall as a result of shelter failure 42 days after sowing;
Early drought (in rain shelter) â€“ also receive 17 mm rainfall and the drought was fully relieved at the onset stem elongation;
Late drought (in rain shelter) â€“ fully irrigated (i.e. net crop water use + soil water evaporation) up to stem elongation and nil irrigation thereafter;
Rain-fed (outside the rain shelter) â€“ received all the rain but no irrigation;
Full irrigation (located outside the rain shelter) â€“ received all the rain and irrigation to replace ET.
This is the mineral nitrogen (N) validation trial conducted by the New Zealannd Institure for Plant & Food Research from September 2015 to February 2016. The experiment was established at the Lincoln University Dairy Farm on a deep well-drained Templeton silt loam soil. Five N fertiliser treatments were evaluated:
Nil (0%) N - the crop did not get any fertiliser N
50% N - the crop received 50% of the required fertiliser N
75% N - the crop received 75% of the required fertiliser N
100% N - the crop received the required fertiliser N as estimated by the fertiliser calculator
125% N - the crop received 25% fertiliser N above the requirement.
This was a sowing date by irrigation trial conducted at Lincoln. The following were evaluated:
Two water treatments; Rainfed (nil irrigation) and irrigated (310 mm)
Two fertiliser N treatments; 0N (no fertiliser) and 150N (150 kg N/ha)
2.2.1 CPT
This was a cultivar evaluation trial conducted in Lincoln New Zealand between 2003 and 2006. Crops were sown in either May or September. Observations made were of (i) DAS to flag leaf appearance and (ii) DAS to flowering.
Experiment Name
Design (Number of Treatments)
CPT
Cult x SD (110)
2.3 Australia
2.3.1 MCVP
This experiment was funded as part of the Australian Managing Climate Variability Program.  Barley cultivars were sown at two planting dates at three sites for the purpose of improving APSIM predictions of the phenology of commercial cultivars.  The locations included Gatton in Queensland (subtropical location), Birchip in Victoria (temperate inland location) and Tarlee in South Australia (temperate location).
Experiment Name
Design (Number of Treatments)
Gatton2011
TOS x Cv (16)
Birchip2011
TOS x Cv (16)
Tarlee2011
TOS x Cv (16)
2.3.2 HermitageRS
This rain-out shelter experiment was undertaken at the Hermitage Research Station near Warwick, Queensland.  Treatments included 3 irrigation experiments to provide combinations of early and later water stress.  The experiment was described by
Goyne et al., 1996
.
Experiment Name
Design (Number of Treatments)
HermitageRS
Irr (3)
2.3.3 Wellcamp
This experiment provides information on biomass accumulation, canopy development and final yield for Barley sown on the eastern Darling Downs.  The experiment is explained in
Goyne et al., 1996
.
Experiment Name
Design (Number of Treatments)
Wellcamp
1993 (1)
2.3.4 Hermitage1990
This experiment was conducted at Hermitage Research Station near Warwick, Queensland.  Three sowing dates were used to study changes in phenology, biomass accumulation and canopy development.  The experiment is explained in
Goyne et al., 1993
.
Experiment Name
Design (Number of Treatments)
Hermitage1990
TOS (3)
2.3.5 Roma1988
This sowing date x plant population experiment was undertaken near Roma, Queensland.  Measurements include biomass accumulation, leaf area development and final grain yield.  The experiment is explained in
Goyne et al., 1996
.
Experiment Name
Design (Number of Treatments)
Roma1988
Sow x Pop (9)
NOTE: Sowing was 12-Mar but simulations are sown on 30 March to capture delayed germination until following rainfall event
2.3.6 Gatton1984
This nitrogen rate (0 to 200 kg N/ha) experiment was conducted in 1984 at Gatton, Queensland. The experiment is explained in
Birch et al., 1990
.
Experiment Name
Design (Number of Treatments)
Gatton1984
NRate (5)
NOTE: High N treatment logdged.  Final grain sizes were less than expected.
2.3.7 NPIField2019
Experiment Name
Design (Number of Treatments)
WaggaWagga2019
TOS x Cv (240)
Callington2019
TOS x Cv (240)
Dale2019
TOS x Cv (240)
YanYean2019
TOS x Cv (240)
2.3.8 NPIField2020
Experiment Name
Design (Number of Treatments)
WaggaWagga2020
TOS x Cv (240)
Urrbrae2020
TOS x Cv (240)
Dale2020
TOS x Cv (240)
YanYean2020
TOS x Cv (210)
2.3.9 ControlledEnvironment
Experiment Name
Design (Number of Treatments)
LaTrobeCE
Treat x Cv x Durat (140)
3 Sensibility tests
Experiment Name
Design (Number of Treatments)
WaterByNFactorial
Irrigation x Nitrogen (10)
This is a hypothetical experiment with Barley sown on the 15th of October every year for 20 years at Lincoln, New Zealand.  The treatments applied are as follows:
Two water treatments; Dry (nil irrigation) and Wet, with irrigation applied when soil water deficit reaches 60% to return water content to 100% of capacity.
five fertiliser N treatments; 0, 50, 100, 200 and 400 kg N/ha with half of the N applied at sowing and the other half applied at growth stage 32.
The results for irrigted, High N treatments the range of yields are inline with expectations for the location of the simulations.  There is no sensitivity to irrigation with zero nitrogen as N supply is the factor limiting production.  As N inputs increase the crop becomes increasingly sensitive to water application and the crop is more responsive to nitrogen with irrigation.  These results show the model is giving sensible predictions
4 Interface
4.1 Barley
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
5 References
Birch, C.J., Long, K.E., 1990. Effect of nitrogen on the growth, yield and grain protein content of barley (Hordeum vulgare). Australian Journal of Experimental Agriculture 30, 237-242.
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Goyne, PJ, Meinke, Holger, Milroy, Stephen, Hammer, G, Hare, JM, 1996. Development and use of a barley crop simulation model to evaluate production management strategies in north-eastern Australia. Australian Journal of Agricultural Research - AUST J AGR RES 47.
Goyne, PJ, Milroy, Stephen, Lilley, Julianne, Hare, JM, 1993. Radiation interception, radiation use efficiency and growth of barley cultivars. Crop and Pasture Science 44, 1351-1366.
Jamieson, P. D., Brooking, I. R., Porter, J. R., Wilson, D. R., 1995. Prediction of leaf appearance in wheat: a question of temperature. Field Crops Research 41 (1), 35-44.
Lawless, Conor, Semenov, MA, Jamieson, PD, 2005. A wheat canopy model linking leaf area and phenology. European Journal of Agronomy 22 (1), 19-32.
