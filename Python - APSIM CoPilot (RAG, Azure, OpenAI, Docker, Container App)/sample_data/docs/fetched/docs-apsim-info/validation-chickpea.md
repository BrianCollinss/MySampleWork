# Chickpea documentation

Source URL: https://docs.apsim.info/validation/Chickpea
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:51:51.895482+00:00

1 The APSIM Chickpea Model
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
The APSIM Chickpea model has been developed from the original APSIM Chickpea model described by
Robertson et al., 2002
using the Plant Modelling Framework of
Brown et al., 2014
.
Note to users:
Desi and Kabuli chickpea are two subspecies of chickpea. The variety set includes both types. As indicated by Peake et al., 2021 (GRDC Updates), there was less confidence in predicting
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
Shell
Models.PMF.Organs.GenericOrgan
Leaf
Models.PMF.Organs.Leaf
Stem
Models.PMF.Organs.GenericOrgan
Nodule
Models.PMF.Organs.Nodule
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
Pod
Models.PMF.CompositeBiomass
1.3 Cultivars
Cultivar Name
Alternative Name(s)
Amethyst
Amethyst
Almaz
Almaz
Boundary
Boundary
CICA1521
CICA1521
Drummond
Drummond
Genesis90
Genesis90
HatTrick
HatTrick
Kalkee
Kalkee
Kyabra
Kyabra
Monarch
Monarch
Pistol
Pistol
QuickKabuli
QuickKabuli
Seamer
Seamer
Slasher
Slasher
SlowDesi
SlowDesi
Striker
Striker
Genesis90_archivePPsensitive
Genesis90_archivePPsensitive
JAKI
JAKI
ICCV2
ICCV2
NBeG47
NBeG47
NBeG119
NBeG119
Anwar
Anwar
Ayaz
Ayaz
Hashem
Hashem
99_73C
99_73C
99_66C
99_66C
99_34C
99_34C
98_79C
98_79C
98_55C
98_55C
98_16C
98_16C
98_107C
98_107C
98_106C
98_106C
97_219C
97_219C
97_120C
97_120C
97_118C
97_118C
97_116C
97_116C
02_35C
02_35C
02_30C
02_30C
02_23C
02_23C
02_10C
02_10C
02_03C
02_03C
01_7C
01_7C
01_61C
01_61C
01_36C
01_36C
00_40C
00_40C
00_21C
00_21C
Ghab4
Ghab4
Ghab5
Ghab5
Ghab3
Ghab3
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
1.4.3 Structure
The structure model simulates morphological development of the plant to inform the Leaf class
when and how many leaves and branches appear and provides an estimate of height.
1.4.4 Grain
This organ uses a generic model for plant reproductive components.  Yield is calculated from its components in terms of organ number and size (for example, grain number and grain size).
1.4.5 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
Minimal root biomass has been simulated. It appears to interfere with simulation of stem biomass (arbitrator).
1.4.6 Shell
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.7 Leaf
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
Dead leaf ext coeff - may need to be higher than 0.3 - chickpea holds its shape pretty well when dead
1.4.8 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.9 Nodule
This organ simulates the root structure associate with symbiotic N fixing bacteria.  It provides the core functions of determining
N fixation supply and related costs.  It also calculates the growth, senescence and detachment of nodules.
1.4.10 MortalityRate
A constant function (name=value)
1.4.11 SeedMortalityRate
A constant function (name=value)
2 Validation
2.1 Map
2.2 Combined Results
Simulation results for the combined datasets from the various growing regions are shown in the following graphs. The model is able to adequately capture the influence of growing conditions (e.g. soil, climate) and management (e.g. population, irrigation, sowing date).
2.3 2019_PulseAdaptation
Data from the one year GRDC funded 'Pulse Adaptation' project led by Allan Peake, CSIRO team members included Fernanda Dreccer, Jeremy Whish, Tony Swan.
Refer to project final report Peake et al (2020):
https://doi.org/10.25919/5f1f2438cd4e4
Experiment Name
Design (Number of Treatments)
Emerald19_
TOS x Cv (27)
Mildura19_
TOS x Cv (27)
Walgett19_
TOS x Cv x Pop (36)
Greenethorpe_2019_
TOS x Cv x Irr (36)
GreenethorpePhen19_
TOS x Cv (20)
GT_HatTrick_WU_2019_
TOS (6)
Narrabri19_
TOS x Cv x Irr (36)
The experiment investigated chickpea for three times of sowing for nine cultivars at Emerald, Queensland, Australia. The soil was a light textured vertosol.  Emerald Site run by DAFQ, site leader was Doug Sands, assisted by Peter Agius. Definately no irrigation applied after 14th May, all trials were dryland.  Measurements were undertaken for biomass accumulation during the crop, and grain yield at crop maturity.
May  10 chickpeas emerged about 19/5
June  3 chickpeas emerged about 15/6
June 27 chickpeas emerged 9-10/7
Note: soil monitoring was only as deep as 120cm. A small amount of water in the 120-150cm layer may have been available to the crop that might improve simulation of TOS1 which slightly underpredicted biomass and yield of some cultivars.
Trial conducted by Michael Moodie (Frontier Farming Systems) on behalf of pulse adaptaton project led by Allan Peake. Soil = red kandosol
Time of sowing x cultivar trial
Emergence observed approx 11 days after sowing for lentils, 14 days for Desi chickpea and 16 days Kabuli chickpea. Sowing depth 6cm.
Trickle tape on the row was used to irrigated each sowing, so the irrigation was not evenly distributed. This concentration of water on the seed zone probably led to different growth patterns in the crop and makes the experiment more difficult to simulate.
Simulating way too much biomass. Possible or even likely that there are issues with the way APSIM simulates potential ET in an arid environment such as Mildura. Also possible that we need a dynamic CLL - later times of sowing don't extract as much water. Or it may be possible that biomass samples at maturity were collected too late and much of the leaf was on the ground.
Experiment conducted by AMPS research on behalf of Pulse Adaptation project. Soil = unusual, area of lighter loamy soil, not a kandosol but not a vertosol. Perhaps an extremely light textured vertosol. Retains water very well and yields well in a dry season, stores water deeper than a typical vertosol and loses less to evaporation as a result. Lower yield potential in a good season.
Time of sowing x cultivar experiment. Second TOS was lost for chickpea, seed uprooted overnight by wild pigs.
Plant population
- initial population averaged at 23 plants/m2; used 30 plants/m2 for the high population treatments.
Met file for Walgett
- trial site located at -30.538537,148.614925 near Come By Chance. Several options in SILO:
Bungle Gully, Come by Chance is approx 5 km from site
Come by Chance store is approx 20 km from site
Grid -30.55,148.60 is approx 2 km from site  --> this met file was selected as is the closest. Combined with TTag data from site
A difficult site to simulate. Felt that the 1st 2 biomass cuts may have been taken on 1/2 the stated area (which would explain why obs=2xpredicted). However trial cooperator is adamant that cut areas were correct, and says he felt the crop growth pattern was unusual, with extremely rapid growth after emergence. Possibly could have been a residual herbicide effect stimulating unusual plant hormone responses. Also potential for nutrient deficiency as we still ended up over-simulating yield from TOS1 by a factor of 1.5 to 2, whereas TOS 2 was simulated accurately for yield when biomass was lower (i.e. less likely to experience nutrient deficiency). Field possibly short on K (but soil was unfortunately not tested for K).
Greenthorpe main trial 2019, Pulse Adaptation project. Irrigated and rainfed treatments, 2-3 cultivars per species (Desi/Kabuli/Lentils). Split plot design. Soil = red-brown earth.
Irrigation system was a little uneven but was the best we could do with little preparation time (individual sprinklers along a long hand-shift hose line), but plant cuts were taken from mostly uniform areas.
Planting density information by TOS was available. It differed broadly by TOS and so average density for TOS used for all cvv.
Initial SW across site is quite variable - questioned whether appropriate to model as factorial. Variability probably due to micro-environment variations at specific location for soil cores, so factorial design was retained. Would have been substantial variation in soil type up and down the steep slope. NMM calibration was from Farming systems site 5-6km away so not sure how trustworthy that data is.
Met file
- based on SILO but used local TTag at trial for max/minT, because trial is at a different point in the landscape (north slope) to the SILO station (east slope)
Greenethorpe phenology trial: sown as small 2m single row observation plots. Conducted at site 5-6km north of Iandra Castle farming systems site.
Trial was sown by hand, so sowing depth was variable and not recorded, establishment date also variable for the same reason. The important data for this trial is the rate of leaf appearance rather than the actual leaf number which is only simulated correctly where observed emergence date matches the simulation.
Some herbicide damage was observed on early sown plots and may have affected some data.
Experiment was situated at the bottom NW corner of the main trial, so temperature would not have been the same as the bulk of plots in the main trial (due to substantial slope). Experiment was on a north facing slope.
all treatments are irrigated - "JW: For greenethorpe simulate unlimited. We tried to keep it unlimited but there would have been some stress no measure of water just applied when ever we were out there."
this experiment used different sowing dates to the other Greenethorpe trial so is simulated separately
there are only 2 sowing dates
only treatments without lights used (there were some artificially lit experiments at Gatton)
phenology measurements are recorded only up until the time of flowering
Greenthorpe main trial 2019, Pulse Adaptation project. Irrigated and rainfed treatments, 2-3 cultivars per species (Desi/Kabuli/Lentils). Split plot design. Soil = red-brown earth.
Irrigation system was a little uneven but was the best we could do with little preparation time (individual sprinklers along a long hand-shift hose line), but plant cuts were taken from mostly uniform areas.
**Water use was modelled separately to main trial so we could use individual SW data for each cultivar and treatment, hoping to get most accurate observed data by looking at individual NMM sites.
Would have been substantial variation in soil type up and down the steep slope. NMM calibration was from Farming systems site 5-6km away so not sure how trustworthy that data is. probably not great, unfortunately.
Met file
- based on SILO but used local TTag at trial for max/minT, because trial is at a different point in the landscape (north slope) to the SILO station (east slope)
Experiment conducted at PBI Narrabri Research Station, by Helen Bramley from USYD on behalf of Pulse Adaptation project. Site was on the new section of the farm, over Killarney Gap Rd. Soil = Vertosol
Met file
- taken from SILO airport + field maxT, MinT and rain spliced in.
Irrigations from contractor
-
17/05	25 mm just after TOS 1 planted (accidentally typed 17/06 but you can see in excel entry is before and after other may entries)
17/06	30 mm - whole trial
06/08	irrigation 25 mm - whole trial
09/09	irrigation - 30mm irrigated part only
24/09	irrigated only 30 mm
25/10	irrigated only 30 mm
Soil water characterisation is a bit uncertain - cores weren't dried at 100 degrees. Still waiting on recalibrated soilwater data as of 7Aug20. This may explain consistent over-prediction at this site
2.4 SE Queensland
Experiment Name
Design (Number of Treatments)
Lawes1990
Sowing (2)
Lawes1991
Sowing (2)
Lawes1995
Trt (2)
Bremar_2003_
S (4)
Peter Carberry data partially published in Brinsmead (1992).
http://www.regional.org.au/au/asa/1992/concurrent/cropping-systems/p-04.htm
Experiment conducted at Gatton RS, 1990. One variety, two sowing dates. Creek bank soil is variable over short distances.
Note that Brinsmead paper says 1st sowing date was waterlogged soon after sowing, which would explain the delayed biomass growth curve in comparison to simulated.
Note - auto-irrigate function has been used as there is no record of actual irrigation, although in theory the experiment was fully irrigated. In practice, this usually means some low level water stress is experienced, as has been simulated with this experiment.
Gatton RS 1991, 2 sowing date experiment. Creek bank soil is highly variable.
Peter Carberry upublished data. Also known as APS2??
Current version of the model seems to be starting grainfill too early for this data. May have been a waterlogging stress that delayed flowering? Or do modern cultivars set seed faster than Amethyst?
Experiment known as APS27. Irrigated vs rainfed treatments. Was sown adjacent to the APS26 wheat experiment and should theoretically have received the same irrigation. Creek bank soil is quite variable. There could be a problem with NMM calibration for this reason. WUE of APS26 wheat was not right, but their problem was too much biomass for the amount of water. This simulation has opposite problem of model getting away too fast. Doesn't seem possible to balance biomass development and water use for simulations of this experiment.
Published data from Yash Dang suggests there is little deep water extraction in Chickpea - at odds with this experiment. There may have been tree roots encroaching on the experimental area.
Possible that the wet treatment developed disease issues or N fixation issues due to over-watering which might explain overprediction of biomass.
Also possible that disease management in general was poor and/or undetected, although grain yield was predicted OK
Experiment conducted by J Whish, near Meandarra. 2 time of sowing x 2 row spacing.
Some uncertainty about whether in-season soil cores were taken directly on the crop row, at the mid inter-row position.
Not great simulations but retained to show observed trends in relation to sowing date and row spacing. Some notes:
(1) site was quite a strong slope for southern QLD - may have simulated less runoff than actual.
(2) soil was highly variable, as per initial coring results from TOS1 and TOS2. The approach taken was to choose an average starting point that would go close to both TOS1 and TOS2, bearing in mind that increasing sowing soil water for TOS2 would increase overprediction.
(3) first biomass cut looks suspiciously similar to the second biomass cut.  Areas may have been incorrectly reported either for first cut or subsequent cuts.
(4) It's quite possible that in 2003 undetected disease was limiting Chickpea growth and development
(5) the site was remote and biomass loss and shattering was likely during storms prior to harvest, and probably greater in TOS1 due to increased delay in obtaining final biomass.
(6) some uncertainty around whether grower would actually have decreased plant pop for 35 cm row spacing, the plant pop for these treatments possibly may be double.
(7) Interesting note: the high population, narrow row spacing treatment was the highest yielding in the observed data - as per Kulai 2003. We haven't been able to simulate this. Possible that the narrow row spacing may have had double the plant population and this was not recorded correctly.
2.5 NewSouthWales
Experiment Name
Design (Number of Treatments)
Kulai_2003_
S (4)
Kulai_2004_
S (4)
Experiment conducted by J Whish, near North Star on a red kandosol (there are a few paddocks with this soil in this district). 2 time of sowing x 2 row spacing.
Some uncertainty about whether in-season soil cores were taken directly on the crop row, at the mid inter-row position.
Interesting note: the high population, narrow row spacing treatment was the highest yielding in the observed data - as per Bremar 2003. We haven't been able to simulate this. Possible that the narrow row spacing may have had double the plant population and this was not recorded correctly.
The site was remote and biomass loss and shattering was likely during storms prior to harvest, and probably greater in TOS1 due to increased delay in obtaining final biomass.
There is some uncertainty around whether grower would actually have decreased plant pop for 35 cm row spacing, the plant pop for these treatments possibly may be double.
Experiment conducted by J Whish, near North Star. In this second year the experiment was conducted on a reddy-brown vertosol on the same farm as the year 1 experiment, which was on a red kandosol (there are a few paddocks with this soil in this district). 2 time of sowing x 2 row spacing.
Some uncertainty about whether in-season soil cores were taken directly on the crop row, at the mid inter-row position.
The site was remote and biomass loss and shattering was likely during storms prior to harvest, and probably greater in TOS1 due to increased delay in obtaining final biomass.
There is some uncertainty around whether grower would actually have decreased plant pop for 35 cm row spacing, the plant pop for these treatments possibly may be double.
2.6 ICRISAT
Experiment Name
Design (Number of Treatments)
ICRISAT_18_19_BM8_
Cv_ (4)
Experiment conducted at ICRISAT, Hyderabad in 2018/2019 by Amir Hajjapoor, postdoc working for Jana Kholova.
A number of experiments were conducted but there were many problems with emergence and pests (peacocks).
This experiment was the best, 4 cultivars x one sowing date. Plant stand was uneven but OK.
Unsure why grain yield is not simulated well when biomass was. Pod + grain data indicates simulated grain development was behind the observed, perhaps in a warm environment pod initiation occurs sooner, or cultivars may have a different branching structure that allows them to set more grains faster once flowering begins. Also possible that flowering date was not correctly recorded and perhaps grainfilling began sooner.
3 Sensibility
Experiment Name
Design (Number of Treatments)
FixationResponse
N (21)
DetailedDynamics
Sowing (1)
CO2TE
CO2 (2)
ExtremeEnvironmentsKatherine
TOS (6)
ExtremeEnvironmentsLaunceston
TOS (6)
Field studies by
Schwenke et al., 1998
showed that, for chickpea crops near Walgett/Moree in 1995, the proportion of total plan N derived from fixation decreased from about 90% when soil nitrate was very low at sowing, down to approximately 20% when soil nitrate was near 100kg/ha at sowing.  Furthermore, N fixation was unable to account for N export in grain.
Peter Carberry unpublished data.  Creek bank soil. Varies in short distances.
Note that Brinsmead paper says 1st sowing date was waterlogged, which would explain the delayed response on that experiment.
Note - auto-irrigate function has been used as no record of actual irrigation? Probably that they ran out of water at the end and actually wouldn't have achieved the yields we simulated
This test examines the impact of a doubling of CO2 from historical (350ppm) on Transpiration Efficiency.
Reyenga et al., 1999
suggest an increase of approximately 37% in Transpiration Efficiency over this range in CO2 concentration for c3 plants (wheat).
In this test, a series of chickpea crops are simulated for Dalby, Queensland, Australia.  The slope of plots of biomass production vs crop water use is used to quantify a gross seasonal TE.  The change in slope should approximate the response suggested by
Reyenga et al., 1999
.
Testing ability of model to simulate rainfed chickpea production at Katherine RS, Northern Territory, Australia.
25mm irrigation applied on day of sowing to ensure germination on all sowing dates through the year
Testing ability of model to simulate rainfed chickpea production at Launceston, Tasmania, Australia.
25mm irrigation applied on day of sowing to ensure germination on all sowing dates through the year
4 Interface
4.1 Chickpea
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
5 References
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Lawless, Conor, Semenov, MA, Jamieson, PD, 2005. A wheat canopy model linking leaf area and phenology. European Journal of Agronomy 22 (1), 19-32.
Reyenga, P.J., Howden, S. M., Meinke, H., McKeon, G.M., 1999. Modelling global change impacts on wheat cropping in south-east Queensland, Australia. Environmental Modelling & Software 14, 297-306.
Robertson, M. J., Carberry, P. S., Huth, N. I., Turpin, J. E., Probert, M. E., Poulton, P. L., Bell, M., Wright, G. C., Yeates, S. J., Brinsmead, R. B., 2002. Simulation of growth and development of diverse legume species in APSIM. Australian Journal of Agricultural Research 53 (4), 429-446.
Schwenke, Graeme, Peoples, M., Turner, G., Herridge, David, 1998. Does nitrogen fixation of commercial, dryland chickpea and faba bean crops in north-west New South Wales maintain or enhance soil nitrogen?. Australian Journal of Experimental Agriculture - AUST J EXP AGR 38.
