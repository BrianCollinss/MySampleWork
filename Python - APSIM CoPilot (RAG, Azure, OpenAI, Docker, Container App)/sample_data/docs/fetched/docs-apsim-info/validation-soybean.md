# Soybean documentation

Source URL: https://docs.apsim.info/validation/Soybean
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:53:11.983389+00:00

1 The APSIM Soybean Model
The APSIM Soybean Model
The APSIM Soybean model has been developed using the Plant Modelling Framework (PMF) of (
Brown et al., 2014
). This new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a model in much the same way that models can be coupled to construct a simulation. This means that dynamic composition of lower level process and organ classes (e.g. photosynthesis, leaf) into larger constructions (e.g. maize, wheat, sorghum) can be achieved by the model developer without additional coding.
The model consists of:
a phenology model to simulate development between growth phases
a structure model to simulate plant morphology
a collection of organs to simulate the various plant parts
an arbitrator to allocate resources (N, biomass) to the various plant organs
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
Buchanan
Buchanan
Djakal
Djakal
Davis
Davis
Hooper_MG40
Hooper_MG40
Stephens_MG40
Stephens_MG40
FiskebyV
FiskebyV
F148_7
F148_7
Bowyer
Bowyer
Bunya
Bunya
Cowrie
Cowrie
Soya791
Soya791
Manark
Manark
Warrigal
Warrigal
Leichhardt
Leichhardt
Djakal1
Djakal1
Trial_MG00
Trial_MG00
Lambert_MG0
Lambert_MG0
IA1006_MG10
IA1006_MG10
IA2008_MG20
IA2008_MG20
PioneerP22T61_MG22
PioneerP22T61_MG22
PioneerP22T69R_MG22
PioneerP22T69R_MG22
Pioneer92MGI_MG26
Pioneer92MGI_MG26
PioneerP92Y75_MG27
PioneerP92Y75_MG27
Macon_MG30
Macon_MG30
PioneerP93M11_MG31
PioneerP93M11_MG31
Becks321NRR_MG32
Becks321NRR_MG32
PioneerP932T16R_MG32
PioneerP932T16R_MG32
Becks367NRR_MG37
Becks367NRR_MG37
AsgrowAG4403_MG40
AsgrowAG4403_MG40
HornbeckHBK4891_MG40
HornbeckHBK4891_MG40
Pioneer94B01_MG40
Pioneer94B01_MG40
PioneerP9504_MG50
PioneerP9504_MG50
AsgrowAG5701_MG50
AsgrowAG5701_MG50
Hutcheson_MG50
Hutcheson_MG50
NK622_MG60
NK622_MG60
Nandou12
Nandou12
Texuan13
Texuan13
Jiuyuehuang
Jiuyuehuang
Hedou19
Hedou19
Generic_MG000
Generic_MG000
Generic_MG00
Generic_MG00
Generic_MG0
Generic_MG0
Generic_MG1
Generic_MG1
Generic_MG2
Generic_MG2
Generic_MG3
Generic_MG3
Generic_MG4
Generic_MG4
Generic_MG5
Generic_MG5
Generic_MG6
Generic_MG6
Generic_MG7
Generic_MG7
Generic_MG8
Generic_MG8
Generic_MG9
Generic_MG9
Generic_MG10
Generic_MG10
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
The new phenology follows the V/R staging system.
Figure [FigureNumber]:
Comparison of soybean phenological stages for APSIM Classic and APSIM Next Generation.
The key differences for cultivars are mostly phenological parameters (e.g., Vegetative.Target, ReproductivePhotoperiodModifier). In some cases, some additional parameters have been changed, see parameter values in the cultivar section.
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
1.4.4 Grain
This organ uses a generic model for plant reproductive components.  Yield is calculated from its components in terms of organ number and size (for example, grain number and grain size).
1.4.5 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
1.4.6 Nodule
This organ simulates the root structure associate with symbiotic N fixing bacteria.  It provides the core functions of determining
N fixation supply and related costs.  It also calculates the growth, senescence and detachment of nodules.
1.4.7 Shell
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.8 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.9 MortalityRate
A constant function (name=value)
1.4.10 SeedMortalityRate
A constant function (name=value)
2 Validation
The soybean model has been tested across a range of planting and agronomic conditions.  These include datasets from Australia, USA and China covering a wide range of weather conditions (See graphs that follow) and agronomic factors (irrigation, fertiliser, sowing dates).
2.1 Map
2.2 USA
2.2.1 FACTS
This trial was conducted on deep fertile soils, in central Iowa (Ames) and in northwest Iowa (Sutherland), USA, in years 2015, 2016 and 2017. In each year two planting date were studied, early versus late. Row spacing was 76 cm, and plant population was around 35 plants/m2 (see details in APSIM managers). No nitrogen or irrigation applied. This region has shallow water tables (about 1.2 m below surface, range 0.3 to 3m) and the soil has no subsurface drainage. Data from years 2015 and 2017 have been incorporated while data from year 2016 will be added later. Local weather data (the station was positioned 10 m from the trial) and actual soil profile data (measured in Nov 2014 up to 1.2 meter, soil organic matter, texture, and pH by layer) were used. Additional soil data were taken from ssurgo. The following measurements were taken per plot (each treatment was replicated 3 times): Phenology (visual observations in the field using the V/R coding system); biomass samplings 6-9 times per season and per treatment (destructive sampling of 1 m2 per plot); biomass partitioning and dry weight of each plant tissue including green and yellow leaves, stem plus petioles, and pod-walls plus seeds (note that seeds were separated at the final harvest only); carbon and nitrogen concentration of each plant tissue (LECO CN analyzer); green leaf area index (LI Area Meter); node number and pod number per plant; leaf area per node in year 2015 only; root front depth over time (manual soil core technique) and maximum root depth, mass and length (mechanical gidding prode technique, depth of 240 cm) at approximately middle grain fill stage; daily soil water and temperature at two depths, 15 and 45 cm (5TM sensors placed horizontally, METEO group), daily water table depth (CTD-10 sensors, METEO group), and soil nitrate and ammonium approximately 10 times over each season and treatment at two depths 0-30 and 30-60 cm (sample was extracted in 2 M potassium chloride), and N-fixation over time (Isotope dilution method); soybean leaf senescence (gages to measure drop leaves dry matter and CN concentration, 1m2 size, 3-4 measurements per season per treatment); crop residue dry matter, soil cover and soil nitrogen and water measurements (manual samplings 10 times from crop harvest to planting of the next crop). This dataset is licensed by the Iowa State University Research Foundation.
Experiment Name
Design (Number of Treatments)
Ames2015
Sow (2)
Ames2017
Sow (2)
Sutherland2015
Sow (2)
Sutherland2017
Sow (2)
2.2.2 Arkansas
This trial (
Edwards, 2005
) was conducted on a silt loam soil in Fayetteville Arkansas, USA, in years 2001, 2002 and 2003. The factor studied were seven soybean maturity groups (from mg 00 to mg 6) and in some year multiple varieties within a maturity group. Crops were irrigated. No N fertilization was applied. This region has no shallow water tables. Phenology data were collected at different growth stages and were used in this work. Weather and soil data for the site were estimated from public sources. For additional information about this trial, see
Archontoulis et al., 2014
.
Experiment Name
Design (Number of Treatments)
Arkansas2001
Cv (8)
Arkansas2002
Cv (8)
Arkansas2003
Cv (8)
2.2.3 Indiana
This trial (
Robinson, 2009
) conducted in West Lafayette Indiana, USA, in years 2006 and 2007. The factors studied were six planting dates x three varieties (from mg 2.6 to mg 3.7). No irrigation or fertilization applied. Phenology, gravimetric soil moisture at three depth (manual soil cores), pod numbers, seed size, grain yield, protein and oil data were collected. Weather and soil data for the site were estimated from public sources. For additional information about this trial, see
Archontoulis et al., 2014
.
Experiment Name
Design (Number of Treatments)
Indiana2006
Sow x Cv (18)
Indiana2007
Sow x Cv (18)
2.2.4 Mississippi
This trial (
Zhang, 2004
) was conducted in Stoneville Mississippi USA, in years 1998 to 2002. The factors studied were planting dates (ranged from early March to early July) and varieties (from mg 3.4 to mg 5.6). Crops were irrigated. Phenological measurements were obtained in the field following the V/R staging system. For additional information about this trial, see
Archontoulis et al., 2014
.
Experiment Name
Design (Number of Treatments)
Mississippi2002
Cv x Sow (8)
Mississippi2003
Cv x Sow (8)
2.2.5 Nebraska
This trial (
Salvagiotti, 2009
)was conducted on a deep silt loam soil in Lincoln Nebraska USA, in years 2006 and 2007. The factors studied were different crop histories (normal N applied to corn-soy rotation and high N applied to the rotation) and 4 N fertilization treatments to the soybean crop (zero N, 180 kg N/ha at planting, 180 kg N/ha as split between preplanting and V6 leaf stage, and 180 kg N/ha at R5 stage). Crops were irrigated. Row spacing was 76 cm, plant density about 31 pl/m2 and planting date around late april to early May. The following measurements were taken: initial soil nitrate, destructive biomass samples six times during the season (size 0.46 m2 per plot) per year and per treatment, biomass partitioning, dry weight and N concentrations of each tissue (Dumas method in Rapid N Cube; Elementer, Germany), fallen leaves, phenology, and N-fixation during the growing season using the ureide method. Local weather data and public soil data were used to drive the simulations.
Experiment Name
Design (Number of Treatments)
Nebraska2006
N (4)
Nebraska2007
N (4)
2.3 Australia
2.3.1 Coleambally
Experiment Name
Design (Number of Treatments)
Coleambally
Sow (1)
2.3.2 Gatton
This dataset (
Muchow et al., 1993
) includes the cultvar "Davis" grown under irrigated conditions in a subtropical environment at the Gatton Research Station in southeastern Queensland, Australia.  Plants were sampled almost weekly for biomass partitioning, nitrogen and plant development.
Experiment Name
Design (Number of Treatments)
Gatton
Sow (1)
2.3.3 Griffith
These data
Timsina et al., 2007
were obtained from a field experiment conducted in 1999–2000 on a Hanwood loam soil at CSIRO Land and Water, Griffith, Australia.  Two indeterminate soybean cultivars (Hooper and Stephens belonging to late maturity group [MG] 3 or early MG 4) were on 15 November, 8 December, and 6 January.
Experiment Name
Design (Number of Treatments)
Griffith
Sow x Cv (6)
2.3.4 Katherine
This dataset (
Muchow et al., 1993
) includes the cultvar "Buchanan" grown under a range of conditions in a tropical environment at the Katherin Research Station in the Northern Territory, Australia.  This includes two irrigated conditions in 1988 and two planting dates in 1989.
Experiment Name
Design (Number of Treatments)
Katherine1988
Water (2)
Katherine1989
Sow (2)
2.3.5 Kununurra
This dataset
Muchow et al., 1986
includes a short-season and a long-season cultivar grown on two different soil types under both irrigated and water-limited conditions in a semi-arid tropical environment at the Kimberley Research Station (15 ° 38' S, 128°43 ' E) in northern Western Australia.  Crops were grown over the winter months because of the tropical location.
Experiment Name
Design (Number of Treatments)
Kununurra1980
Water (3)
Kununurra1979
Water (3)
2.3.6 Leeton
This dataset
Gaynor et al., 2011
is derived from a small-plot serial sowing date study were conducted over the summers of 2006–07 and 2007–08, at the NSW Department of Primary Industries’ Leeton Field Station, New South Wales, Australia (348280S, 1468250E). The soil was a grey self-mulching clay, described as a Vertosol under the Australian Soil Classification.  Ten diverse soybean genotypes ranging from very early to very late maturity were grown with 10 sowing dates in the first season and 9 in the second.
Experiment Name
Design (Number of Treatments)
Leeton2006
Sow x Cv (50)
Leeton2007
Sow x Cv (45)
2.4 Combined Results
Simulation results for the combined datasets from the various countries are shown in the following graphs.  The model is able to adequately capture the influence of growing conditions (soil, climate) and management (population, Nitrogen, irrigation, sowing date).
2.5 China
2.5.1 Yaan
These data [Wu2019] were obtained from a field experiment conducted in 2014–2016 on a a Purple clay loam(XI-Luvic Xerosols, FAO classification) at Sichuan Agricultural University, Yaan, China.  Three soybean cultivars (Jiuyuehuang,Nandou12, and Texuan13 belonging to maturity group [MG] 5-7 ) were sown on 15 Jun 2014, 18 Jun 2015, and 18 Jun 2016.
Experiment Name
Design (Number of Treatments)
Yaan2014
Cv (3)
Yaan2015
Cv (3)
Yaan2016
Cv (3)
2.5.2 Heze
These data [Wu2019] were obtained from a field experiment conducted in 2013–2015 on sandy
loam (J-Fluvisols, FAO classification) in the surface 0.5 m, changing to loam at depth with a deep soil profile (> 2 m) in Heze city, China. One soybean cultivar (Hedou19 belonging to maturity group [MG]3) were sown on 15 Jun 2014, 18 Jun 2015, and 18 Jun 2016.
Experiment Name
Design (Number of Treatments)
Hedou19
Sow (3)
3 Sensibility
3.1 FertiliserResponse
This sensibility test checks that Soybean yield response to N fertiliser is modest under normal field conditions.  A simulation experiment has been constructed to simulate yield in Nebraska for different rates of N applied at sowing.  Yield variation in response to this should be low.
Experiment Name
Design (Number of Treatments)
FertiliserResponse
N (5)
3.2 MaturityGroups
The optimum maturity class for soybean in Iowa should be approximately 2 to 3.  Simulations are provided for the entire range of generic cultivars for several years at Ames, Iowa.
Experiment Name
Design (Number of Treatments)
MaturityGroupsIowa
Cv (13)
3.3 TimeOfSowing
The data of
Gaynor et al., 2011
show that Soybean yield should be approximately 4t/ha for early sown soybean (Mid November) in the Australian Riverina region and decrease across the growing season to lower values (1.5-2t/ha) by late January.
Experiment Name
Design (Number of Treatments)
TimeOfSowingGriffith
TOS (4)
3.4 DetailedDynamics
Experiment Name
Design (Number of Treatments)
DetailedDynamics
Sow (1)
4 Interface
4.1 Soybean
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
Archontoulis, S. V., Miguez, F. E., Moore, K. J., 2014. A methodology and an optimization tool to calibrate phenology of short-day species included in the APSIM PLANT model: Application to soybean. Environmental Modelling & Software 62, 465 - 477.
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Edwards, J.T., Purcell, L.C., 2005. Soybean yield and biomass responses to increasing plant population among diverse maturity groups: I. Agronomic characteristics. Crop Science 45, 1770–1777.
Gaynor,L.G., Lawn, R.J., James, A.T., 2011. Agronomic studies on irrigated soybean in southern New South Wales. I. Phenological adaptation of genotypes to sowing date. Crop & Pasture Science 62, 1056–1066.
Muchow, R.C., Robertson, M.J., Pengelly, B.C., 1993. Accumulation and partitioning of biomass and nitrogen by soybean, mungbean and cowpea under contrasting environmental conditions. Field Crop Research 33, 13–36.
Muchow, R.C., Sinclair, T.R., 1986. Agronomic studies on irrigated soybean in southern New South Wales. I. Phenological adaptation of genotypes to sowing date. Field Crops Research 15, 143-156.
Robinson, A.P., Conley, S.P., Volenec, J.J., Santini, J.B., 2009. Analysis of high yielding, early-planted soybean in Indiana. Agronomy Journal 101, 131–139.
Salvagiotti, F., Specht, J.E., Cassman, K.G., Walters, D.T., Weis, A., Dobermann, A., 2009. Growth and N fixation in high yielding soybean: impact if N fertilization. Agronomy Journal 101, 958-970.
Timsina,J., Boote,K.J., Duffield,S., 2007. Evaluating the CROPGRO Soybean Model for Predicting Impacts of Insect Defoliation and Depodding. Agronomy Journal 99, 148–157.
Zhang, L., Zhang, J., Kyei-Boahen, S., 2004. Developing phenological prediction tables for soybean.. Crop Management.
