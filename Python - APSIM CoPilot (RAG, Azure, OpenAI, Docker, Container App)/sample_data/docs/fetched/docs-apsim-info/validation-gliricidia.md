# Gliricidia documentation

Source URL: https://docs.apsim.info/validation/Gliricidia
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:51:58.018384+00:00

1 The APSIM Gliricidia Model
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
MortalityRate
Models.Functions.Constant
SeedMortalityRate
Models.Functions.Constant
Age
Models.Functions.AccumulateFunction
Phenology
Models.PMF.Phen.Phenology
Arbitrator
Models.PMF.OrganArbitrator
IndividualTreeLiveWt
Models.Functions.DivideFunction
Leaf
Models.PMF.Organs.PerennialLeaf
Root
Models.PMF.Organs.Root
Stem
Models.PMF.Organs.GenericOrgan
Nodule
Models.PMF.Organs.Nodule
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
Gliricidia
Gliricidia
GliricidiaYoung
GliricidiaYoung
1.4 Child Components
1.4.1 MortalityRate
A constant function (name=value)
This model does not consider plant mortality.
1.4.2 SeedMortalityRate
A constant function (name=value)
1.4.3 Age
Accumulates a child function between a start and end stage.
Age is the age of the planting in years.
1.4.4 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
1.4.5 Arbitrator
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
1.4.6 IndividualTreeLiveWt
A class that divides all child functions.
Returns zero if nominator is zero, returns double.maxValue if denominator is zero.
1.4.7 Leaf
This organ is parameterised using a simple leaf organ type which provides the core functions of intercepting radiation, providing a photosynthesis supply and a transpiration demand.  It also calculates the growth, senescence and detachment of leaves.
1.4.8 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
1.4.9 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.10 Nodule
This organ simulates the root structure associate with symbiotic N fixing bacteria.  It provides the core functions of determining
N fixation supply and related costs.  It also calculates the growth, senescence and detachment of nodules.
This model calculated the N supply from biological N fixation by nodules within the plant root system.
2 Validation
The Gliricidia model has been tested using datasets from Malawi, Guadeloupe and USA that cover a wide range of weather conditions, soil, and agronomic systems.
2.1 Map
Experiment Name
Design (Number of Treatments)
Hamakuapoko
Harvest (1)
Guadeloupe
Harvest (3)
GuadeloupeGodet
Harvest (3)
Makoka
N x Maize (6)
This dataset measured above-ground biomass and leaf area, and calculated light interception, in a 2-year experiment on the island of Maui, Hawaii (
Harrington et al., 1995
).  The soil was a silty clay classified as an Oxic Haplustoll, with a surface soil (0-15cm) pH of 5-2 (1:1 soil:water), 2-4% organic C, and 0-18% total N.  The dry season from May to October receives one-third of the mean annual rainfall of 1200 mm. We used data only from the uncut treatment, as it was too hard to understand well enough the data for the coppice treatment.
The study (
Nygren et al., 2000
,
NYGREN et al., 1998
,
Garcia et al., 2001
)was carried out at the experimental farm of the Antillean research centre of the Institut National de la Recherche Agronomique (INRA) in Prise d’Eau, Guadeloupe (16°12′ N, 61°39′ W, 125 m a.s.l.). The soils are alluvial Ultisols with low acidity and relatively high cation exchange capacity.  The experiment was established in May 1993. Gliricidia sepium was planted using stakes cut from a near-by living fence. Between May 1993 and March 1995 the whole experiment was managed uniformly by means of partial prunings every 3–6 months according to the growth of trees, in order to avoid excessive shading of the grass layer. The experiment started 29th February, 1996, when the G. sepium trees were pruned completely. Three tree cutting regimes were then initiated: total removal of all leaves and branches above 0.5 m once a year, or every 6 months, and partial pruning every 2 months.
More frequent and severe cutting leads less biomass growth and biomass removed for residues (see
Duguma et al., 1988
qualitatively simulated for the Makoka site as a sensibility test).
The study (
NYGREN et al., 2000
,
Cruz, 1997
) was carried out at the experimental farm of the Antillean Research Centre of the Institut National de la Recherche Agronomique (INRA) in Godet, Guadeloupe (16°20′ N, 61°30′ W, 10 m a.s.l.). The soil is a slightly basic Vertisol with soil depth varying irregularly from 0.25 to 0.75 m. The climate is warm and subhumid.
The experiment was established in May 1989 by planting G. sepium cuttings from a nearby living fence in natural D. aristatum grassland. Gliricidia sepium was pruned every 2–2.5 months with successive full (100%) and partial (20–30%) defoliations, and the pruning residues were removed from the site. On April 1, 1996, the G. sepium trees were pruned completely. Three tree cutting regimes were then initiated: total removal of all leaves and branches above 0.5 m once a year, or every 6 months, and partial pruning every 2 months. Dichantium aristatum was cut monthly and the cut grass removed from the site.
More frequent and severe cutting leads less biomass growth and biomass removed for residues. see
Duguma et al., 1988
qualitatively simulated for the Makoka site as a sensibility test).
This agroforestry experiment (
Makumba et al., 2006
,
Makumba et al., 2007
,
Makumba et al., 2009
,
Akinnifesi et al., 2006
,
Akinnifesi et al., 2007
,
Akinnifesi et al., 2009
,
Mweta et al., 2007
,
Ikerra et al., 1999
) was continuously cropped with maize between establishment in 1992 and final available measurementsa in 2004. The trial includes three rates of N fertiliser with and without Gliricidia sepium intercropping. The study was conducted at Makoka Agricultural Research Station near Zomba in Southern Malawi. The soil had 42% clay, 46% sand and 1.42 g/cm3 bulk density.  The rainfall is unimodal with a mean rainfall of 937 mm. There was some P deficiency, demonstrated by a response to P fertilizer, so only the high P fertilizer treatment was simulated. There was also suspicion that P deficiency further developed quite later in the experiment.
Results indicated that, without Gliricidia, maize yield was low and decreased with time without N inputs. Gliricidia increased maize yields, but the effect took about 3 years to fully develop. N fertilization increased yields both with and without fertiliser. Annual trends reflected patterns of rainfall.
2.2 Combined Results
Combined results from all four sites indicates that the biomass growth of Gliricidia and amounts cut approximated those observed, which indicates that the Gliricidia model was adequate for these situations.
3 Sensibility
3.1 StripCropping
This sensibility test checks that Gliricidia crops provide N supply to neighbouring maize crops in strip cropping situations.  A simulation experiment has been constructed to simulate yield in Malawi.  Yield response to Gliricidia was positive and was generally a little higher than to N ferilisation, and temporal variations were close to those for fertilised crops, as expected. However, under different conditions, N supply from residues or feriliser may not be sufficient in some wet years, and competition for water may reduce crop yield below fertilised sole crops in dry years. Finally, the benefits of the trees may take a few seasons to be realised.
3.2 Temperature Response
The temperature requirements of Gliricidia sepium indicated by its distribution at native sites is for mean monthly temperatures in the range from approximately 21-29C (
Simons et al., 1994
). In addition we note: (1) Athanase Mukuralinda commented that gliricidia doesn’t grow above about 1600 m in Rwanda, which occurs about half way along our Rwanda transect, and (2) There is a record of it growing on a transect up to 1000 m in Guatemala in its native range (
Melchor-Marroqu'\in et al., 1999
). Gliricidia sheds leaves once night time temperatures fall below approximately 15C (
Whiteman et al., 0
), and the plant is susceptible to frosts unless pruned (
Stewart J.L. et al., 1992
). These tests should indicate a decrease in yield below an optimum average annual temperature, with negligible yield below about 19C.
Experiment Name
Design (Number of Treatments)
GuatemalaTransect
Cell (8)
RwandaTransect
Cell (10)
3.3 CuttingResponse
This sensibility test shows that biomass developoment is higher with a lower frequency of pruning and lower stump height of pruning, as showing in
Duguma et al., 1988
. This effect occurs because regrowth of shoots after pruning depends on carbohydrate remobilisation from roots and stumps. These reserves can be later replenished and added to once enough carbohydrate production and storage is reached in regrown shoots. This test applies these treatments to a hypothetical stand of gliricidia grown at the Makoka site in Malawi. In this test, cutting was done in factorial combinations of frequency (6-, 3-, and 1-monthly) and stump height (approximated by specifying the weight of stem remaining: 100 g, 50 g, or 25 g), while 10 g of leaf were left after pruning in all treatment combinations. These results may differ to those of
Duguma et al., 1988
due to to the much warmer climate at Ibadan, Nigeria, compared to Makoka, Malawi, and weights of stump remaining (which was not reported by
Duguma et al., 1988
). Severe water stress was predicted to limit gliricidia growth in late 2005.
Experiment Name
Design (Number of Treatments)
MakokaDuguma
Treat (9)
3.4 StockMassBalance
This sensibility test performs a livestock grazing mass balance check.
The MassBalanceCalculations script is intentionally before Stock in the simulation tree so that it will be called
before the STOCK model does its calculations for the day AND after the plant model has updated its biomass for the day. The script then
stores the plant biomass before and after stock calculations its daily variables. It then calculates the amount of plant biomass removed:
PlantBiomassRemoved = PlantBiomassBeforeStock - PlantBiomassAfterStock;
It then subtracts the animal intake.
Lost = PlantBiomassRemoved - Intake;
The Lost variable (which is output) should always be zero. The script will throw an exception if this isn't the case.
The graph below shows the biomass removed from the plant is the same as the plant biomass consumed by the animal which results in a
mass balance variable equal to zero.
4 Interface
4.1 Gliricidia
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
Akinnifesi, Festus K, Sileshi, Gudeta, Franzel, Steven, Ajayi, Oluyede C, Harawa, Rebbie, Makumba, Wilkson, Chakeredza, Sesbastian, Mngomba, Simon, de Wolf, Judith J, Chianu, Jonas N, others, 2009. On-farm assessment of legume fallows and other soil fertility management options used by smallholder farmers in southern Malawi. Agricultural Journal 4 (6), 260-271.
Akinnifesi, Festus Kehinde, Makumba, Wilkson, Sileshi, Gudeta, Ajayi, Oluyede C, Mweta, David, 2007. Synergistic effect of inorganic N and P fertilizers and organic inputs from Gliricidia sepium on productivity of intercropped maize in Southern Malawi. Plant and Soil 294 (1-2), 203-217.
Akinnifesi, FK, Makumba, W, Kwesiga, FR, 2006. Sustainable maize production using gliricidia/maize intercropping in southern Malawi. Experimental Agriculture 42 (4), 441-457.
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Cruz, Pablo, 1997. Effect of shade on the carbon and nitrogen allocation in a perennial tropical grass, Dichanthium aristatum. Journal of Experimental Botany 48 (1), 15-24.
Duguma, Bahiru, Kang, BT, Okali, DUU, 1988. Effect of pruning intensities of three woody leguminous species grown in alley cropping with maize and cowpea on an alfisol. Agroforestry Systems 6 (1-3), 19-35.
Garcia, Heli, Nygren, Pekka, Desfontaines, Lucienne, 2001. Dynamics of nonstructural carbohydrates and biomass yield in a fodder legume tree at different harvest intensities. Tree Physiology 21 (8), 523-531.
Harrington, R.A., Fownes, J.H., 1995. Radiation Interception and Growth of Planted and Coppice Stands of Four Fast-GrowingTropical Trees. Journal of Applied Ecology 32 (1), 1-8.
Ikerra, Susan T, Maghembe, Jumanne A, Smithson, Paul C, Buresh, Roland J, 1999. Soil nitrogen dynamics and relationships with maize yields in a gliricidia-maize intercrop in Malawi. Plant and Soil 211 (2), 155-164.
Makumba, W., Janssen, B., Oenema, O., Akinnifesi, F.K., Mweta,D., Kwesiga, F., 2006. The long-term effects of a gliricidia–maize intercropping system in Southern Malawi, on gliricidia and maize yields, and soil properties. Agriculture, Ecosystems and Environment 116, 85-92.
Makumba, Wilkson, Akinnifesi, Festus K, Janssen, Bert H, 2009. Spatial rooting patterns of gliricidia, pigeon pea and maize intercrops and effect on profile soil N and P distribution in southern Malawi. African Journal of Agricultural Research 4 (4), 278-288.
Makumba, Wilkson, Akinnifesi, Festus K, Janssen, Bert, Oenema, Oene, 2007. Long-term impact of a gliricidia-maize intercropping system on carbon sequestration in southern Malawi. Agriculture, ecosystems  and  environment 118 (1-4), 237-243.
Melchor-Marroqu'\in, JI, Vargas-Hernandez, JJ, Ferrera-Cerrato, R, Krishnamurthy, L, 1999. Screening Rhizobium spp. strains associated with Gliricidia sepium along an altitudinal transect in Veracruz, Mexico. Agroforestry systems 46 (1), 25-38.
Mweta, DE, Akinnifesi, FK, Saka, JDK, Makumba, W, Chokotho, N, 2007. Use of pruning and mineral fertilizer affects soil phosphorus availability and fractionation in a gliricidia/maize intercropping system. African Journal of Agricultural Research 2 (10), 521-527.
Nygren, Pekka, Vaillant, Victor, Desfontaines, Lucienne, Cruz, Pablo, Domenach, Anne Marie, 2000. Effects of nitrogen source and defoliation on growth and biological dinitrogen fixation of Gliricidia sepium seedlings. Tree Physiology 20 (1), 33-40.
NYGREN,P. ,CRUZ, P., DOMENACH, A. ,VAILLANT, V., SIERRA, J., 2000. Influence of forage harvesting regimes on dynamics of biological dinitrogen fixation of a tropical woody legume. Tree Physiology 20, 41-48.
NYGREN,P., CRUZ, P., 1998. Biomass allocation and nodulation of Gliricidia sepium under two cut-and-carry forage production regimes. Agroforestry Systems 41, 277-292.
Simons, A, Stewart, J, 1994. 2.2 Gliricidia sepium-a Multipurpose Forage Tree Legume..
Stewart J.L., Dunsdon, A.J., Hellin, J.J., Hughes, C.E., 1992. Wood Biomass Estimation of Central American Dry Zone Species. Tropical Forestry Paper 26..
Whiteman, P.C., Oka G.M., Marmim, S., Chand, S., Gutteridge, R.C., 0. Studies on the germination, growth and winter survival of Gliricidia maculata in southeastern Queensland.. International Tree Crops Journal.
