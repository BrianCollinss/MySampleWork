# Grapevine documentation

Source URL: https://docs.apsim.info/validation/Grapevine
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:51:59.719471+00:00

1 The APSIM Grapevine Model
The APSIM Grapevine Model
Junqi Zhu, Hamish Edward Brown
The New Zealand Institute for Plant & Food Research Limited (PFR), PO Box 845 Blenheim 7240, New Zealand
Correspondance: junqi.zhu@plantandfood.co.nz;Hamish.brown@plantandfood.co.nz
Introduction
Grapevine is one of the most economically important fruit crops worldwide, and its use in wine production has played an important cultural role in many parts of the world. Grapevines are now cultivated in more than 90 countries for wine, distilled liquors, juice, table grapes, and raisin production. Because of its global economic importance, the climate diversity of the producing regions, and a large amount of studies (from genomics to production practices), grapevine has emerged as a model perennial fruit crop species.
Objective
The model presented here was built to simulate the phenology of the vines, canopy development and light interception by the vine row, carbohydrate allocation, carbohydrate storage and remobilisation, and yield formation of grapevine (
Vitis vinifera
) as a model perennial crop.
It has the following objectives initially:
Capture the seasonal yield variations
Understand the long-term dynamics of yield and carbohydrate reserves under certain pruning systems
Capture the effects of vineyard management on yield and carbohydrate dynamics, e.g. summer and winter pruning, irrigation
Model construction
The APSIM Grapevine model was developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a model in much the same way that models can be coupled to construct a simulation. This means that dynamic composition of lower level process and organ classes (e.g. photosynthesis, leaf) into larger constructions (e.g. maize, wheat, sorghum) can be achieved by the model developer without additional coding.
A series of development was taken to adapt the source code to simulate the perennial woody plants. The current model including the following features:
A new carbohydrate allocation method to distribute carbohydrates to structural and non-structural organ components simultaneously based on sink strength and priority.
A yield module that uses the weather conditions in both the previous and current season as well as the carbohydrate status to determine potential bunch number per shoot, berry number per bunch and berry mass
Zhu et al., 2020
.For getting a reliable estimation of yield, simulations should start one season before the season in question.
A row and alley strip configuration to represent a typical vineyard setup with rows of grape vines growing in a strip of bare ground, interspersed with alleys of short grass. Roots from each strip are present beneath their neighbouring strip and compete for water and nutrients.
An adapted row crop light interception method based on Goudriaan (1977) to partition light interception between the row canopyies and the alley.
Flexibility in simulating different training systems by controlling the bud number per vine, dynamics of canopy height, canopy width and depth as user inputs.
An adapted phenology method that defines the start of endo-dormancy by critical photoperiod and then goes through each phenology phases progressively. Budburst was used for triggering canopy growth instead of crop emergence as in annual crops.
A flexible parameter optimization procedure that calls APSIM Next Generation through R bash command and optimises model parameters through using an R package.
Cautions for running the model
Vineyard conditions differ from one to the other due to differences in local climatic conditions, soil, rootstock, varieity, irrigtation, fertilization and canopy management practice. The current grapevine model rather provide a modelling framework to simulate phenology, canopy development, radiation interception, dry matter production and allocation, non-structural carbohydrate dynamics, yield components, soil water and nitrogen uptake in vineyard, than a universal model that can be applied anywhere without adjustments.
The current grapevine model was developed based on a rich phenology and yield dataset of Sauvignon blanc in New Zealand in five regions, mainly in the Marlborough region. The dataset were collected in irrigated and fertilized vineyards where water and nutrient stress were minimized. This was reflected in the simulation through irrigation and fertilization management script as well. For conditions with water and nutrient stress, further calibration based on local dataset would be needed.
Parameters for potential bunch number, berry number and berry mass determination were derived based on the long-term Sauvignon blanc trial in Marlborough. The general framework would hold for other varieties as well, but actual parameters needed to be refitted based on robust dataset for other varieties.
The current model can allocate vine roots to the alley and allocate the root of the alley crop to the vine row. However, the water and nitrogen competition between the vine and alley crop is based on the value of KL of each species, not based on the amount of root and root density presented in the soil zone. One option is change the KL as a function of root density or directly use root density in the source code. A good dataset of root distribution in different layor would be required. Further calibration of soil water, nitrogen and organic matter dynamics are needed.
Currently alley crop was represented by a generic plant model ‘Slurp’. The ‘Slurp’ model can take up water and nitrogen, but does not predict crop growth or yields. We forced its leaf area index to follow a reasonable growth pattern under the vineyard management in Marlborough New Zealand with periodic mowing events. Four different types were provided for the Slurp model: static crop, static tree, pasture, lurcern. see details in the Slurp node by right click and expanding the model structure.
The row crop light interception method assume the canopy shape is like a cube. So it is more suitable to use for verticle shoot prunned canopy.
Water stress was turned on for leaf expansion and photosynthesis, but nitrogen stress was turned off in the current model. However, nitrogen uptake and availability will still affect the organ development as the arbitrator is conducting biomass and nitrogen allocation automatically in the background.
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
Phenology
Models.PMF.Phen.Phenology
Structure
Models.PMF.Struct.Structure
Leaf
Models.PMF.Organs.Leaf
Shoot
Models.PMF.Organs.GenericOrgan
Cane
Models.PMF.Organs.GenericOrgan
Trunk
Models.PMF.Organs.GenericOrgan
StructuralRoot
Models.PMF.Organs.GenericOrgan
Berry
Models.PMF.Organs.ReproductiveOrgan
Root
Models.PMF.Organs.Root
MortalityRate
Models.Functions.Constant
SeedMortalityRate
Models.Functions.Constant
Arbitrator
Models.PMF.OrganArbitrator
1.2 Composite Biomass
Component Name
Component Type
AboveGround
Models.PMF.CompositeBiomass
BelowGround
Models.PMF.CompositeBiomass
1.3 Cultivars
Cultivar Name
Alternative Name(s)
SauvignonBlanc
SauvignonBlanc
Chardonnay
Chardonnay
Merlot
Merlot
PinotNoir
PinotNoir
PinotGris
PinotGris
Syrah
Syrah
1.4 Child Components
1.4.1 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
The phenology module starts with dormancy phase of the buds. This is followed by budding, flowering, fruit setting, berry development and canopy senescence phases. The dormancy phase include two stages: endo-dormancy and eco-dormancy. Endo-dormancy is defined as the period when buds are dormant due to physiological conditions, and eco-dormancy is defined when buds remain dormant due to unfavourable environmental conditions. The budburst stage in budding phase occurs when eco-dormancy is broken. For consistency in calculating the phenology stage in both Southern and Northern Hemispheres, we use days after winter solstice (DAWS) as the standard time in the model.
We assume the endo-dormancy stage of the bud starts in autumn when photoperiod drops below a critical value. The critical value depends on variety and was set to 13.1 h in the current model. Three budburst modelling approaches were tested using a phenology dataset: 1) forcing-only approach, which only describes the release of eco-dormancy; 2) chilling-forcing, which describes the release of endo- and eco-dormancy in sequence; 3) chilling-forcing-overlap, which describes the release of endo- and eco-dormancy in sequence plus the effect of chilling units on forcing accumulation through implementation of an exponential decrease function. Chilling-forcing approach was selected in the current grapevine model because it enabled the best predictions of the timing of budburst. In this approach, endo-dormancy is completed when the chilling requirement is satisfied, and eco-dormancy is completed when sufficient heat units are accumulated to force bud burst. A negative sigmoid function was selected for simulating the responses of chilling units and a growing degree approach was used to calculate forcing heat unit. Interpolated hourly temperature was used in the chilling unit calculation, see the description of InterPolationMethod below.
The flowering, fruit setting and berry development phases were simulated using a physiological development day target which is the number of days the phase would take to complete at optimal temperatures. The daily increment of development days is calculated using the Wang-Engle temperature response function
Wang et al., 1998
, which returns a value of one when temperatures are optimal and a value between zero and one at sub and supra optimal temperatures. Development days were used for all other temperature-dependent processes, e.g., leaf appearance and expansion.
After summer solstice, the photoperiod decreases. When photoperiod decreases to a critical value, the bud for the following season’s growth enters into endo-dormancy. However, the whole plant phenology cannot be set to endo-dormancy at this time because the current year’s shoots and berries are still growing. Although it would be tidy to develop two different phenological clocks, one for the bud and one for the plant in growth, we decided to work around it by resetting the plant phenology phase into endo-dormancy at the winter pruning event which was defined in the management script with user input time. The chilling accumulation between the critical photoperiod and the pruning event was captured by a function and then added to the total chill accumulation in the endo-dormancy phase.
1.4.2 Structure
The structure model simulates morphological development of the plant to inform the Leaf class
when and how many leaves and branches appear and provides an estimate of height.
The structure module defines the bud number per vine, rate of main stem primordia initiation rate, phyllochron (development day interval between two successive leaf appearance), branching rate and branching mortality, final leaf number and canopy height.
Phyllochron was defined by a phase look up function in combination with a linear interpolation function. The phase look up function defines when new leaf appearance will happen, which is currently defined between budburst and véraison. The linear interpolation function captured the phyllochron difference between first five ranks and the ramaining ranks.
Branching rate was calculated as the product of three components: potential branching rate, the effect of leaf area per vine, and the effects of retained bud number per meter row
1.4.3 Leaf
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
The grapevine model used the phytomer-based Leaf organ class. It predicted the appearance, expansion and senescence of cohorts of leaves at each position on the primary shoot and estimated how many leaves were present in each cohort based on branching rates. Branch leaves were treated the same as main-stem leaves appeared at the same time.
The maximum area of each cohort was currently set as a function of relative node position in respect to the final leaf number. The growth duration, lag duration, senescence duration and specific minimum and maximum leaf area were parameterised based on our field experiment, see Datasets used for Model calibration and validation. For correctly modelling leaf senescence and the dynamics of leaf area, a photoperiod acceleration effect was added. The photoperiod acceleration effect included and reflected the effects of the time of harvest on leaf senescence as well, as we observed that leaves would stay green much longer if the fruit were not harvested. Simulation of leaf senescence has highly importance to the replenishment of non-structural carbohydrate reserves.  Other leaf properties were parameterised to ensure minimum nitrogen or water stress on leaf expansion in the current grapevine model as the water and nitrogen dynamic part require further calibration. However, the model do include a linear water stress on leaf expansion and photosynthesis.
The potential total DM demand of a leaf was calculated based upon the delta leaf area increase per day (constrained by stress) times the mean of maximum and minimum specific leaf area given in UI. The total DM demand was then divided into structural DM demand and Metabolic DM demand based on the fraction of structural DM in total leaf weight. The priority factor q for leaf structural and metabolic demand was set to a high value (1.6) as it is the immediate source of photosynthetic and has the highest priority early in the season
Buwalda, 1991
Lakso et al., 2007
. Non-structural reserves were not considered in leaf current model.
The dynamics of seasonal canopy width and canopy depth were defined in the leaf class based on our field observation.
The dynamics of seasonal nitrogen concentration was taken from
Holzapfel et al., 2019
.
1.4.4 Shoot
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Shoot was represented by a Generic Organ class.
The structural biomass demand of the shoot module was calculated by a delta function that takes the difference of yesterday and today’s value of a fitted biomass growth curve. The structural biomass growth curve was represented by a beta growth function
Yin et al., 2003
fitted on individual shoot weight over time and times the shoot population per square meter. Shoot population per square meter was calculated internally based on shoot number per vine and vine density.
Note the shoot dry matter including both the primary and secondary shoots. However, we did not separate the biomass of primary and secondary shoot during simulation. For capturing the reduction of shoot biomass under high retained bud number per meter row caused both by smaller primary shoot and less lateral shoots
Greven et al., 2014
, the priority factor for shoot DM demand was set to a very low value (4e-3).
Parameters for shoot non-structural DM demand, e.g. priority factors were set as half of the trunk and root.
1.4.5 Cane
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Cane, Trunk and Structural root module were all represented by the Generic Organ class, which has properties of biomass status and daily biomass demand and supply. The structural DM demand of cane and trunk were calculated by their radius (r, unit m) derived from their DM and length, daily growth rate of the radius (dr/dt, m d-1), length (l, m) and wood density (ρ, g m-3)
Cieslak et al., 2011
.
The priority factor for structural DM demand for a certain organ, determined as 0.7 for all three organs. Daily growth rate of the radius was calculated based on the trunk circumference increment in 14 years measured on 800 field grown irrigated Sauvignon blanc vines.
The structural carbohydrate demand of the structural root was calculated by the structural demand of the trunk times the structural root/trunk ratio (set to one in this model). The structural root/trunk ratio may vary between vineyards and training systems and further work is required to capture this in the model. The non-structural carbohydrate (NSC) DM demand was modelled as an active competing sink
Cieslak et al., 2011
, and the parameters were kept the same for those three organs. NSC included total soluble carbohydrates and starch. The rate of non-structural DM synthesis depended on organ size and limited by overloading.
For capturing the fast recovery of carbohydrate reserves after flowering
Greven et al., 2016
, ksynthesis was simulated using a beta function. Parameters were optimized based on the measured dynamics of non-structural carbohydrate in trunk and root by Greven et al. (2016).
For potential carbohydrate retranslocation, 3.7 % of the total carbohydrate storage in each organ per day (DM retranslocation factor) was set available to be retranslocated. Actual retranslocation depends on the daily carbohydrate supply and demand differences. Carbohydrate supplies from organ senescence and photosynthesis will be used first before using carbohydrate storage.
The biomass of retained cane for the following season was reset by the winter pruning events based on input cane diameter in the UI for the cane-pruned vines. The biomass of trunk and structural root keeps growing each year. In addition, retained cane can be parameterized as cordon and keep growing each year as well.
1.4.6 Trunk
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.7 StructuralRoot
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note this represents the structural roots of all the plant in the population. The structural root are primarily considered as storage organs, its researves can be made available to boost plant growth in spring and/or following a defoliation.
The structural root is separated from the mian root class because the current root class can not handle the biomass retranslocation while a generic organ can. The biomass of the structural root is expressed as gram per square meter, while the initial dry mass for the root class is expressed at per plant level.
structural roots in grapevine are perennial. It can grow in diamention and biomass. Its structural growth rate was calculated based on the the trunk demand times structural root and trunk ratio in this model. Its storage biomass typicall decrease in spring and refill after veraison.
1.4.8 Berry
This organ uses a generic model for plant reproductive components.  Yield is calculated from its components in terms of organ number and size (for example, grain number and grain size).
The final yield was calculated by bunches per shoot, shoots per vine, berries per bunch,and berry fresh or dry weight.
The effects of carbon status on yield compoent has been included but may requires further calibration. Bunch number, berry number and potential berry fresh weight were determined by weather conditions at critical periods around flowerings of the previous and current season, see details at
Zhu et al., 2020
. Furthermore, in the current model, carbon effects represented by total carbon supply and demand were added in the calculation of bunch number, berry number and potential berry fresh weight.
Berry dry mass accumulation following the source-sink carbon allocation rules. Brix was calculated based on the ratio of berry dry weight to fresh weight. Total titratable acid was simulated based on thermaltime accumulation after veraison follwoing a negative exponetial curve.
A long-term phenology and yield monitoring trial using both two-cane and four-cane trained vertically shoot positioned (VSP) Sauvignon blanc vines was established in four vineyards in Marlborough, New Zealand in 2004, and was used for calibrating the berry module. Phenology, bunch number, berry mass, yield and meteorology records were collated. A multivariable mixed linear model was used to assess the relationship between various yield components and weather conditions. The critical periods for each yield component and weather factor were optimised based on the maximum likelihood returned from the mixed linear model. The optimised critical periods of temperature for all yield components occurred mainly before 50 % flowering either in the previous season (during inflorescence initiation) and the current season, indicating the importance of the pre-flowering period on yield formation. Out of all weather factors, maximum daily temperature had the largest effect on bunch number and overall yield and strongly influenced berry number and bunch mass. Rainfall near flowering time had a negative effect on berry mass and bunch mass, but post-flowering rainfall had a strong positive effect.
1.4.9 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
Note that this organs is parameterised to represents all the fine roots of the plant.  The root organ is reponsible for uptake but can also supply both N and DM from its non-structural biomass.  The Fibrous root organ is used for simulating biomass storage and remobilisation because the current root class can not handle the biomass retranslocation.
The dynamics of root biomass was based on the root length dynamics measurement done on mature concord vines in UC Davis by comos et al., 2005. both root growth rate and mortality rate have annual cycles.Maintenance was set to zero as it was parameterized insided the senescence rate.
the maximum root biomass during the season is around 60 g per plant, estimated by lakso et al., 2008.
the initial weight of the root was increased as we start the simulation in February with no leaves.the start of simulation in February was for correctly simulating the budburst in the first year. the initial weight was given to ensure that the peak of the biomass can cycle around 30 g/m2.
1.4.10 MortalityRate
A constant function (name=value)
1.4.11 SeedMortalityRate
A constant function (name=value)
1.4.12 Arbitrator
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
2 Validation
2.1 Sauvignonblanc
Experiment Name
Design (Number of Treatments)
RetainedNodeNum
Node (5)
Marlborough_2Cane
Climate (4)
Marlborough_3Cane
Climate (2)
Marlborough_4Cane
Climate (4)
RPC_2000_2002
Irri (2)
RPC_2002_2006
Irri (3)
Renwick_2002_2007
Irri (4)
OtherRegions
Climate (5)
RPC corresponding to Central Rapaura in the manuscript "Developing perennial fruit crop models in APSIM Next Generation using grapevine as an example" that we just submitted.
2.2 Chardonnay
Experiment Name
Design (Number of Treatments)
Chardonnay
Climate (6)
2.3 Merlot
Experiment Name
Design (Number of Treatments)
Merlot
Climate (2)
2.4 PinotNoir
Experiment Name
Design (Number of Treatments)
PinotNoir
Climate (7)
2.5 PinotGris
Experiment Name
Design (Number of Treatments)
PinotGris
Climate (3)
3 SheepInVineyard
This simulation example was made based on the Stock.apsimx AgistmentGrazing simulation.
Agistment Grazing refers to a practise of bringing stock onto and then off a paddock based on a set of criteria. The location and management of the stock when not on the paddock are ignored.
Bring sheep to vineyard is a common practice in NZ especially in winter to graze the interrows. Sometimes growers also use sheep a leaf plucking method. This simulation is more to test whether there is issues with adding sheep into the grapevine model due to the row structure and see what components will the sheep eat.
Because we are not concerned with the disposition of the stock while not grazing the forage crop, this a single-paddock simulation. The paddock contains a forage crop, in this example we use plantain, that is irrigated and fertilised. The forage is sown in spring of the first year and is rejuvenated thereafter at the frequency set on the sowing rule in the paddock. In this simulation a new mob of animals are bought every time the forage is ready to be grazed and sold after each grazing event (note that here bought and sold only refer to acquiring and  disposing of animals not the financial transactions of such actions). The user sets the characteristics of the stock, the grazing rules and if/how any supplementary feeding is done.
This example includes:
setting several commonly-needed stock characteristics
buying stock
finding out the current location of the stock
changing the location of the stock
feeding supplements based on a range of criteria
selling stock
4 Sensibility
Experiment Name
Design (Number of Treatments)
IrrigationResponse
Node (5)
This simulation experiment is set up to test the sensibility of the model output to water stress. The days of interval between two irrigation events was set as the experimental factor. The results show the model present reasonable water stress responses for leaf area, biomass dynamics of different organ, profile soil water storage, and non-structural carbohydrate. one caution should be note is for the Brix values. Brix was calculated based on water content, which is related to the berry dry weight and fresh weight. Dry weight response strongly to the water stress as carbon assimilation was reduced. For the response of fresh weight to water stress, only a emipirical function based on our rule of thumb was added. The effects of carbon status on berry fresh weight needed to be added based on experimental data as well.
5 Interface
5.1 Grapevine
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
5.2 SowingParameters
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
5.3 Phenology
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
SetPhaseCompletionDate
void SetPhaseCompletionDate(String completionDate, String PhaseName)
Method to set the DateToProgress property in nominated phase which forces the phase to ignore its own mechanisum and complete on the nominated day
GetPhaseTable
DataTable GetPhaseTable()
6 References
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Buwalda, JG, 1991. A mathematical model of carbon acquisition and utilisation by kiwifruit vines. Ecological Modelling 57 (1-2), 43-64.
Cieslak, Mikolaj, Seleznyova, Alla N, Hanan, Jim, 2011. A functional-structural kiwifruit vine model integrating architecture, carbon dynamics and effects of the environment. Annals of Botany 107 (5), 747-764.
Greven, Marc M, Neal, Sue M, Tustin, D Stuart, Boldingh, Helen, Bennett, Jeff, Vasconcelos, Maria Carmo, 2016. Effect of postharvest defoliation on carbon and nitrogen resources of high-yielding Sauvignon Blanc grapevines. American Journal of Enology and Viticulture 67 (3), 315-326.
Greven, MM, Bennett, JS, Neal, SM, 2014. Influence of retained node number on S auvignon B lanc grapevine vegetative growth and yield. Australian Journal of Grape and Wine Research 20 (2), 263-271.
Holzapfel, Bruno Peter, Smith, Jason P, Field, Stewart K, 2019. Seasonal vine nutrient dynamics and distribution of Shiraz grapevines: This article is published in cooperation with the 21th GIESCO International Meeting, June 23-28 2019, Thessaloniki, Greece. Guests editors: Stefanos Koundouras and Laurent Torregrosa. Oeno One 53 (2).
Lakso, Alan N, Intrigliolo, Diego, Eissenstat, David M, 2007. Modeling concord grapes with VitiSim, a simplified carbon balance model: understanding pruning effects. VIII International Symposium on Modelling in Fruit Research and Orchard Management 803, 243-250.
Lawless, Conor, Semenov, MA, Jamieson, PD, 2005. A wheat canopy model linking leaf area and phenology. European Journal of Agronomy 22 (1), 19-32.
Wang, Enli, Engel, Thomas, 1998. Simulation of phenological development of wheat crops. Agricultural Systems 58 (1), 1-24.
Yin, Xinyou, Goudriaan, JAN, Lantinga, Egbert A, Vos, JAN, Spiertz, Huub J, 2003. A flexible sigmoid function of determinate growth. Annals of botany 91 (3), 361-371.
Zhu, Junqi, Fraysse, R'emi, Trought, Michael, Raw, Victoria, Yang, Linlin, Greven, Marc, Martin, Damian, Agnew, Rob, 2020. Quantifying the seasonal variations in grapevine yield components based on pre-and post-flowering weather conditions. Oeno One 54 (2), 213-230.
