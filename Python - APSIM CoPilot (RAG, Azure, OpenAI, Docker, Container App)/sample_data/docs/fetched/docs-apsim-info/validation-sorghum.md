# Sorghum documentation

Source URL: https://docs.apsim.info/validation/Sorghum
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:59.291627+00:00

1 The APSIM Sorghum Model
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
The APSIM NextGen Sorghum Model – Structure and Function
Hammer, G.L., McLean, G., Brider, J., van Oosterom, E., Wu, A., Holzworth, D.  July 2022
The APSIM-sorghum model is based on a framework of the physiological determinants of crop growth and development
Charles-Edwards, 1982
, is focused at organ scale, and has been detailed in
Hammer et al., 2010
and
Hammer et al., 2019
. It generates the phenotype of a crop as a consequence of underlying physiological processes (Fig. 1) by using the concept of supply and demand balances for light, carbon, water, and nitrogen
Hammer et al., 2001
. The approach is focused around quantifying capture and use of radiation, water, and nitrogen within a framework that predicts the dynamics of crop development and the realized growth of major organs based on their potential growth and whether the supply of carbohydrate and nitrogen can satisfy this potential. Demand for resources is defined by potential organ growth and potential supply by resource capture (
Monteith et al., 1977
;
Passioura, 1983
;
Monteith et al., 1986
) (Figure. 1). Arbitration rules and organ level responses are invoked when resource capture cannot satisfy demand. The APSIM-sorghum model retains some features and concepts of earlier models:
Sinclair, 1986
; [rosenthal_sorkam:_1989];
Birch et al., 1990
;
Sinclair et al., 1992
;
Chapman et al., 1993
;
Hammer et al., 1994
, but has been adapted and redesigned to generate a more explanatory approach to the modelling of the underlying physiology
Hammer et al., 2006
.
APSIM-sorghum operates via the dynamic interaction of crop development, crop growth, and crop nitrogen with soil and weather attributes (Fig. 1). Predictive schemas can be separated into crop growth and development dynamics (Fig. 1a) and crop nitrogen dynamics (Fig. 1b) for purposes of description, but the interactions between these major components are critical.
Image Reference:
Hammer et al., 2010
Crop Growth and Development Dynamics
Phenology is simulated through a number of development stages by using a thermal time approach
Muchow et al., 1990
;
Hammer et al., 1994
, with the temperature response characterized by a base (Tb), optimum (Topt), and maximum (Tm) temperature.
Hammer et al., 1993
and
Carberry et al., 1993
reported values of Tb, Topt, and Tm for sorghum of 11, 32, and 42°C, respectively. The thermal time target for the phase between emergence and panicle initiation is also a function of day length (
Hammer et al., 1989
;
Ravi Kumar et al., 2009
), and its duration, when divided by the plastochron (°C per leaf), determines total leaf number once an allowance for leaf initials in the embryo has been included. Total leaf number multiplied by the phyllochron (°C per leaf) determines the thermal time to reach flag leaf stage, which is thus an emergent property of the model. Timing of the stages anthesis, and start and end of grain filling are also simulated through thermal time targets (
Muchow et al., 1990
;
Hammer et al., 1994
;
Ravi Kumar et al., 2009
). Drought stress and N stress can both reduce the leaf appearance rate and hence delay phenology during the vegetative stages (
Craufurd et al., 1993
;
van Oosterom et al., 2010
).
Canopy development is simulated on a whole plant basis given the fertile tiller number and leaf size-leaf number distribution (
Carberry et al., 1993
;
van Oosterom et al., 2001
). Expected fertile tiller number can be input or predicted using a dynamic tillering prediction routine. The total plant leaf area at any time is calculated as the sum of main culm and tiller leaf area. Main culm leaf area is determined from the number of fully expanded leaves on the main culm and their size, plus an adjustment for the area of expanding leaves in the whorl (
Hammer et al., 1993
). The area of leaves on tillers is determined from the number of fertile tillers and the size of their leaves, which is determined via the allometric association of leaf size distribution on each tiller with that on the main culm. The dynamic tillering routine predicts the total number of tillers produced from a relationship combining the genetic propensity to tiller (input) with availability of surplus assimilate at the time of tiller outgrowth early in crop development (
Alam et al., 2014
). Plant assimilate status is estimated from the balance between radiation-driven supply and main culm leaf expansion-driven demand. Hence, tillering is favoured in high radiation-low temperature conditions as found by
Kim et al., 2010
. The number of tillers surviving to produce grain is then dependent on internal plant competition for available assimilate, which will be influenced by the rate of leaf expansion for the entire plant and the plant density. The trajectory of specific leaf area (SLA cm2 g-1) is a good indicator of extent of internal plant competition and is used to drive tiller outgrowth cessation until a balance is reached (
LAFARGE et al., 2002
). It is also used to reduce potential leaf size on the main culm in situations of reduced assimilate availability per plant (eg under high density) where SLA will reach boundary conditions.
The number of fully expanded leaves at any time is the product of thermal time elapsed since emergence and the leaf appearance rate (phyllochron). This enables calculation of leaf area per plant at any time as outlined above. Actual crop leaf area is the product of plant density and leaf area per plant. Green leaf area index (LAI) is the difference between the total plant leaf area and the senesced leaf area. Under drought stress, the crop will initially cease expanding new leaves, thus reducing transpiration demand, and then commence senescing leaves until demand for transpiration no longer exceeds supply from uptake (
Hammer et al., 2001
).
Aboveground biomass accumulation is simulated as the minimum of light-limited or water-limited growth. In the absence of water limitation, biomass accumulation is the product of the amount of intercepted radiation (IR) and its conversion efficiency, the radiation use efficiency (RUE). The fraction of incident radiation intercepted is a function of the LAI and the canopy extinction coefficient (k), which is a measure of canopy structure (
LAFARGE et al., 2002
). The effects of N supply on crop growth are implicitly incorporated in this approach. Nitrogen limitation will reduce leaf area growth and hence LAI and IR. It can also reduce RUE, which is a function of the N status of the leaves (
Muchow et al., 1994
;
Sinclair et al., 1992
). Sinclair and Muchow
Sinclair et al., 1999
reviewed studies that had measured RUE in many crops and noted a consistent value of 1.25 g MJ-1 for triple-dwarf sorghum under optimum growing conditions. The flexibility of the object-oriented template also allows simulation of crop biomass accumulation via diurnal canopy photosynthesis models where this is required, as in the studies of
Sinclair et al., 2005
,
Hammer et al., 2009
, and
Wu et al., 2019
.
Under water limitation, aboveground biomass accumulation is the product of realized transpiration and its conversion efficiency, biomass produced per unit of water transpired, or transpiration efficiency (TE). It is necessary to adjust TE to allow for the prevailing vapor pressure deficit (vpd) (
Tanner et al., 1983
;
Kemanian et al., 2005
). Numerous studies in sorghum (
Tanner et al., 1983
;
Hammer et al., 1997
) have found a standard value of 9 Pa for the TE coefficient in sorghum, so that at a vpd of 2 kPa a TE of 4.5 gm-2 mm-1 results. The water supply accessible to the plant depends on the effective rooting depth and the rate at which soil water can be extracted from the soil by the roots. The potential extraction rate is related to the soil water content via an exponential function, parameterized via an extraction decay constant (kl) that incorporates effects of both soil hydraulic conductivity and root length density on water uptake (
Passioura, 1983
;
Monteith et al., 1986
;
Robertson et al., 1993
;
Hammer et al., 2001
). Water extraction occurs from multiple layers, and the total extraction is the sum of that calculated for individual layers. As RUE and TE are based on aboveground biomass only, root mass is not explicitly modelled, but is added to the aboveground biomass accumulation according to a root/shoot ratio that declines with successive growth stages of the crop.
Daily aboveground biomass accumulation is partitioned to plant parts in allometric ratios that depend on the growth stage of the crop via functions that have been found to describe these ratios well ([jones_ceres-maize:_1986]). Before the flag leaf stage, new biomass is allocated to stem and leaves. Leaves are partitioned a fraction that decreases with increasing node number up to a maximum absolute allocation to leaf that is set by the ratio of the new leaf area to be grown (described above) and a minimum specific leaf area (cm2 g-1). The remaining biomass is partitioned to stem and rachis. The stem fraction incorporates leaf sheaths, but a distinct allocation to rachis commences after panicle initiation. Between flag leaf and anthesis, accumulated biomass is allocated to the stem and rachis in a fixed ratio.
Grain yield is simulated as the product of grain number and grain size. Maximum grain number is a function of the change in plant biomass between panicle initiation and start grain filling ([rosenthal_sorkam:_1989]), while grain size is determined by grain growth rate, effective grain filling period, and redistribution of assimilates postanthesis (
Heiniger et al., 1997
). If grain mass demand for a day exceeds the daily increase in biomass, the shortfall will first be met through translocation from stem and, if that is insufficient to meet demand of the grain, through translocation from leaves, accelerating their senescence. Conversely, if the daily increase in biomass exceeds the grain mass demand, the excess biomass production is allocated to the stem.
Crop Nitrogen Dynamics
Crop N dynamics are modelled based on a physiological approach that accounts for the fact that the bulk of reduced N present in leaves is associated with photosynthesis structures and enzymes (
GRINDLAY, 1997
) (Fig. 1b). The rate of light-saturated net photosynthesis has been shown to be a linear function of the amount of leaf N per unit leaf area (specific leaf nitrogen [SLN]), until a species-specific maximum rate of photosynthesis has been reached (
Sinclair et al., 1989
;
Anten et al., 1995
;
GRINDLAY, 1997
). Expressing crop N demand relative to canopy expansion thus provides a physiological link between crop N status, light interception, and dry matter accumulation. In addition, the cardinal SLN values for new leaf growth and for leaf death in response to N deficiency are independent of growth stage (
van Oosterom et al., 2010
).
During the preanthesis period, only stems (including rachis) and leaves are expanding, and their N demand is met in a hierarchical fashion (
van Oosterom et al., 2010
). First, structural N demand of the stem (and rachis) is met, as structural stem mass is required to support leaf growth. Structural stem N demand is represented by the minimum stem N concentration. If insufficient N has been taken up to meet structural stem N requirement, N can be translocated from leaves by dilution or, in extreme cases of early season N deficiency, by leaf senescence. Second, the N demand of expanding new leaves will be met, and this is determined from their critical SLN. Any additional N uptake will first be allocated to leaves to meet their target SLN and then to stem. For leaves, this N uptake represents "luxury" uptake that can occur after full expansion of a leaf, and which does not affect growth and development (
van Oosterom et al., 2010
). This hierarchical allocation of N is consistent with observations that under N stress a relatively larger proportion of N is allocated to the leaves (
van Oosterom et al., 2010
). Hence, preanthesis N allocation ratios are a consequence of model dynamics, rather than a model input.
After anthesis, grain becomes the major sink for N, and grain N demand is determined as the product of grain number and N demand per grain. During the first part of grain filling, N demand per grain is constant and independent of grain growth rate and N status of the crop (
van Oosterom et al., 2010
). At this time embryonic and endosperm cells are dividing, so that the accumulation of structural (metabolic) proteins in the grain is the key driver. During the second half of grain filling, grain N demand is linked with grain growth rate as cell division and simultaneous storage of carbohydrate and proteins assumes a greater role (
Martre et al., 2006
). Grain protein content can thus vary depending on the N supply–demand balance and the carbohydrate supply to the grain. Grain N demand is initially met through stem (plus rachis) N translocation, and if this becomes insufficient, then N translocation from leaf can occur. Maximum N translocation rates from stem and per unit leaf area are a function of the N status of these organs, so that sink demand determines the amount of leaf area that is senescing at any one time (
van Oosterom et al., 2010
). The source regulation of N translocation follows a first-order kinetic relationship that is representative of enzyme activity. Leaf SLN thus declines to its structural (minimum) level, and the amount of leaf area senesced, in the absence of other factors that can affect senescence, such as water limitation and shading, depends on the N supply–demand balance.
The daily rate of crop N uptake is the minimum of demand for N by the crop and potential supply of N from the soil and senescing leaves, capped at a maximum N uptake rate (
van Oosterom et al., 2010
). Potential N supply from the soil depends on the available soil N through the profile and on the extent to which roots have explored the soil. N supply from the soil is calculated from the combination of passive uptake, through mass flow of N taken up with the transpiration, and active uptake if there is a deficiency (
Van Keulen et al., 1987
). Soil N transformations and their modelling in APSIM have been detailed by
Probert et al., 1998
.
Crop Model Applications
The APSIM sorghum model has been tested (
Hammer et al., 2010
) and used extensively to support research and decision-making in agronomy (
Meinke et al., 2000
;
Nelson et al., 2002
;
Whish et al., 2005
) and plant breeding (
Chapman et al., 2000
,
Chapman et al., 2000
,
Chapman et al., 2003
;
Hammer et al., 2005
,
Hammer et al., 2016
). Comprehensive in silico adaptation (GxExM) studies have been reported for current
Hammer et al., 2014
and future
Hammer et al., 2020
climates. Sorghum crop modelling has evolved in capability and reached a credible level of acceptance in agronomy. This has been associated with strong connections between crop physiological experimentation, model development, and agronomists.
While advances in knowledge, model improvements, and enhanced interactions with decision-makers will undoubtedly further advance the utility of modelling in agronomy, it is the potential to add significant value to the revolution in plant breeding associated with genomic technologies that is the new modelling frontier (
Hammer et al., 2019
). This will require models where capturing biological understanding in a crop growth and development context is as important as the predictive capability of the model—the right answer for the right reason. Models developed for agronomic application will likely not be sufficient. Models with more robust biological underpinning and the ability to link parameters with the genetic architecture of adaptive traits in a stable manner will come to the fore (
Cooper et al., 2014
;
Messina et al., 2018
).
The model is constructed from the following list of software components. Details of the implementation and model parameterisation are provided in the following sections.
1.1 Plant Model Components
Component Name
Component Type
Arbitrator
Models.PMF.OrganArbitrator
Phenology
Models.PMF.Phen.Phenology
Grain
Models.PMF.Organs.ReproductiveOrgan
Root
Models.PMF.Organs.Root
Leaf
Models.PMF.Organs.SorghumLeaf
Rachis
Models.PMF.Organs.GenericOrgan
Stem
Models.PMF.Organs.GenericOrgan
TotalPlantDemand
Models.Functions.AddFunction
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
1.3 Cultivars
Cultivar Name
Alternative Name(s)
Buster
Buster
early
early
medium
medium
late
late
ATX623xRTX430
ATX623xRTX430
A35xQL36
A35xQL36
QL41xQL36
QL41xQL36
QL39xQL36
QL39xQL36
M35-1
M35-1
CSH13R
CSH13R
dekalb_DK55
dekalb_DK55
texas_RS610
texas_RS610
pioneer_s34
pioneer_s34
texas_671
texas_671
Scorpio
Scorpio
Apollo
Apollo
Bazley
Bazley
Taurus
Taurus
P85G33
P85G33
P84G22
P84G22
P84G99
P84G99
P86G56
P86G56
MR43
MR43
N_14NUS01
N_14NUS01
N_14NUS02
N_14NUS02
N_14NUS03
N_14NUS03
N_14NUS04
N_14NUS04
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
1.4.3 Grain
This organ uses a generic model for plant reproductive components.  Yield is calculated from its components in terms of organ number and size (for example, grain number and grain size).
1.4.4 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
1.4.5 Leaf
SorghumLeaf reproduces the functionality provided by the sorghum and maize models in Apsim Classic.
It provides the core functions of intercepting radiation, producing biomass through photosynthesis, and determining the plant's transpiration demand.
1.4.6 Rachis
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.7 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.8 TotalPlantDemand
A class that returns the sum of its child functions.
1.4.9 MortalityRate
A constant function (name=value)
1.4.10 SeedMortalityRate
A constant function (name=value)
2 Validation
2.1 Combined Results
Simulation results for the combined datasets from the various countries are shown in the following graphs.  The model is able to adequately capture the influence of growing conditions (soil, climate) and management (population, Nitrogen, irrigation, sowing date).
2.2 Hermitage
Experiment Name
Design (Number of Treatments)
HE1
Cultivar (3)
HE2
Cultivar (3)
HE3
Cultivar (3)
HE4
Cultivar (3)
HE5
Cultivar (2)
HE6
Cultivar (2)
HE7
Cultivar (2)
HE8
Cultivar (2)
Experiment: HE1
Description: HE1 - High N/Irrig. - 3 Indian/Aust. Cultivars
Start Date: 1/12/1996
End Date: 30/06/1997
SimulationName
Cultivar
Sorghum_HE1_T1
QL41xQL36
Sorghum_HE1_T2
Buster
Sorghum_HE1_T3
M35-1
Experiment: HE2
Description: HE2 - Low N/Irrig. - 3 Indian/Aust. Cultivars
Start Date: 1/12/1996
End Date: 30/06/1997
SimulationName
Cultivar
Sorghum_HE1_T1
QL41xQL36
Sorghum_HE1_T2
Buster
Sorghum_HE1_T3
M35-1
Experiment: HE3
Description: HE3 - High N/No Irrig. - 3 Indian/Aust. Cultivars
Start Date: 1/12/1996
End Date: 30/06/1997
SimulationName
Cultivar
Sorghum_HE3_T1
QL41xQL36
Sorghum_HE3_T2
Buster
Sorghum_HE3_T3
M35-1
Experiment: HE4
Description: HE4 - Low N/No Irrig. - 3 Indian/Aust. Cultivars
Start Date: 1/12/1996
End Date: 30/06/1997
SimulationName
Cultivar
Sorghum_HE4_T1
QL41xQL36
Sorghum_HE4_T2
Buster
Sorghum_HE4_T3
M35-1
Experiment: HE5
Description: HE5 - High N/Irrig. - 3 Indian/Aust. Cultivars
Start Date: 18/11/1997
End Date: 15/04/1998
SimulationName
Cultivar
Sorghum_HE5_T1
Buster
Sorghum_HE5_T2
M35-1
Sorghum_HE5_T3
CSH13R
Experiment: HE6
Description: HE6 - Low N/Irrig. - 3 Indian/Aust. Cultivars
Start Date: 18/11/1997
End Date: 15/04/1998
SimulationName
Cultivar
Sorghum_HE6_T1
Buster
Sorghum_HE6_T2
M35-1
Sorghum_HE6_T3
CSH13R
Experiment: HE7
Description: HE7 - High N/No Irrig. - 3 Indian/Aust. Cultivars
Start Date: 18/11/1997
End Date: 15/04/1998
SimulationName
Cultivar
Sorghum_HE7_T1
Buster
Sorghum_HE7_T2
M35-1
Sorghum_HE7_T3
CSH13R
Experiment: HE8
Description: HE8 - Low N/No Irrig - 3 Indian/Aust. Cultivars
Start Date: 17/11/1997
End Date: 15/04/1998
SimulationName
Cultivar
Sorghum_HE8_T1
Buster
Sorghum_HE8_T2
M35-1
Sorghum_HE8_T3
CSH13R
2.3 Lawes
Experiment Name
Design (Number of Treatments)
LE14
Cv (3)
LE15
Cv (3)
LE17
Cv (2)
LE19
(4)
LE20
(4)
LE21
Cv x Fert (6)
Experiment: LE14
Description: Early Sow - 3 Indian/Aust Cultivars
Start Date: 5/09/1996
End Date: 19/02/1997
SimulationName
Cultivar
Lawes1996EarlyCvBuster
Buster
Lawes1996EarlyCvM351
M35-1
Lawes1996EarlyCvQL41xQL36
QL41xQL36
Experiment: LE15
Description: Late Sow - 3 Indian/Aust Cultivars
Start Date: 5/09/1996
End Date: 8/05/1997
!! This experiment had severe storm at flowering. Question mark over post anthesis data !!
SimulationName
Cultivar
Lawes1996LateCvBuster
Buster
Lawes1996LateCvM351
M35-1
Lawes1996LateCvQL41xQL36
QL41xQL36
Experiment: LE17
Description: LE17 Growth Analysis- Late Sow - 2 Indian/ 1 Aust. Cultivars
Start Date: 27/11/1997
End Date: 27/04/1998
SimulationName
Cultivar
Lawes1997LateCvBuster
Buster
Lawes1997LateCvM351
M35-1
Lawes1997LateCvCSH13R
CSH13R
Experiment: LE19
Description: LE19 - Genotypic variation in Radiation Use Efficiency
Start Date: 9/11/1998
End Date: 7/03/1999
SimulationName
N Rates
Cultivar
Lawes1998FertOffBuster
0 kg/ha
Buster
Lawes1998FertLowBuster
120 kg/ha
Buster
Lawes1998FertMedBuster
240 kg/ha
Buster
Lawes1998FertHighBuster
360 kg/ha
Buster
Lawes1998FertOffCSH13R
0 kg/ha
CSH13R
Lawes1998FertLowCSH13R
120 kg/ha
CSH13R
Lawes1998FertMedCSH13R
240 kg/ha
CSH13R
Lawes1998FertHighCSH13R
360 kg/ha
CSH13R
Dynamic Tillering is still under development.
Experiment: LE20
Description: LE20 - Density
Start Date: 9/11/1998
End Date: 7/03/1999
SimulationName
Density
Density_1
2 plants/m2
Density_2
4 plants/m2
Density_2
8 plants/m2
Density_3
16 plants/m2
Experiment: Gatton_RUE
Description: N Rates x Genotype LE21
Start Date: 22/11/1999
End Date: 3/04/2000
SimulationName
Cultivar
N Rates
Lawes1999FertLowCvCSH13R
CSH13R
0kg/ha
Lawes1999FertLowCvA35xQL36
A35xQL36
0kg/ha
Lawes1999FertLowCvQL39xQL36
QL39xQL36
0kg/ha
Lawes1999FertMedCvCSH13R
CSH13R
45kg/ha
Lawes1999FertMedCvA35xQL36
A35xQL36
45kg/ha
Lawes1999FertMedCvQL39xQL36
QL39xQL36
45kg/ha
Lawes1999FertHighCvCSH13R
CSH13R
360kg/ha
Lawes1999FertHighCvA35xQL36
A35xQL36
360kg/ha
Lawes1999FertHighCvQL39xQL36
QL39xQL36
360kg/ha
3 Interface
3.1 Sorghum
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
Alam, Mohammad Mobashwer, Hammer, Graeme L., Oosterom, Erik J., Cruickshank, Alan W., Hunt, Colleen H., Jordan, David R., 2014. A physiological framework to explain genetic and environmental regulation of tillering in sorghum. New Phytologist 203 (1), 155-167.
Anten, N. P. R., Schieving, F., Werger, M. J. A., 1995. Patterns of light and nitrogen distribution in relation to whole canopy carbon gain in C3 and C4 mono- and dicotyledonous species. Oecologia 101 (4), 504-513.
Birch, C.J., Carberry, P.S., Muchow, R.C., McCown, R.L., Hargreaves, J.N.G., 1990. Development and evaluation of a sorghum model based on CERES-Maize in a semi-arid tropical environment. Field Crops Research 24 (1-2), 87-104.
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Carberry, P.S., Muchow, R.C., Hammer, G.L., 1993. Modelling genotypic and environmental control of leaf area dynamics in grain sorghum. II. Individual leaf level. Field Crops Research 33 (3), 311-328.
Chapman, S. C., Cooper, M., Hammer, G. L., Butler, D. G., 2000. Genotype by environment interactions affecting grain sorghum. II. Frequencies of different seasonal patterns of drought stress are related to location effects on hybrid yields. Australian Journal of Agricultural Research 51 (2), 209.
Chapman, S. C., Hammer, G. L., Butler, D. G., Cooper, M., 2000. Genotype by environment interactions affecting grain sorghum. III. Temporal sequences and spatial patterns in the target population of environments. Australian Journal of Agricultural Research 51 (2), 223-233.
Chapman, S. C., Hammer, G. L., Meinke, H., 1993. A Sunflower simulation model.1. Model development. Agronomy Journal 85 (3), 725-735.
Chapman, S., Cooper, M., Podlich, D.W., Hammer, G., 2003. Evaluating plant breeding strategies by simulating gene action and dryland environment effects.. Agron. J. 95, 99-113.
Charles-Edwards, D.A., 1982. Physiological Determinants of Crop Growth. XF2006232780.
Cooper, Mark, Messina, Carlos D., Podlich, Dean, Totir, L. Radu, Baumgarten, Andrew, Hausmann, Neil J., Wright, Deanne, Graham, Geoffrey, 2014. Predicting the future of plant breeding: complementing empirical evaluation with genetic prediction. Crop and Pasture Science 65 (4), 311.
Craufurd, P. Q., Flower, D. J., Peacock, J. M., 1993. Effect of Heat and Drought Stress on Sorghum (\textitSorghum Bicolor). I. Panicle Development and Leaf Appearance. Experimental Agriculture 29 (1), 61-76.
GRINDLAY, D. J. C., 1997. REVIEW Towards an explanation of crop nitrogen demand. The Journal of Agricultural Science 128 (4), 377-396.
Hammer, G. L., Chapman, S., van Oosterom, E., Podlich, D. W., 2005. Trait physiology and crop modelling as a framework to link phenotypic complexity to underlying genetic systems. Australian Journal of Agricultural Research 56 (9), 947-960.
Hammer, G. L., Muchow, R. C., 1994. Assessing climatic risk to sorghum production in water-limited subtropical environments I. Development and testing of a simulation model. Field Crops Research 36 (3), 221-234.
Hammer, G. L., Vanderlip, R. L., Gibson, G., Wade, L. J., Henzell, R. G., Younger, D. R., Warren, J., Dale, A. B., 1989. Genotype-by-Environment Interaction in Grain Sorghum. II. Effects of Temperature and Photoperiod on Ontogeny. Crop Science 29 (2), 376.
Hammer, G.L., Carberry, P.S., Muchow, R.C., 1993. Modelling genotypic and environmental control of leaf area dynamics in grain sorghum. I. Whole plant level. Field Crops Research 33 (3), 293-310.
Hammer, Graeme L., Dong, Zhanshan, McLean, Greg, Doherty, Al, Messina, Carlos, Schussler, Jeff, Zinselmeier, Chris, Paszkiewicz, Steve, Cooper, Mark, 2009. Can Changes in Canopy and/or Root System Architecture Explain Historical Maize Yield Trends in the U.S. Corn Belt?. Crop Sci 49 (1), 299-312.
Hammer, Graeme L., Farquhar, Graham D., Broad, Ian J., 1997. On the extent of genetic variation for transpiration efficiency in sorghum. Australian Journal of Agricultural Research 48 (5), 649.
Hammer, Graeme L., McLean, Greg, Chapman, Scott, Zheng, Bangyou, Doherty, Al, Harrison, Matthew T., van Oosterom, Erik, Jordan, David, 2014. Crop design for specific adaptation in variable dryland production environments. Crop and Pasture Science 65 (7), 614.
Hammer, Graeme L., van Oosterom, Erik, McLean, Greg, Chapman, Scott C., Broad, Ian, Harland, Peter, Muchow, Russell C., 2010. Adapting APSIM to model the physiology and genetics of complex adaptive traits in field crops. Journal of Experimental Botany 61 (8), 2185-2202.
Hammer, Graeme, Cooper, Mark, Tardieu, Francois, Welch, Stephen, Walsh, Bruce, van Eeuwijk, Fred, Chapman, Scott, Podlich, Dean, 2006. Models for navigating biological complexity in breeding improved crop plants. Trends in Plant Science 11 (12), 587-593.
Hammer, Graeme, Erik J. van Oosterom, Scott C. Chapman, Greg McLean, 2001. The economic theory of water and nitrogen dynamics and management in field crops..
Hammer, Graeme, McLean, Greg, Doherty, Al, van Oosterom, Erik, Chapman, Scott, 2019. Agronomy Monographs. Agronomy Monographs, 215-239.
Hammer, Graeme, Messina, Charlie, van Oosterom, Erik, Chapman, Scott, Singh, Vijaya, Borrell, Andrew, Jordan, David, Cooper, Mark, 2016. Crop Systems Biology. Crop Systems Biology, 147-162.
Hammer, Graeme, Messina, Charlie, Wu, Alex, Cooper, Mark, 2019. Biological reality and parsimony in crop models--why we need both in crop improvement!. in silico Plants 1 (1).
Hammer, Graeme. L., McLean, Greg, Oosterom, Erik, Chapman, Scott, Zheng, Bangyou, Wu, Alex, Doherty, Alastair, Jordan, David, 2020. Designing crops for adaptation to the drought and hightemperature risks anticipated in future climates. Crop Science 60 (2), 605-621.
Heiniger, Ronnie W., Vanderlip, Richard L., Welch, Stephen M., Muchow, Russell C., 1997. Developing Guidelines for Replanting Grain Sorghum: II. Improved Methods of Simulating Caryopsis Weight and Tiller Number. Agronomy Journal 89 (1), 84-92.
Kemanian, Armen R., St\ ockle, Claudio O., Huggins, David R., 2005. Transpiration-use efficiency of barley. Agricultural and Forest Meteorology 130 (1-2), 1-11.
Kim, Hae Koo, van Oosterom, Erik, Dingkuhn, Michael, Luquet, Delphine, Hammer, Graeme, 2010. Regulation of tillering in sorghum: environmental effects. Annals of Botany 106 (1), 57-67.
LAFARGE, T. A., HAMMER, G. L., 2002. Tillering in Grain Sorghum over a Wide Range of Population Densities: Modelling Dynamics of Tiller Fertility. Annals of Botany 90 (1), 99-110.
Martre, Pierre, Jamieson, Peter D., Semenov, Mikhail A., Zyskowski, Robert F., Porter, John R., Triboi, Eug` ene, 2006. Modelling protein content and composition in relation to crop nitrogen dynamics for wheat. European Journal of Agronomy 25 (2), 138-154.
Meinke, Holger, Hochman, Zvi, 2000. Applications of Seasonal Climate Forecasting in Agricultural and Natural Ecosystems. Applications of Seasonal Climate Forecasting in Agricultural and Natural Ecosystems, 149-165.
Messina, C.D., Technow, F., Tang, T., Totir, R., Gho, C., Cooper, M., 2018. Leveraging biological insight and environmental variation to improve phenotypic prediction: Integrating crop growth models (CGM) with whole genome prediction (WGP). European Journal of Agronomy 100, 151-162.
Monteith, J. L., Greenwood, D. J., 1986. How Do Crops Manipulate Water Supply and Demand? [and Discussion]. Philosophical Transactions of the Royal Society of London A: Mathematical, Physical and Engineering Sciences 316 (1537), 245-259.
Monteith, J. L., Moss, C. J., 1977. Climate and the Efficiency of Crop Production in Britain [and Discussion]. Philosophical Transactions of the Royal Society of London. Series B, Biological Sciences 281 (980), 277-294.
Muchow, R. C., Sinclair, T. R., 1994. Nitrogen Response of Leaf Photosynthesis and Canopy Radiation Use Efficiency in FieldGrown Maize and Sorghum. Crop Science 34 (3), 721-727.
Muchow, R.C., Carberry, P.S., 1990. Phenology and leaf-area development in a tropical grain sorghum. Field Crops Research 23 (3-4), 221-237.
Nelson, R. A., Holzworth, D. P., Hammer, G. L., Hayman, P. T., 2002. Infusing the use of seasonal climate forecasting into crop management practice in North East Australia using discussion support software. Agricultural Systems 74 (3), 393-414.
Passioura, J.B., 1983. Roots and drought resistance. Agricultural Water Management 7 (1-3), 265-280.
Probert, M. E., Dimes, J. P., Keating, B. A., Dalal, R. C., Strong, W. M., 1998. APSIM's Water and Nitrogen Modules and Simulation of the Dynamics of Water and Nitrogen in Fallow Systems. Agricultural Systems 56 (1), 1-28.
Ravi Kumar, S., Hammer, Graeme L., Broad, Ian, Harland, Peter, McLean, Greg, 2009. Modelling environmental effects on phenology and canopy development of diverse sorghum genotypes. Field Crops Research 111 (1-2), 157-165.
Robertson, M.J., Fukai, S., Ludlow, M.M., Hammer, G.L., 1993. Water extraction by grain sorghum in a sub-humid environment. I. Analysis of the water extraction pattern. Field Crops Research 33 (1-2), 81-97.
Sinclair, T. R., 1986. Water and nitrogen limitations in soybean grain production. I. Model development.. Field Crops Research 15, 125-141.
Sinclair, T. R., Horie, T., 1989. Leaf Nitrogen, Photosynthesis, and Crop Radiation Use Efficiency: A Review. Crop Science 29 (1), cropsci1989.0011183X002900010023x.
Sinclair, T.R., Amir, J., 1992. A model to assess nitrogen limitations on the growth and yield of spring wheat. Field Crops Research 30 (1-2), 63-78.
Sinclair, Thomas R., Hammer, Graeme L., van Oosterom, Erik J., 2005. Potential yield and water-use efficiency benefits in sorghum from limited maximum transpiration rate. Functional Plant Biology 32 (10), 945.
Sinclair, Thomas R., Muchow, Russell C., 1999. Advances in Agronomy. Advances in Agronomy, 215-265.
Tanner, C. B., Sinclair, T. R., 1983. Limitations to Efficient Water Use in Crop Production. Limitations to Efficient Water Use in Crop Production, 1-27.
Van Keulen, H., Seligman, N. G., 1987. Simulation of water use, nitrogen nutrition and growth of a spring wheat crop. Simulation of water use, nitrogen nutrition and growth of a spring wheat crop..
van Oosterom, E.J., Borrell, A.K., Chapman, S.C., Broad, I.J., Hammer, G.L., 2010. Functional dynamics of the nitrogen balance of sorghum: I. N demand of vegetative plant parts. Field Crops Research 115 (1), 19-28.
van Oosterom, E.J., Carberry, P.S., Hargreaves, J.N.G., O’Leary, G.J., 2001. Simulating growth, development, and yield of tillering pearl millet: II. Simulation of canopy development. Field Crops Research 72 (1), 67 - 91.
Whish, J., Butler, G., Castor, M., Cawthray, S., Broad, I., Carberry, P., Hammer, G., McLean, G., Routley, R., Yeates, S., 2005. Modelling the effects of row configuration on sorghum yield reliability in north-eastern Australia. Australian Journal of Agricultural Research 56 (1), 11-23.
Wu, Alex, Hammer, Graeme L., Doherty, Al, von Caemmerer, Susanne, Farquhar, Graham D., 2019. Quantifying impacts of enhancing photosynthesis on crop yield. Nature Plants 5 (4), 380-388.
