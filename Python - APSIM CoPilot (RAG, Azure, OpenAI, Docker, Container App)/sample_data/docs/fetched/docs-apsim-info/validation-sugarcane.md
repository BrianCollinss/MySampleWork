# Sugarcane documentation

Source URL: https://docs.apsim.info/validation/Sugarcane
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:53:24.816900+00:00

1 The APSIM Sugarcane Model
Datasets
Simulations numbered as per
Table 2. "Datasets used for model development" (Page 260)
in:
Modelling sugarcane production systems I. Development and performance of the sugarcane module.
Field Crops Research, 61: 253-271.  (1999).
Keating, B.A., Robertson, M.J., Muchow, R.C. and Huth, N.I.
nb.
Datasets 4 and 17 were not included in simulations as they were not in original files
Dataset 18 was not included as irrigation records were missing.
nb.
I had to increase the harvest dates in every simulation by one day.
This is because the model does management at the start of everyday (ie. the crop is harvested at the start of every day)
But the "daily" reporting is the very last thing that happens each day.
Therefore if we set our harvest date to be the same as that in the observed data, then our predicted data at harvest will all be zeroed (because after the model harvests it zeros the values).
This is not a problem when we only report at "harvesting" instead of "daily" because the reporting occurs before the values are zeroed.
However we have to report daily because we have observation data during the crop growth not just at harvesting.
The APSIM Sugarcane Model
Sugarcane model is ported from APSIM 7.10 and does not have a PMF structure.
Default values for cultivars are based of Q117 and Q117_ratoon.
Model Components Overview
Crop dry weight accumulation is driven by the conversion of intercepted radiation to biomass, via a radiation use efficiency (RUE).
RUE is reduced whenever extremes of temperature, soil water shortage or excess, or plant nitrogen deficit limit photosynthesis.
The crop leaf canopy, which intercepts radiation, expands its area as a function of temperature, and can also be limited by extremes of temperature, soil water shortage or excess, or plant nitrogen deficit.
Biomass is partitioned among the various plant components (leaf, cabbage, structural stem, roots and sucrose) as determined by crop phenological stage.
Nitrogen uptake is simulated, as is the return of carbon and nitrogen to the soil in trash and roots.
In many sugarcane production systems, commercial yield is measured as the fresh weight of sugarcane stems and their sucrose concentration. Hence, the water content in addition to the dry weight of the stem is simulated.
Since sugarcane is grown both as a plant and ratoon crop, the model also needs to be able to simulate differences between crop classes based on any known physiological differences between these classes.
Crop growth in the absence of nitrogen or water limitation
Thermal time
Thermal time is used in the model to drive phenological development and canopy expansion.
In APSIM Sugarcane, thermal time is calculated using a base temperature of 9 oC, optimum temperature of 32 oC, and maximum temperature of 45 oC.
The optimum and maximum temperatures were taken from those used for maize [jones_ceres maize:_1986].
Base temperatures for sugarcane have been variously reported between 8 oC and 15 oC [inman bamber_temperature_1994]
Robertson et al., 1998
.
The base of 9 oC used in APSIM sugarcane was chosen to be consistent with those studies which sampled the greatest temperature range, namely [inman bamber_temperature_1994]
Robertson et al., 1998
who identified base temperatures of 10 oC and 8 oC respectively.
For thermal time calculations in the model, temperature is estimated every three hours from a sine function fitted to daily maximum and minimum temperatures, using the method described by [jones_ceres maize:_1986].
Phenology
The sugar model uses six different stages to define crop growth and status.
|Stage     |Description
|          |:
|sowing    |From sowing to sprouting
|sprouting |From sprouting to emergence
|emergence |From emergence to the beginning of cane growth
|begin_cane|From the beginning of cane growth to flowering
|flowering |From flowering to the end of the crop
|end_crop  |Crop is not currently in the simulated system.
Sprouting occurs after a lag period, set to 350 oCdays for plant crops and 100 oCdays for ratoon crops.
Provided the soil water content of the layer is adequate, shoots will elongate towards the soil surface at a rate of 0.8 mm per oCday.
The thermal duration between emergence and beginning of stalk growth is a genotype coefficient in the range 1200 to 1800 oCdays.
Although, sugarcane does produce flowers, the number of stalks producing flowers in a field is highly variable, and its physiological basis is not fully understood.
While the model structure has been developed to include flowering as a phenological stage, it has been deactivated until a better physiological basis for prediction is available.
Canopy expansion
The experimental basis for the canopy expansion model is described by
Robertson et al., 2016
.
Briefly, green leaf area index is the product of
green leaf area per stalk
and the
number of stalks per unit ground area
.
Green leaf area per stalk
is simulated by summing the fully expanded area of successive leaves that appear on each stalk, and adding a correction factor for the area of expanding leaves (set to 1.6 leaves per stalk).
Profiles of leaf area per leaf are input as genotype coefficients.
Robertson et al., 2016
found leaf appearance rates declined as a continuous function of cumulative thermal time, so that at emergence leaves took 80 oCd to appear while leaf 40 required 150 oCd.
These responses are reproduced in the model (via a series of linear interpolations) in both plant and ratoon crops.
Stalk number
rises rapidly to a peak during the first 1400 oCdays from emergence, thereafter declining to reach a stable stalk number (e.g. [inman bamber_temperature_1994]).
Ratoon crops commonly reach an earlier peak stalk number than plant crops, with consequently faster early canopy expansion in ratoons
Robertson et al., 1996
.
In the model, the complexity of simulating the dynamics of tillering in order to predict LAI during early growth is avoided.
Instead, the crop is conceived to have a notional constant stalk number throughout growth, usually set at 10 stalks m 2 , although this value can be varied as an input.
The additional leaf area associated with tillers that appear and subsequently die, is captured via a calibrated tillering factor, that effectively increases the area of the leaves that are produced over the early tillering period.
The known faster early expansion of LAI in ratoon crops is simulated via two effects.
Firstly, the lag time for regrowth of shoots after harvest is shorter in a ratoon crop than is the equivalent thermal time for a plant crop to initiate stalk elongation.
Secondly, tillering is recognised in the model coefficients as making a larger contribution to leaf area development in a ratoon crop than a plant crop.
The daily rate of senescence of green leaf area is calculated as the maximum of four rates determined by the factors of ageing, light competition, water stress and frost.
In the model, ageing causes senescence by not allowing at any time more than 13 fully expanded green leaves per stalk.
Light competition is simulated to induce senescence once fractional radiation interception reaches 0.85.
Water stress induces senescence once the soil water deficit factor for photosynthesis declines below 1.0.
Frosting removes 10% of the LAI per day if the minimum temperature reaches 0 oC, and 100% if it reaches  5 oC.
Root growth and development
Root biomass is produced independently from the shoot, so that a proportion of daily above ground biomass production is added to the root system.
The proportion decreases from a maximum of 0.30 at emergence and asymptotes to 0.20 at flowering.
Root biomass is converted to root length via a specific root length of 18000 mm g 1 .
The depth of the root front in plant crops increases by 1.5 cm day 1 [_glover_proceedings_nodate] from emergence, with the maximum depth of rooting set by the user.
At harvest, 17% of roots in all the occupied soil layer die [ball coelho_root_1992].
Biomass accumulation and partitioning
The sugar model partitions dry matter to
five different plant pools
. These are as follows:
|Plant Part |Description
|           |:
|Root       |Below ground biomass
|Leaf       |Leaf
|Sstem      |Structural component of millable stalk
|Cabbage    |Leaf sheath and tip of growing stalks etc
|Sucrose    |Sucrose content of millable stalk
In addition to the five live biomass pools outlined above, senescent leaf and cabbage is maintained as trash on the plant or progressively detached to become residues on the soil surface.
In APSIM, the RESIDUE module
Probert et al., 1996
takes on the role of decomposition of crop residues.
LAI is used in the model to intercept incident solar radiation following Beer's Law, using a radiation extinction coefficient of 0.38, determined by
Muchow et al., 1994
Robertson et al., 1996
.
Intercepted radiation is used to produce daily biomass production using a radiation use efficiency (RUE) of 1.80 g MJ 1 for plant crops and 1.65 g MJ 1 for ratoon crops.
The values of RUE used in the model are those adjusted upwards from field measured values
Muchow et al., 1994
Robertson et al., 1996
due to the underestimate of biomass production caused by incomplete recovery of senesced leaf material
Robertson et al., 1996
.
In the model, RUE is reduced if the mean daily temperature falls below 15 oC or exceeds 35 oC, and becomes zero if the mean temperature reaches 5 or 50 oC, respectively.
These effects are similar to those used in other models of C4 crop species
Hammer et al., 1994
.
Four above ground biomass pools are modelled:
leaf
,
cabbage
,
structural stem
,
stem sucrose
,
(and an additional pool for
roots
that is simulated separately from above ground production).
Between emergence and the beginning of stalk growth, above ground biomass is partitioned between leaf and cabbage in the ratio 1.7:1
Robertson et al., 1996
.
After the beginning of stem growth 0.7 of above ground biomass is partitioned to the stem robertson_growth_1996, with the remainder partitioned between leaf and cabbage in the ratio 1.7:1.
After a minimum amount of stem biomass has accumulated, the daily biomass partitioned to stem is divided between structural and sucrose pools, following the framework developed by
Muchow et al., 1996
and
Robertson et al., 1996
. Thereafter, the stem biomass is equal to the sum of structural and sucrose pools.
If biomass partitioned to leaf is insufficient for growth of the leaf area, as determined by a maximum specific leaf area, then daily leaf area expansion is reduced.
If biomass partitioned to leaf is in excess of that required to grow the leaf area on that day, then specific leaf area is permitted to decrease to a lower limit, beyond which the “excess” biomass is partitioned to sucrose and structural stem.
A stalk growth stress factor is calculated as the most limiting of the water, nitrogen and temperature limitations on photosynthesis.
This stress factor influences both the onset and rate of assimilate partitioning to sucrose at the expense of structural stem.
Stem water content
A stem water pool is simulated for the purposes of calculating cane fresh weight and CCS%.
For every gram of structural stem grown, a weight of water is considered to have been accumulated by the cane stems.
This relationship varies with thermal time, ranging from 9 g g 1 initially, to 5 g g 1 late in the crop life cycle.
The former represents the water content of young stem (eg. cabbage) while the latter represents a combination of young stem growth and thickening of older stem.
Sucrose deposition in the stem removes water content at the rate of 1 g water g 1 sucrose.
Varietal effects
Currently varieties differ in only two respects in the model.
Firstly, Inman Bamber (1991) found that varieties in South Africa differed in the fully expanded area of individual leaves.
The distributions for NCo376 and N14 were taken from Inman Bamber and Thompson (1989), while that for Q117 and Q96 was those assigned values that gave best fit to the time course of LAI during the model calibration stage.
Secondly,
Robertson et al., 1996
found that varieties from South Africa and Australia differed in terms of partitioning of biomass to sucrose in the stem.
There is scope for incorporating other varietal differences as new knowledge becomes available.
Water deficit limitation
Soil water infiltration and redistribution, evaporation and drainage is simulated by other modules in the APSIM framework
Probert et al., 1996
and (Verburg et al, 1997).
Water stress in the model reduces the rate of leaf area expansion and radiation use efficiency, via two soil water deficit factors, which vary from zero to 1.0, following the concepts embodied in the CERES models (Ritchie, 1986).
Soil water deficit factor 1 (SWDEF1), which is less sensitive to soil drying, reduces the radiation use efficiency (i.e. net photosynthesis) and hence transpiration, below its maximum.
Soil water deficit factor 2 (SWDEF2), which is more sensitive to soil drying, reduces the rate of processes governed primarily by cell expansion, i.e. daily leaf expansion rate.
SWDEF1 and 2 are calculated as a function of the ratio of (potential soil water supply from the root system) and the (transpiration demand).
Following [sinclair_water_1986 and Monteith (1986), transpiration demand is modelled as a function of the (current day's crop growth rate), divided by the transpiration use efficiency.
When soil water supply exceeds transpiration demand, assimilate fixation is a function of radiation interception and radiation use efficiency.
When soil water supply is less than transpiration demand, assimilate fixation is a function of water supply and transpiration efficiency and the vapour pressure deficit (VPD).
Transpiration use efficiency has not been directly measured for sugarcane, but calibration of the current model on datasets exhibiting water deficits (Robertson et al, unpubl. data) resulted in the use of a transpiration use efficiency of 8 g kg 1 at a VPD of 1 kPa.
This efficiency declines linearly as a function of VPD (Tanner and Sinclair, 1983).
This compares with reported values of 9 g kg 1 kPa 1 for other C 4 species (Tanner and Sinclair 1983), a value that has been used in the models of sorghum (Hammer and Muchow, 1994) and maize
Carberry et al., 1991
.
Potential soil water uptake is calculated using the approach first advocated by Monteith (1986) and subsequently tested for sunflower
Meinke et al., 1993
and grain sorghum (Robertson et al., 1994).
It is the sum of root water uptake from each profile layer occupied by roots.
The potential rate of extraction in a layer is calculated using a rate constant, which defines the fraction of available water able to be extracted per day.
The actual rate of water extraction is the lesser of the potential extraction rate and the transpiration demand.
If the computed potential extraction rate from the profile exceeds demand, then the extracted water is removed from the occupied layers in proportion to the values of potential root water uptake in each layer.
If the computed potential extraction from the profile is less than the demand then SWDEF2 declines in proportion, and the actual root water uptake from a layer is equal to the computed potential uptake.
In addition to the effects on canopy expansion and biomass accumulation, water stress influence biomass partitioning in the stem in two ways.
Firstly, the minimum amount of stem biomass required to initiate sucrose accumulation declines with accumulated stress.
Secondly, the daily dry weight increment between structural stem and sucrose shifts in favour of sucrose as water deficits develop.
Water excess limitation
The proportion of the root system exposed to saturated or near saturated soil water conditions is calculated and used to calculate a water logging stress factor.
This factor reduces photosynthetic activity via an effect on RUE.
Nitrogen limitation
N supply from the soil is simulated in other modules in the APSIM framework
Probert et al., 1996
.
Crop nitrogen demand is simulated using an approach similar to that used in the CERES models
Godwin et al., 1985
.
Crop N demand is calculated as the product of maximum tissue N concentration and the increment in tissue weight.
Separate N pools are described for green leaf, cabbage, millable stalk and dead leaf. The sucrose pool is assumed to have no nitrogen associated with it.
Only the leaf N concentrations influence crop growth processes. Growth is unaffected until leaf N concentrations fall below a critical concentration.
Sugarcane has been shown to exhibit luxury N uptake
Muchow et al., 1994
(Catchpoole and Keating 1995) and the difference between the maximum and critical N concentrations is intended to simulate this phenomenon.
Nitrogen stress is proportional to the extent to which leaf N falls between the critical and the minimum N concentration.
Senescing leaves (and the associated leaf sheaths contained in the cabbage pool) are assumed to die at their minimum N concentrations and the balance of the N in these tissues is retranslocated to the green leaf and cabbage pools.
Maximum, critical and minimum N concentrations are all functions of thermal time, and were chosen on the basis of the findings of Catchpoole and Keating (1995) and
Muchow et al., 1994
and subsequently refined during the model calibration.
Critical green leaf concentrations used in the model differ between photosynthetic, leaf expansion and stem growth processes.
For photosynthesis they begin at 1.2% N at emergence or ratooning and asymptote towards 0.5%N at flowering.
For leaf area expansion they are 1.3 and 0.5% N
and stem growth, 1.5 and 0.5%N.
N uptake cannot exceed N demand by the crop and is simulated to take place by mass flow in the water that is used for transpiration.
Should mass flow not meet crop demand and nitrate be available in soil layers, the approach of
Van Keulen et al., 1987
is used to simulate the uptake of nitrate over and above that which can be accounted for by mass flow.
While van Keulen and Seligman (1987) referred to this approach as “diffusion”, the routine more realistically serves as a surrogate for a number of sources of uncertainty in nitrate uptake.
Nitrogen stress also influences biomass partitioning in the stem, in a similar fashion to that described above for water stress.
Other features of the sugar module
APSIM Sugarcane includes a number of features relevant to sugarcane production systems.
Either plant or ratoon crops can be simulated at the outset or a plant crop will regenerate as a ratoon crop if a crop cycle is being simulated.
Production systems of plant   multiple ratoon   fallow can be simulated or alternatively other APSIM crop or pasture modules can be included in rotation with sugarcane.
Trash can be burnt or retained at harvest time.
Insect or other biological or mechanical damage to the canopy can be simulated via “graze” actions.
Many sugarcane crops are “hilled up” early in canopy development, an operation that involves the movement of soil from the interrow to the crop row.
This operation facilitates irrigation operations and improves the crop's ability to stand upright.
APSIM Sugarcane responds to a management event of hilling up by removal of lower leaf area and stem from the biomass pools.
Lodging is a widespread phenomenon in high yielding sugarcane crops.
The APSIM MANAGER [mccown_apsim:_1996] can initiate a lodging event in response to any aspect of the system state (eg crop size, time of year and weather).
APSIM SUGARCANE responds to lodging via four effects:
A low rate of stalk death which has been widely observed in heavily lodged crops (Muchow et al., 1995; Robertson et al., 1996)
Singh et al., 2002
;
A reduction in radiation use efficiency (Singh et al., 1999)
Singh et al., 2002
A reduction in the proportion of daily biomass that is partitioned as sucrose
Singh et al., 2002
; and
A reduction in the maximum number of green leaves, to capture the reported reduction in leaf appearance rate and increase in leaf senescence
Singh et al., 2002
###Parameterisation
(Structure of the xml in the .apsimx file)
There are
4
separate categories of variables in the Sugarcane modules xml.
They are listed below with some examples of the type of parameters included in each.
Constants
Upper and lower bounds for met and soil variables
Plant_crop
Growth and partitioning parameters
Water Use Parameters and Water and temperature Stress Factors
Frosting Factors
Nitrogen Contents and Nitrogen Stress Factors
Ratoon_crop
Same as Plant crop section but there is the ability to change the parameters between plant and ratoon crops.
Cultivar (Plant Crop and Ratoon Crop)
Plant Crop Cultivar
Leaf Development Parameters
Phenology
Sucrose and Cane Stalk  (Partitioning Parameters)
Ratoon Crop Cultivar
Same as for the Plant Crop Cultivar
By creating a completely new cultivar with the same name as the plant crop cultivar but appending "_ratoon" to the end of the name,
the Sugarcane module will then automatically use this ratoon crop cultivar rather then the plant crop cultivar
when the crop changes from a Plant Crop to a Ratoon Crop.
Sugar Module Outputs
|Variable Name  | Units      | Description
|               |:           |:
|Stage_name     |            | Name of the current crop growth stage
|Stage          |            | Current growth stage number
|Crop_status    |            | Status of the current crop (alive,dead,out)
|ratoon_no      |            | Ratoon number (0 for plant crop, 1 for 1st ratoon, 2 for 2nd ratoon, …etc)
|das            | Days       | Days after sowing (ie. crop duration)
|Ep             | mm         | Crop evapotranspiration (extraction) for each soil layer
|cep            | mm         | Cumulative plant evapotranspiration
|rlv            | mm/mm
3    | root length per volume of soil in each soil layer
|esw            | mm         | Extractable Soil water in each soil layer
|root_depth     | mm         | Root depth
|sw_demand      | mm         | Daily demand for soil water
|biomass        | g/m
2      | Total crop above ground biomass (Green + Trash)
|green_biomass  | g/m
2      | Total green crop above ground biomass
|biomass_n      | g/m
2      | Total Nitrogen in above ground biomass (Green + Trash)
|green_biomass_n| g/m
2      | Amount of Nitrogen in green above ground biomass
|dlt_dm         | g/m
2      | Daily increase in plant dry matter (photosynthesis)
|dm_senesced    | g/m
2      | Senesced dry matter in each plant pool
|n_senesced     | g/m
2      | Amount of Nitrogen in senesced material for each plant pool
|Canefw         | t/ha       | Fresh Cane weight
|ccs            | %          | Commercial Cane Sugar
|Cane_wt        | g/m
2      | Weight of cane dry matter
|leaf_wt        | g/m
2      | Weight of plant green leaf
|root_wt        | g/m
2      | Weight of plant roots
|sstem_wt       | g/m
2      | Weight of plant structural stem
|sucrose_wt     | g/m
2      | Weight of plant sucrose
|cabbage_wt     | g/m
2      | Weight of plant cabbage
|n_conc_cane    | g/g        | Nitrogen concentration in cane
|n_conc_leaf    | g/g        | Nitrogen concentration in green leaf
|n_conc_cabbage | g/g        | Nitrogen concentration in green cabbage
|n_demand       | g/m
2      | Daily demand for Nitrogen
|cover_green    | 0 1        | Fractional cover by green plant material
|cover_tot      | 0 1        | Fractional cover by total plant material (Green + Trash)
|lai            | mm
2/mm
2  | Leaf area index of green leaves
|tlai           | mm
2/mm
2  | Total plant leaf area index (green + senesced)
|slai           | mm
2/mm
2  | Senesced leaf area index
|n_leaf_crit    | g/m
2      | Critical Nitrogen level for the current crop
|n_leaf_min     | g/m^2      | Minimum Nitrogen level for the current crop
|nfact_photo    | 0 1        | Nitrogen stress factor for photosynthesis
|nfact_expan    | 0 1        | Nitrogen stress factor for cell expansion
|swdef_photo    | 0 1        | Soil water stress factor for photosynthesis
|swdef_expan    | 0 1        | Soil water stress factor for cell expansion
|swdef_phen     | 0 1        | Soil water stress factor for phenology
The model is constructed from the following list of software components. Details of the implementation and model parameterisation are provided in the following sections.
1.1 Plant Model Components
Component Name
Component Type
1.2 Cultivars
Cultivar Name
Alternative Name(s)
q117
q117
q117_ratoon
q117_ratoon
q117_fum
q117_fum
q117_fum_ratoon
q117_fum_ratoon
q96
q96
q96_ratoon
q96_ratoon
q96_fum
q96_fum
q96_fum_ratoon
q96_fum_ratoon
q138
q138
q138_ratoon
q138_ratoon
q138_fum
q138_fum
q138_fum_ratoon
q138_fum_ratoon
ts65-28
ts65-28
ts65-28_ratoon
ts65-28_ratoon
ts65-28_fum
ts65-28_fum
ts65-28_fum_ratoon
ts65-28_fum_ratoon
h73
h73
h73_ratoon
h73_ratoon
q141
q141
q141_ratoon
q141_ratoon
nco376
nco376
nco376_ratoon
nco376_ratoon
n12
n12
n12_ratoon
n12_ratoon
n14
n14
n14_ratoon
n14_ratoon
CP51
CP51
CP51_ratoon
CP51_ratoon
R570
R570
R570_ratoon
R570_ratoon
M1356
M1356
M1356_ratoon
M1356_ratoon
M55560
M55560
M55560_ratoon
M55560_ratoon
q124
q124
q124_ratoon
q124_ratoon
2 Interface
2.1 Sugarcane
Parameters (Inputs)
Name
Description
Units
Type
Value
LAI
double
0
WaterDemand
mm
double
0
crop_type
()
String
Sugarcane
tt_emerg_to_begcane_ub
double
2000
tt_begcane_to_flowering_ub
double
10000
tt_flowering_to_crop_end_ub
oC
double
5000
n_uptake_option
int32
1
NO3_diffn_const
days
double
2
n_supply_preference
String
active
kno3
/day
double
0.05
no3ppm_min
oC
double
NaN
knh4
/day
double
0.05
nh4ppm_min
oC
double
NaN
total_n_uptake_max
g/m2
double
0.6
ll_ub
oC
double
1000
kl_ub
oC
double
1
minsw
double
1E-05
latitude_ub
oL
double
90
latitude_lb
oL
double
-90
maxt_ub
oC
double
55
mint_ub
oC
double
40
mint_lb
oC
double
-10
radn_ub
MJ/m^2
double
50
radn_lb
MJ/m^2
double
1
dlayer_ub
mm
double
1000
dlayer_lb
mm
double
0
dul_dep_ub
mm
double
1000
dul_dep_lb
mm
double
0
sw_dep_ub
mm
double
1000
sw_dep_lb
mm
double
0
NO3_ub
kg/ha
double
10000
NO3_lb
kg/ha
double
0
NO3_min_ub
kg/ha
double
10000
NO3_min_lb
kg/ha
double
0
NH4_ub
kg/ha
double
10000
NH4_lb
kg/ha
double
0
NH4_min_ub
kg/ha
double
10000
NH4_min_lb
kg/ha
double
0
plant
SugarcaneSpeciesConstants
SugarcaneSpeciesConstants
ratoon
SugarcaneSpeciesConstants
SugarcaneSpeciesConstants
eo_crop_factor
double
100
uptake_source
String
calc
rlv_init
double
System.Double[]
ShowInDocs
Whether this folder should show up in documentation or not.
boolean
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Command
String
Properties (Outputs)
Name
Description
Units
Type
Settable?
Structure
IStructure
True
CanopyType
String
False
Albedo
double
False
Gsmax
double
False
R50
double
False
LAITotal
double
False
CoverGreen
double
False
CoverTotal
double
False
Height
double
False
Depth
double
False
Width
double
False
FRGR
double
False
PotentialEP
double
True
LightProfile
CanopyEnergyBalanceInterceptio...
True
PlantType
String
False
IsC4
boolean
False
AboveGround
IBiomass
False
cultivars
SugarcaneCultivar
True
cult
SugarcaneCultivar
True
num_layers
int32
False
plants
(/m2)
double
True
lodge_redn_photo
()
double
True
lodge_redn_sucrose
()
double
True
lodge_redn_green_leaf
()
double
True
WaterUptake
double
False
DaysAfterSowing
(days)
int32
False
crop_status
()
String
False
stage
()
double
False
stage_code
()
double
False
stagename
()
String
False
ratoon_no
()
int32
False
phase_tt
(oC)
double
False
tt_tot
(oC)
double
False
leaf_no
()
double
False
node_no_dead
()
double
False
node_no_detached
()
double
False
leaves
()
double
False
green_leaves
()
double
False
dead_leaves
()
double
False
leaf_area
()
double
False
leaf_dm
()
double
False
height
(mm)
double
False
root_depth
(mm)
double
False
cover_green
()
double
False
radn_int
(mj/m2)
double
False
cover_tot
()
double
False
lai_sum
()
double
False
tlai
()
double
False
tla
()
double
False
slai
()
double
False
lai
(m
2/m
2)
double
False
RootLengthDensity
(mm/mm3)
double
False
rlv_tot
(mm/mm3)
double
False
ll_dep
(mm)
double
False
lai2
(g/m^2)
double
False
leaf_wt2
(g/m^2)
double
False
rootgreenwt
(g/m^2)
double
False
leafgreenwt
(g/m^2)
double
False
sstem_wt
(g/m^2)
double
False
cane_dmf
(0-1)
double
False
canefw
(t/ha)
double
False
ccs
(%)
double
False
scmstf
(g/g)
double
False
scmst
(g/g)
double
False
sucrose_wt
(g/m^2)
double
False
cabbage_wt
(g/m^2)
double
False
cane_wt
(g/m^2)
double
False
biomass
(g/m^2)
double
False
green_biomass
(g/m^2)
double
False
greenwt
(g/m^2)
double
False
senescedwt
(g/m^2)
double
False
dm_dead
(g/m^2)
double
False
dlt_dm
(g/m^2)
double
False
partition_xs
(g/m^2)
double
False
dlt_dm_green
(g/m^2)
double
False
dlt_dm_detached
(g/m^2)
double
False
n_critical
(g/g)
double
False
n_minimum
(g/g)
double
False
n_conc_leaf
(g/m^2)
double
False
n_conc_cab
(g/m^2)
double
False
n_conc_cane
(g/m^2)
double
False
n_leaf_crit
(g/m^2)
double
False
n_leaf_min
(g/m^2)
double
False
biomass_n
(g/m^2)
double
False
plant_n_tot
(g/m^2)
double
False
green_biomass_n
(g/m^2)
double
False
n_green
(g/m^2)
double
False
greenn
(g/m^2)
double
False
senescedn
(g/m^2)
double
False
dlt_n_green
(g/m^2)
double
False
swdef_pheno
()
double
False
swdef_photo
()
double
False
swdef_expan
()
double
False
swdef_stalk
()
double
False
nfact_photo
()
double
False
nfact_expan
()
double
False
oxdef_photo
(0-1)
double
False
ep
(mm)
double
False
cep
(mm)
double
False
sw_uptake
(mm)
double
False
sw_demand
(mm)
double
False
sw_demand_te
(mm)
double
False
fasw
(0-1)
double
False
esw_layr
(mm)
double
False
no3_tot
(g/m^2)
double
False
n_demand
(g/m^2)
double
False
no3_demand
(kg/ha)
double
False
n_supply
(g/m^2)
double
False
NitrogenUptake
(g/m2)
double
False
nh4_uptake
(g/m2)
double
False
no3_uptake_pot
(g/m2)
double
False
nh4_uptake_pot
(g/m2)
double
False
CropType
String
False
IsAlive
boolean
False
IsReadyForHarvesting
boolean
False
CultivarNames
String
False
Links (Dependencies)
Name
Type
IsOptional?
Clock
IClock
False
Weather
IWeather
False
Soil
Soil
False
waterBalance
ISoilWater
False
soilPhysical
IPhysical
False
Summary
ISummary
False
NO3
ISolute
False
NH4
ISolute
False
nutrient
Nutrient
False
Events published
Name
Type
Sowing
Void Sowing (Object sender, EventArgs e)
Harvesting
Void Harvesting (Object sender, EventArgs e)
Killing
Void Killing (Object sender, EventArgs e)
BiomassRemoved
Void BiomassRemoved (BiomassRemovedType Data)
Methods (callable from manager)
Name
Description
root_proportion
double root_proportion(int32 Layer_ob, double Dlayer, double RootDepth)
Root_proportions the specified layer_ob.
on_day_of
boolean on_day_of(int32 stage_no, double current_stage)
On_day_ofs the specified stage_no.
stage_is_between
boolean stage_is_between(int32 start_ob, int32 finish_ob, double current_stage)
Stage_is_betweens the specified start_ob.
linint_3hrly_temp
double linint_3hrly_temp(double i_tmax, double i_tmin, double i_temps, double i_y)
Linint_3hrly_temps the specified i_tmax.
temp_3hr
double temp_3hr(double i_tmax, double i_tmin, int32 i_period)
Temp_3hrs the specified i_tmax.
accumulate_ob
void accumulate_ob(double i_value, double i_array_zb, double i_index_ob, double i_dlt_index)
accumulate_zb
void accumulate_zb(double i_value, double io_array_zb, double i_index_zb, double i_dlt_index)
error_margin
double error_margin(double Variable)
Error_margins the specified variable.
get_cumulative_index_real
int32 get_cumulative_index_real(double cum_sum, double A)
Get_cumulative_index_reals the specified cum_sum.
fill_real_array
void fill_real_array(double A_zb, double Value, int32 StopLayer_ob)
l_bound
double l_bound(double A, double MinVal)
L_bounds the specified a.
u_bound
double u_bound(double A, double MaxVal)
U_bounds the specified a.
bound
double bound(double A, double MinVal, double MaxVal)
Bounds the specified a.
max
double max(double A)
Allows any number of parameters (unlike Math.Max())
min
double min(double A)
Allows any number of parameters (unlike Math.Min())
bound_check_integer_var
void bound_check_integer_var(int32 value, int32 lower, int32 upper, String vname)
Bound_check_integer_vars the specified value.
Harvest
void Harvest(boolean removeBiomassFromOrgans)
Harvest the crop
GetWaterUptakeEstimates
ZoneWaterAndN GetWaterUptakeEstimates(SoilState soilstate)
Placeholder for SoilArbitrator
GetNitrogenUptakeEstimates
ZoneWaterAndN GetNitrogenUptakeEstimates(SoilState soilstate)
Placeholder for SoilArbitrator
SetActualWaterUptake
void SetActualWaterUptake(ZoneWaterAndN info)
SetActualNitrogenUptakes
void SetActualNitrogenUptakes(ZoneWaterAndN info)
Sow
void Sow(String cultivar, double population, double depth, double rowSpacing, double maxCover, double budNumber, double rowConfig, double seeds, int32 tillering, double ftn)
Sows the plant
SowNewPlant
void SowNewPlant(double PlantingDensity, double Depth, String CultivarName)
Sow a Newly Planted Sugarcane Crop. (crop_status is set to "crop_alive")Sugarcane will keep ratooning indefinitely until it is stopped by using an EndCrop or KillCrop.NB. All Ratoons are treated the same. No difference between first ratoon and second, third etc.
SowRatoon
void SowRatoon(double PlantingDensity, double Depth, String CultivarName, int32 StartingRatoonNo)
Sow a Sugarcane Crop BUT starting with a Ratoon instead of Newly Planted Crop. (crop_status is set to "crop_alive")However can still sow a Newly Planted Crop by setting StartingRatoonNo = 0.Sugarcane will keep ratooning indefinitely until it is stopped by using an EndCrop or KillCrop.NB. All Ratoons are treated the same. No difference between first ratoon and second, third etc.
HarvestCrop
void HarvestCrop()
KillCrop
void KillCrop()
EndCrop
void EndCrop()
LodgeTheCane
void LodgeTheCane()
HillUpTheSoil
void HillUpTheSoil(double CaneFr, double TopsFr)
Mound soil around base of crop and bury some plant material.Burying the plant material incorporates it as fresh organic matter into the Soil.This applies no matter the state of the plant material: Green, Senesced and DeadCan only do a HillUp during the Emergence phase (Sprouting to BeginCane).
BiomassRemovalComplete
void BiomassRemovalComplete(double fractionRemoved)
Biomass has been removed from the plant.
2.2 CanopyEnergyBalanceInterceptionlayerType[]
Methods (callable from manager)
Name
Description
Get
CanopyEnergyBalanceInterceptio... Get(int32 )
Set
void Set(int32 , CanopyEnergyBalanceInterceptio... )
Address
CanopyEnergyBalanceInterceptio... Address(int32 )
3 References
Carberry, P.S., Abrecht, D.G., 1991. Tailoring crop models to the semi-arid tropics., Eds: Muchow, R.C. and Bellamy, J.A., 157-182.
Godwin, D. C., Vlek, P. L. G., 1985. Simulation of Nitrogen Dynamics in Wheat Cropping Systems. NATO ASI Science, Springer US, 311-332.
Hammer, G. L., Muchow, R. C., 1994. Assessing climatic risk to sorghum production in water-limited subtropical environments I. Development and testing of a simulation model. Field Crops Research 36 (3), 221-234.
Meinke, H., Hammer, G. L., Chapman, S. C., 1993. A SUNFLOWER SIMULATION-MODEL .2. SIMULATING PRODUCTION RISKS IN A VARIABLE SUBTROPICAL ENVIRONMENT. Agronomy Journal 85 (3), 735-742.
Muchow, R. C., Robertson, M. J., Wood, A. W., Keating, B. A., 1996. Effect of nitrogen on the time-course of sucrose accumulation in sugarcane. Field Crops Research 47 (2), 143-153.
Muchow, R. C., Spillman, M. F., Wood, A. W., Thomas, M. R., 1994. Radiation interception and biomass accumulation in a sugarcane crop grown under irrigated tropical conditions. Australian Journal of Agricultural Research 45 (1), 37-49.
Probert, M. E., Dimes, J. P., Dalal, R. C., Strong, W. M., 1996. APSIM SOILWAT and SOILN: validation against observed data for a cracking clay soil.. Proceedings of the 8th Australian Agronomy Conference, Toowoomba,   Queensland, Australia, 30 January-2 February, 1996., Eds: Asghar, Mohammad, 458-461.
Robertson, M. J., Lilley, J. M., 2016. Simulation of growth, development and yield of canola (Brassica napus) in APSIM. Crop  and  Pasture Science 67 (3-4), 332-344.
Robertson, M. J., Wood, A. W., Muchow, R. C., 1996. Growth of sugarcane under high input conditions in tropical Australia. I. Radiation use, biomass accumulation and partitioning. Field Crops Research 48 (1), 11-25.
Robertson, M.J., Carberry, P.S., 1998. Simulating growth and development of soybean in APSIM.., 130-136.
Singh, G., Chapman, S. C., Jackson, P. A., Lawn, R. J., 2002. Lodging reduces sucrose accumulation of sugarcane in the wet and dry tropics. Australian Journal of Agricultural Research 53 (11), 1183-1195.
Van Keulen, H., Seligman, N. G., 1987. Simulation of water use, nitrogen nutrition and growth of a spring wheat crop. Simulation of water use, nitrogen nutrition and growth of a spring wheat crop..
