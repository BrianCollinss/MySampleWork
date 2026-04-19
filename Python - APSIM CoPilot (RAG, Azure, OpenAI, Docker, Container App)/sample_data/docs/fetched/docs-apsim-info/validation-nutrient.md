# Nutrient documentation

Source URL: https://docs.apsim.info/validation/Nutrient
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:05.726143+00:00

1 The APSIM Nutrient Model
The soil nutrient model includes functionality for simulating pools of organic matter and mineral nitrogen.  The processes for each are described below.
This work builds upon earlier APSIM soil organic matter models such as SoilN (
Probert et al., 1998
).
The N pools / flows simulate the nitrate content of each soil layer.  Much of the capability has been taken from implementation in the APSIM SoilN model (
Probert et al., 1998
), which was derived from approaches in CERES-Maize ([jones_ceres-maize:_1986]).
1.1 Structure
Soil organic matter is modelled as a series of discrete organic matter pools which are described in terms of their masses of carbon and nutrients. These pools are initialised according to approaches specific to each pool.  Organic matter pools may have carbon flows, such as a decomposition process, associated to them.  These carbon flows are also specific to each pool, are independently specified, and are described in each case in the documentation for each organic matter pool below.
Mineral nutrient pools (e.g. Nitrate, Ammonium, Urea) are described as solutes within the model.  Each pool captures the mass of the nutrient (e.g. N,P) and they may also contain nutrient flows to describe losses or transformations for that particular compound (e.g. denitrification of nitrate, hydrolysis of urea).
1.2 Pools
A nutrient pool class is used to encapsulate the carbon and nitrogen within each soil organic matter pool.  Child functions within these classes provide information for initialisation and flows of C and N to other pools, or losses from the system.
The soil organic matter pools used within the model are described in the following sections in terms of their initialisation and the carbon flows occurring from them.
1.3 Solutes
The soil mineral nutrient pools used within the model are described in the following sections in terms of their initialisation and the flows occurring from them.
2 Validation
The Soil Nutrient model has been tested on a variety of datasets studying the impact of management (tillage, cropping rotation, nitrogen management) on soil carbon, nitrogen and crop productivity for a range of soil types and environments.
2.1 Map
2.2 Australia
Test data are provided for three locations across Australia, ranging from warmer subtropical Queensland, through to cooler temperature locations in southern Australia.
2.2.1 Tarlee
This Rotation Trial (
Schultz, 1995
) was located near the township of Tarlee (34.28 S, 138.77E) in South Australia from 1979 to 1996.  It was established on a Red Brown Earth to monitor the long term effects of rotations on soil properties and crop production.  In this test, we use data for continuous wheat, and wheat-fallow rotations with 3 stubble treatments (burning, incorporation, retention) and 3 Nitrogen rates (0,40,80 kg/ha).
Experiment Name
Design (Number of Treatments)
Tarlee
Rotation x Stubble x N (18)
2.2.2 Hudson
This dataset demonstrates cropping system performance and soil carbon dynamics under continuous winter cereal versus perrenial pasture.  The cropping and pasture systems experiment was established in August 1994 on the farming property ‘Hudson’ located in the foothills of the Liverpool Ranges (31.758S, 150.458E; average annual rainfall 684mm with some summer dominance, average annual pan evaporation 1718 mm).  Further details about the experiment and the data can be found at
Young et al., 2009
and
Paydar et al., 2005
.
Experiment Name
Design (Number of Treatments)
Hudson
Treatment (2)
2.2.3 Brigalow Catchment Study
This dataset was originally simulated using APSIM by
Huth et al., 2010
.  The study was conducted near Theodore, Queensland, Australia (24.81°S, 149.80°E). Several catchments were monitored under different land uses following clearing of native Brigalow Forest (Acacia Harpophylla).  Data for part of the cropping catchment are used here.
Experiment Name
Design (Number of Treatments)
Brigalow
Catchment (1)
2.2.4 Horsham
Experiment Name
Design (Number of Treatments)
Horsham
Treatment (3)
2.3 North America
2.3.1 Pendleton
The Pendleton Long Term Experiment (
Rasmussen et al., 1998
) was established in 1931 at Oregon State University’s Columbia Basin Agricultural Research Center near Pendleton, OR (45.72 N, 118.63 W).
It consisted of nine treatments consisting of crop residue (fall burn, spring burn, and no burn) and fertility
(0, 45, and 90 kg N/ha, manure, and pea vine) management practices under a Winter Wheat-Summer Fallow system.
All plots were tilled using a moldboard plow, cultivated, and rod-weeded to control weeds.
Experiment Name
Design (Number of Treatments)
Pendleton
Treatment (9)
3 Sensibility
3.1 N2O
N2O emmisions are modelled by the APSIM Nutrient model.  Further work is encouraged to test and improve this part of the model.  Till then, sensibility tests are conducted to ensure that the results from the model meet basic expectations from previous studies.
This very simple sensibility test ensures that the following criteria hold for a range of different farming systems across different geographical locations:
Oil Palm in Papua New Guinea
Wheat in Southern Queensland, Australia.
Sugarcane in Northern Queensland, Australia
Maize in Malawi, Africa.
Tests check that the following are maintained:
Total annual N2O losses from denitrification are relatively low (less than 25 kg N/ha/y)
N2O losses from denitrification lie within 10% and 25% of total N losses from denitrification
Total annual N2O losses from nitrification are very low (less than 3 kg N/ha/y)
3.2 Incubation
Experiment Name
Design (Number of Treatments)
Incubation
Temperature x InitialP (6)
4 Interface
4.1 Nutrient
Parameters (Inputs)
Name
Description
Units
Type
Value
DirectedGraphInfo
DirectedGraph
APSIM.Shared.Graphing.DirectedGraph
ResourceName
String
Nutrient
Text
String
Text
String
Expression
String
Text
String
Expression
String
Text
String
Expression
String
Text
String
FixedValue
double
Text
String
Expression
String
Text
String
Expression
String
Text
String
FixedValue
double
DestinationNames
String
DestinationFraction
double
PotentialRate
double
VariableName
String
Text
String
Expression
String
Text
String
Expression
String
Text
String
FixedValue
double
DestinationNames
String
DestinationFraction
double
PotentialRate
double
VariableName
String
Text
String
Expression
String
Text
String
Expression
String
Text
String
FixedValue
double
DestinationNames
String
DestinationFraction
double
PotentialRate
double
VariableName
String
Text
String
Expression
String
Text
String
Expression
String
Text
String
Expression
String
Text
String
FixedValue
double
FixedValue
double
DestinationNames
String
DestinationFraction
double
Text
String
PropertyName
String
StringValue
String
Text
String
FixedValue
double
FixedValue
double
VariableName
String
VariableName
String
Text
String
Expression
String
Text
String
Expression
String
Text
String
Expression
String
Text
String
DestinationNames
String
DestinationFraction
double
Text
String
PotentialRate
double
FixedValue
double
PropertyName
String
StringValue
String
X
double
Y
double
VariableName
String
X
double
Y
double
VariableName
String
MineralisationSTBase
double
MineralisationSTOpt
double
Expression
String
X
double
Y
double
VariableName
String
FixedValue
double
FixedValue
double
SourceName
String
DestinationName
String
Text
String
FixedValue
double
FixedValue
double
FixedValue
double
SourceName
String
Text
String
DenitrificationRateModifier
double
IsInertActive
boolean
FixedValue
double
FixedValue
double
N2ODiffusionCoefficient
double
Text
String
SourceName
String
DestinationName
String
Text
String
PotentialNitrificationRate
kg/ha/d
double
ConcentrationAtHalfMax
ppm
double
FixedValue
double
FixedValue
double
FixedValue
double
Text
String
sourceName
String
destinationName
String
VariableName
String
VariableName
String
sourceName
String
destinationName
String
VariableName
String
VariableName
String
VariableName
String
FixedValue
double
Properties (Outputs)
Name
Description
Units
Type
Settable?
Structure
IStructure
True
Inert
IOrganicPool
True
Microbial
IOrganicPool
True
Humic
IOrganicPool
True
FOMCellulose
IOrganicPool
True
FOMCarbohydrate
IOrganicPool
True
FOMLignin
IOrganicPool
True
NO3
ISolute
True
NH4
ISolute
True
Urea
ISolute
True
FOM
IOrganicPool
True
TotalC
kg/ha
double
False
Catm
kg/ha
double
False
Natm
kg/ha
double
False
N2Oatm
kg/ha
double
False
DenitrifiedN
kg/ha
double
False
NitrifiedN
kg/ha
double
False
HydrolysedN
kg/ha
double
False
MineralisedN
kg/ha
double
False
Organic
IOrganicPool
False
TotalOrganicN
kg/ha
double
False
FOMCNRFactor
double
False
CNRF
double
False
MineralN
kg/ha
double
False
TotalN
kg/ha
double
False
Links (Dependencies)
Name
Type
IsOptional?
summary
ISummary
False
soilPhysical
IPhysical
False
nutrientPools
OrganicPool
False
nutrientFlows
NFlow
False
surfaceResidue
OrganicPool
False
Methods (callable from manager)
Name
Description
DoIncorpFOM
void DoIncorpFOM(FOMLayerType FOMdata)
Incorporate the given FOM C and N into each layer
IncorpFOMPool
void IncorpFOMPool(FOMPoolType FOMPoolData)
Partition the given FOM C and N into fractions in each layer (FOM pools)
Reset
void Reset()
AddSolute
void AddSolute(Solute solute)
Add a solute.
5 References
Huth, N. I., Thorburn, P. J., Radford, B. J., Thornton, C. M., 2010. Impacts of fertilisers and legumes on N2O and CO2 emissions from soils in subtropical agricultural systems: A simulation study. Agriculture Ecosystems  and  Environment 136 (3-4), 351-357.
Paydar, Z., Huth, N., Ringrose-Voase, A., Young, R., Bernardi, T., Keating, B., Cresswell, H., 2005. Deep drainage and land use systems. Model verification and systems comparison. Australian Journal of Agricultural Research 56 (9), 995-1007.
Probert, M. E., Dimes, J. P., Keating, B. A., Dalal, R. C., Strong, W. M., 1998. APSIM's Water and Nitrogen Modules and Simulation of the Dynamics of Water and Nitrogen in Fallow Systems. Agricultural Systems 56 (1), 1-28.
Rasmussen, P E, Albrecht, S L,, Smiley, R W, 1998. Soil C and N changes under tillage and cropping systems in semi-arid Pacific Northwest agriculture. Soil and Tillage Research 47, 197-205.
Schultz, J.E., 1995. Crop production in a rotation trial at Tarlee, South Australia. Australian Journal of Experimental Agriculture 35 (865-876).
Young, R. R., Wilson, B., Harden, S., Bernardi, A., 2009. Accumulation of soil carbon under zero tillage cropping and perennial. AUSTRALIAN JOURNAL OF SOIL RESEARCH 47 (3), 273-285.
