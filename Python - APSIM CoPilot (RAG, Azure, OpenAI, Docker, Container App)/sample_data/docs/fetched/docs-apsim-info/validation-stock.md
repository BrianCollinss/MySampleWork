# Stock documentation

Source URL: https://docs.apsim.info/validation/Stock
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:53:28.435830+00:00

1 The APSIM Stock Model
The APSIM STOCK Model
Neville Herrmann (CSIRO)
Andrew Moore (CSIRO)
Eric Zurcher (CSIRO)
Dean Holzworth (AgResearch/CSIRO)
Mark Lieffering (AgResearch)
Val Snow (AgResearch)
Acknowledgements
CSIRO, AgResearch and the APSIM Initiative (
https://www.apsim.info/
) for funding the implemetation of the Stock model into APSIMX
Introduction
The Stock model is an implementation of the CSIRO’s Australian Feeding Standards as expressed in the Grazplan (
https://grazplan.csiro.au/
) model. The technical document for Grazplan is available at available at
https://grazplan.csiro.au/wp-content/uploads/2007/08/TechPaperMay12.pdf
. The model has been ported into APSIM NextGeneration to allow full interaction with other models within the APSIM environment. Animals in Stock show the full range of growth, reproduction and death processes (see Grazplan) and the basic management actions (mating, weaning, moving, feeding supplements, selling and buying) have been implemented. All crop/plant models in APSIM NextGeneration have a ‘damage’ interface implemented and so allow grazing as soon as the Stock are moved to the same location (zone or paddock) as the crop.
Different animal species, genotypes and ages can be modelled together and various management systems implemented, including, but not limited to, forage crop grazing, rotational grazing, moving stock from paddock to feedlot and back again. Management can be simulated via Operations managers as in the simulations here or scripts. The collection of simulations in the Stock example shows examples of simulating three different management regimes (forage grazing, rotational grazing and a wheat/feedlot system) using scripts.
Management of animal groups is done by user-assigned tag values that take integer values. Tag values have two purposes:
They can be used to manage distinct groups of animals in a common fashion. For example, all lactating ewes might be assigned the same tag value, and then all animals with this tag value might undergo the same supplementary feeding regime.
If tag values are assigned sequentially (starting at 1), they can be used to generate summary variables. For example, WeightTag[1] gives the average live weight of all animals in groups with a tag value of 1.
Note that animal groups with different tag values are never merged, even if they are otherwise similar.
To set the tag value of an animal group, use the Tag method.
To determine the tag value of an animal group, use the TagNo variable.
Animal groups that become sufficiently similar are automatically merged into a single group.Animals are similar if all these criteria are the same:
Occupy the same paddock
Reproduction status (Castrated, Male, Empty, Early Preg,  Late Preg)
Number of foetuses
Mating cycle (day in the mating cycle)
Days to mating (Days left in joining period)
Pregnancy (Days since conception)
Lactation status (Days since parturition (if lactating)) – within 7 days
Has (not) young
If young exist, their reproductive status must be the same
Implants (hormone implants)
Mean age (if the animals are less than one year old )
These two simulations in this file are validations of the Stock model in APSIM Next Gen. A description of the validation dataset can found in the "Description" memo of the "Validation" node. Descriptions of the two validation simulations ("LUDF" and "StockSlurp")  can be found in the memos of the respective simulations.
The parameters and variables that can be specified in the Stock model are in the auto-generated APSIM Next Gen documentation which can be found at
https://apsimnextgeneration.netlify.app/
under the "Model documentation" link; the "Stock" model is near the bottom of the first table.
Changing Stock Parameters
Overview
The Stock model is an implementation of the CSIRO’s Australian Feeding Standards as expressed in the Grazplan (
https://grazplan.csiro.au/
) model.
The technical paper describing Grazplan can be found at
https://grazplan.csiro.au/wp-content/uploads/2007/08/TechPaperMay12.pdf
The structure of the stock parameter strings (which can be found after adding a "Genotype" model to the "Stock" node) is not user friendly but they are needed so the code stays aligned with Grazplan.
A useful resource to decipher the stock parameter strings are the Sheep and Cattle Explorer Excel spreadsheets. The Explorers are also useful to explore the effect of changing parameter values and they can be found at:
https://grazplan.csiro.au/wp-content/uploads/2007/08/SheepExplorer.xlsx
for sheep and
https://grazplan.csiro.au/wp-content/uploads/2007/08/CattleExplorer.xlsx
for cattle
Example
An example on how to change a parameter value is shown below. In the example, the Growth rate constant (CN1), which controls growth (and hence potential intake) will be changed from its default of 0.0115 to 0.015
in the Cattle Explorer Excel sheet, go to the "Pot.Intake" worksheet and confirm that the default Growth rate constant (CN1) is 0.0115 (cell B6). Note also any other numbers listed above or below the desired parameter - this is to double check you have the right value in the Stock GUI
in the genotype node (in the validation example "Friesian" under the main Stock node; this is where the animal's default parameters are defined) look for the appropriate name in the left hand column i.e. Growth
this name is followed by notation ("Growth C c-n-") in which the last two characters match that in the Explorer (CN)
ensure that the default value (0.0115) in the Cattle Explorer is found in the array in the right hand column along with the other numbers next to the default value of interest. This is just to ensure that you are dealing with the right parameter value
double click the array and change the default value to the new, desired value (0.015)
click away to save the changed value
Stock
The STOCK component encapsulates the GRAZPLAN animal biology model, as described in
M Freer et al., 1997
.
[The GrazPlan animal model technical description](
https://grazplan.csiro.au/wp
content/uploads/2007/08/TechPaperMay12.pdf)
Animals may be of different genotypes. In particular, sheep and cattle may be represented within a single STOCK instance.
Usually a single STOCK module is added to an AusFarm simulation, at the top level in the
module hierarchy.
In a grazing system, however, there may be a variety of different classes of livestock. Animals
may be of different genotypes (including both sheep and cattle); may be males, females or
castrates; are likely to have a range of different ages; and females may be pregnant and/or
lactating. The set of classes of livestock can change over time as animals enter or leave the
system, are mated, give birth or are weaned. Further, animals that are otherwise similar may be
placed in different paddocks, where their growth rates may differ.
In the STOCK component, this complexity is handled by representing the set of animals in a
simulated system as a list of animal groups (Figure 2.1). The members of each animal group
have the same genotype and age class, but may have a range of ages (for example, an animal
group containing mature animals may include four year old, five year old and six year old
stock). The members of each animal group also have the same stage of pregnancy and/or
lactation; the same number of suckling offspring; and occupy the same paddock.
The set of animal groups changes as animals enter and leave the simulation, and as
physiological events such as maturation, mating, birth or weaning take place. Animal groups
that become sufficiently similar are merged into a single group. The state of any unweaned
lambs or calves is stored alongside that of their mothers; at weaning, the male and female
weaners are transferred into two new animal groups within the main list.
In addition to the biological state variables that describe the animals, each animal group has
four attributes that are of particular interest when writing management scripts.
Index
Each animal group has a unique, internally assigned integer index, starting at 1.
Because the set of groups present in a component instance is dynamic, the index
number associated with a particular group of animals can – and usually does – change
over time. This dynamic numbering scheme has consequences for the way that animals
of a particular kind must be located when writing management scripts.
Paddock
Each animal group is also assigned a paddock. The forage and supplementary feed
available to a group of animals are determined by the paddock it occupies. Paddocks are
referred to by name in the STOCK component:
To set the paddock occupied by an animal group, use the
Move
event.
To determine the paddock occupied by an animal group, use the
Paddock
variable.
It is the user’s responsibility to ensure that paddock names correspond to PADDOCK
modules or other sources of necessary driving variables.
Tag Value
Each animal group also has a user assigned tag value that takes an integer value. Tag
values have two purposes:
They can be used to manage distinct groups of animals in a common fashion. For
example, all lactating ewes might be assigned the same tag value, and then all
animals with this tag value might undergo the same supplementary feeding regime.
If tag values are assigned sequentially (starting at 1), they can be used to generate
summary variables. For example,
WeightTag[1]
gives the average live weight
of all animals in groups with a tag value of 1.
Note that animal groups with different tag values are never merged, even if they are
otherwise similar.
To set the tag value of an animal group, use the
Tag
method.
To determine the tag value of an animal group, use the
TagNo
variable.
Merging groups of similar animals
Animal groups that become sufficiently similar are merged into a single group.
Animals are similar if all these are the same:
Occupy the same paddock
Reproduction status (Castrated, Male, Empty, Early Preg,  Late Preg)
Number of foetuses
Mating cycle (day in the mating cycle)
Days to mating (Days left in joining period)
Pregnancy (Days since conception)
Lactation status (Days since parturition (if lactating)) – within 7 days
Has (not) young
If young exist, their reproductive status must be the same
Implants (hormone implants)
Mean age (if the animals are less than one year old )
2 Validation
Validation Data Set Description
This vignette describes the origins and development of the Stock Model validation dataset. A high-level overview of the Stock Model itself can be found by clicking the Stock node.
The observed data set used for this validation simulation is based on publicly available data from New Zealand’s Lincoln University Dairy Farm
LUDF
. The LUDF is a commercial demonstration dairy farm established in 2001 and operated by the South Island Dairying Development Centre (SIDDC) on behalf of Lincoln University to showcase sustainable and profitable dairy farming.
The LUDF is located at 1504 Shands Road, Lincoln (New Zealand; 43°38'S 172°26'E). The property is 186 ha of which 160 ha is the milking platform. The different soil types on the farm represent most of the common soil types in the surrounding Canterbury region. The farm operates in the top 2% of NZ dairy farms on profitability.  The farm’s targets and goals have varied over the years but in the 2019/20 season the target stocking rate was 3.5 cows/ha (peak milked), milk production of 1750 kg MS/ha (equivalent to 500kg MS/cow i.e. >100% liveweight in milk production), application of 160kg N/ha plus 300kg DM/cow imported supplement. Most cows are wintered off farm.
Average annual rainfall of 666 mm per annum is supplemented by an average irrigation input of 450 mm; average evapotranspiration for Lincoln is 870 mm/year. The milking platform was sown at conversion from a sheep operation (March 2001) in a mix of ryegrasses with white clovers, and a small amount of Timothy.
The breed of cows at the LUDF are “KiwiCross” which was established as a separate breed in 2005 and is now New Zealand's most popular breed. In the 2019/20 season peak number of cows milked was 555 with the average days in milk of 282 days. The stocking rate of 3.5 cows/ha is equivalent to 1,665 kg liveweight/ha. In terms of feeding, in 2019/20 the cows ate 4.4 t DM/cow as pasture and 0.2 t DM/cow as supplement. Off-farm grazing was 0.7 t DM/cow giving a total feed intake of 5.4 t DM/cow.
Weekly farm data from the LUDF is available at
http://www.siddc.org.nz/lu-dairy-farm/weekly-data/
as PDF reports. The data for the years 2004 to 2017 was transcribed and collated into an validation dataset.
Note that for these validation simulations, one stock parameter was changed from the original values in
M Freer et al., 1997
to reflect modern dairy cow genetics. This, which can be accessed via the GUI in the "Friesian" node in the Stock model, was "Potential intake (Intake C c-i-[2]) which was increased from 0.025 to 0.04. Also note that "Dairy intake peak (c-idy-0)"  was set to 1.
Acknowledgements
South Island Dairying Development Centre (SIDDC) and Lincoln University for making the dairy farm’s production data available. Scott Rains, Samuel Dennis, Anna Taylor, Rogerio Cichota and Ronaldo Vibart for collating the LUDF data when working at AgResearch. Ron Pellow (SIDDC) for comments on the collated data set. David Pacheco and Ronaldo Vibart (AgResearch) for further discussions about the data.
Feedlot simulation
The Feedlot simulation is a simple illustration of the Stock model by using animals entering a feedlot, being fed and milked and leaving the feedlot. In addition to the Stock model itself and the Supplement specification GUI, at its core is the ChangeLactatingCowNumber GUI, the Stock operations and the Supplement operations. Both the operation files are based on stock and pasture/supplement data from the LUDF dataset (see the Validation memo for details). In the Stock operations, data on actual LUDF weekly changes in cow number have been collated from 1/08/2004 to 30/06/2017. Positive numbers denote lactating cows have been bought and moved to the feedlot while negative numbers are when cows are dried off and removed from the feedlot. It was assumed that cows were bought at weekly intervals. For the Supplement operations, six types of supplements labelled "silage_11me", "pasture_11.5me", "pasture_12me", "pasture_12.5me", "pasture_13me", and "pasture_13.5me" are bought. An excess amount of each supplement is bought (enough to last longer than the simulation) and their characteristics must be specified in the Supplement node. The LUDF dataseet is based on weekly reports - the daily pasture and supplements supplied (and their characteristics such as ME content) listed in the Supplement operations was extrapolated from the weekly data to the intervening days.
###SLURP simulation
Ideally, to test the Stock model’s estimates of production we would have a simulation of the animals rotationally grazing paddocks in which an appropriate pasture was growing. That would be a good reflection of reality but would also add errors in the modelling of plant growth and rotation rules that might obscure the testing of the Stock model.
SLURP is a “crop” model that has been built using the Plant Modelling Framework to provide a very simple representation of crops and pastures with user-prescribed growth rather than an internal calculation of growth. The model does not predict crop growth, development or yields - these are supplied by the user. By continually setting the SLURP biomass and pasture quality at the start of the day to the pre-grazing values measured at LUDF, we can use SLURP to test the performance of Stock without adding in other sources of error.
In this simulation, the pre-grazing pasture characteristics of SLURP are set in the SlurpPreGrazingSet component under the Paddock node. Here the relevant Excel file and worksheet are specified; within which PreGrazingCover, GrazedArea, PastureMEConc, PastureDigestibility, and SupplementOffered are specified. In addition to the Stock model itself, at its core the simulation has the ChangeLactatingCowNumber component, Stock Operations and SLURP. Stock Operations continually resets the number of dairy cows with the values taken from the LUDF dataset (see the Validation memo for details). In the Stock Operations, data on actual LUDF weekly changes in cow number have been collated from 1-Aug-2004 to 30-Jun-2017. Positive numbers denote lactating cows have been bought and moved to the feedlot while negative numbers are when cows are dried off and removed from the paddock. It was assumed that cows were bought at weekly intervals. For SLURP, the LUDF dataset is based on weekly reports - the pasture and supplement supplied in the SLURP Excel spreadsheet was extrapolated to the intervening days.
2.1 FeedlotTests
Experiment Name
Design (Number of Treatments)
Hersom1
(3)
Hersom2
(3)
Sharman1
(4)
Sharman2
(4)
Coleman
(2)
Paco
(5)
3 Sensibility
This sensibility test explores dual-purpose wheat in a high rainfall livestock system in south-eastern Australia.
It is based on
Sprague et al., 2015
Sheep grazing wheat and fed in feedlot
In this example simulation sheep are bought and sold on specified date.
They are fed supplement in a feedlot at a set rate, but graze a wheat crop when crop biomass >= 2.4 t/ha.
Sheep are moved from the wheat crop and back to feedlot when crop biomass reaches 0.5 t/ha or crop zadok = 31.
Activities in this manager:
Buy animals at start of year & put in feedlot
Move animals from feedlot to crop when ready to graze
Move animals from crop to feedlot
Shear all animals on specified date
Sell all animals at end of year
NOTES
When the animals are in the feedlot and an animal dies during the day, the supplement has already been fed into the feedlot
based on the number of animals in the feedlot at the start of the day. This means the remaining animals have access to slightly
more supplement and causes a spike in supp intake graph.
When sheep are culled for age + purchased to maintain stocking rate, several groups of sheep are created.
This causes irregular amounts of supplement to be fed.
3.1 TemperatureResponse
Experiment Name
Design (Number of Treatments)
BeefCattleTemperatureResponse
(5)
SheepTemperatureResponse
(5)
4 MassBalanceCheck
This simulation checks the mass balance of animal /
plant
interactions. The plant model used is a PMF Slurp model that doesn't simulate plant growth. This makes it much easier to check mass balance.
The checks are done in the MassBalanceCalculations manager script.
The script first calculates the amount of pasture removed:
PastureRemoved = StartOfDayPasture - EndOfDayPasture;
and the weight gain of the animals on a day.
LiveWeightGain = EndOfDayAnimal - StartOfDayAnimal;
It then calculates two balance terms that should be zero. The first checks that pasture removed = animal intake.
LostWt = PastureRemoved.Wt - Intake.Wt;
The second checks that N in the pasture removed = excreta N + the N retained by the animal.
LostN = PastureRemoved.N - (Excreta.N + LiveWeightGain.N);
A check is then made (on day 1 of the simulation) that the faeces from the animal is added to the surface organic matter (som) model:
if (!MathUtilities.FloatsAreEqual(animals.FaecesAll.Weight, som.Wt))
throw new Exception("Mass balance error: The animal faeces weight on day 1 is not equal to surface organic matter weight.");
A check is then made (on day 1 of the simulation) that the urine from the animal is added to the soil urea pool:
if (!MathUtilities.FloatsAreEqual(animals.UrineNAll, MathUtilities.Sum(urea.kgha)))
throw new Exception("Mass balance error: The animal urine on day 1 is not equal to soil urea amount.");
These last checks are only done on day 1 because flows in and out of SOM and Urea pools make it difficult to calculate mass balance. The assumption is that if it works on day 1 it works for all other days.
This simulation checks the mass balance of animal /
supplement
interactions. The checks are done in the MassBalanceCalculations manager script.
The script first calculates the amount of pasture removed:
PastureRemoved = StartOfDayPasture - EndOfDayPasture;
and the weight gain of the animals on a day.
LiveWeightGain = EndOfDayAnimal - StartOfDayAnimal;
It then calculates two balance terms that should be zero. The first checks that supplement removed = animal intake.
LostWt = SupplementRemoved.Wt - Intake.Wt;
The second checks that N in the pasture removed = excreta N + the N retained by the animal.
LostN = PastureRemoved.N - (Excreta.N + LiveWeightGain.N);
A check is then made (on day 1 of the simulation) that the faeces from the animal is added to the surface organic matter (som) model:
if (!MathUtilities.FloatsAreEqual(animals.FaecesAll.Weight, som.Wt))
throw new Exception("Mass balance error: The animal faeces weight on day 1 is not equal to surface organic matter weight.");
A check is then made (on day 1 of the simulation) that the urine from the animal is added to the soil urea pool:
if (!MathUtilities.FloatsAreEqual(animals.UrineNAll, MathUtilities.Sum(urea.kgha)))
throw new Exception("Mass balance error: The animal urine on day 1 is not equal to soil urea amount.");
These last checks are only done on day 1 because flows in and out of SOM and Urea pools make it difficult to calculate mass balance. The assumption is that if it works on day 1 it works for all other days.
5 Interface
5.1 Stock
Properties (Outputs)
Name
Description
Units
Type
Settable?
Structure
IStructure
True
RandSeed
int32
True
Genotypes
Genotypes
False
StockModel
StockList
True
AnimalGroups
AnimalGroup
False
Trampling
kg/ha
double
False
SuppEaten
-
SupplementEaten
False
NoGroups
-
int32
False
Number
-
int32
False
NumberAll
-
int32
False
NumberTag
-
int32
False
NumberYng
-
int32
False
NumberYngAll
-
int32
False
NumberYngTag
-
int32
False
NoFemale
-
int32
False
NoFemaleAll
-
int32
False
NoFemaleTag
-
int32
False
NoFemaleYng
-
int32
False
NoFemaleYngAll
-
int32
False
NoFemaleYngTag
-
int32
False
NoMale
-
int32
False
NoMaleAll
-
int32
False
NoMaleTag
-
int32
False
NoMaleYng
-
int32
False
NoMaleYngAll
-
int32
False
NoMaleYngTag
-
int32
False
DeathsAll
-
int32
False
Deaths
-
int32
False
DeathsTag
-
int32
False
Sex
-
String
False
Age
d
double
False
AgeAll
d
double
False
AgeTag
d
double
False
AgeYng
d
double
False
AgeYngAll
d
double
False
AgeYngTag
d
double
False
AgeMonths
month
double
False
AgeMonthsAll
month
double
False
AgeMonthsTag
month
double
False
AgeMonthsYng
month
double
False
AgeMonthsYngAll
month
double
False
AgeMonthsYngTag
month
double
False
Weight
kg
double
False
WeightAll
kg
double
False
WeightTag
kg
double
False
WeightYng
kg
double
False
WeightYngAll
kg
double
False
WeightYngTag
kg
double
False
BaseWt
kg
double
False
BaseWtAll
kg
double
False
BaseWtTag
kg
double
False
BaseWtYng
kg
double
False
BaseWtYngAll
kg
double
False
BaseWtYngTag
kg
double
False
BaseWtEmpty
kg
double
False
CondScore
-
double
False
CondScoreAll
-
double
False
CondScoreTag
-
double
False
CondScoreYng
-
double
False
CondScoreYngAll
-
double
False
CondScoreYngTag
-
double
False
MaxPrevWt
kg
double
False
MaxPrevWtAll
kg
double
False
MaxPrevWtTag
kg
double
False
MaxPrevWtYng
kg
double
False
MaxPrevWtYngAll
kg
double
False
MaxPrevWtYngTag
kg
double
False
FleeceWt
kg
double
False
FleeceWtAll
kg
double
False
FleeceWtTag
kg
double
False
FleeceWtYng
kg
double
False
FleeceWtYngAll
kg
double
False
FleeceWtYngTag
kg
double
False
CFleeceWt
kg
double
False
CFleeceWtAll
kg
double
False
CFleeceWtTag
kg
double
False
CFleeceWtYng
kg
double
False
CFleeceWtYngAll
kg
double
False
CFleeceWtYngTag
kg
double
False
FibreDiam
um
double
False
FibreDiamAll
um
double
False
FibreDiamTag
um
double
False
FibreDiamYng
um
double
False
FibreDiamYngAll
um
double
False
FibreDiamYngTag
um
double
False
Pregnant
d
double
False
PregnantAll
d
double
False
PregnantTag
d
double
False
Lactating
d
double
False
LactatingAll
d
double
False
LactatingTag
d
double
False
NoFoetuses
-
double
False
NoFoetusesAll
-
double
False
NoFoetusesTag
-
double
False
NoSuckling
-
double
False
NoSucklingAll
-
double
False
NoSucklingTag
-
double
False
BirthCS
-
double
False
BirthCSAll
-
double
False
BirthCSTag
-
double
False
Paddock
-
String
False
TagNo
-
int32
False
DSE
-
double
False
DSEAll
-
double
False
DSETag
-
double
False
DSEYng
-
double
False
DSEYngAll
-
double
False
DSEYngTag
-
double
False
WtChange
kg/d
double
False
WtChangeAll
kg/d
double
False
WtChangeTag
kg/d
double
False
WtChangeYng
kg/d
double
False
WtChangeYngAll
kg/d
double
False
WtChangeYngTag
kg/d
double
False
Intake
-
DMPoolHead
False
IntakeAll
-
DMPoolHead
False
IntakeTag
-
DMPoolHead
False
IntakeYng
-
DMPoolHead
False
IntakeYngAll
-
DMPoolHead
False
IntakeYngTag
-
DMPoolHead
False
PastIntake
-
DMPoolHead
False
PastIntakeAll
-
DMPoolHead
False
PastIntakeTag
-
DMPoolHead
False
PastIntakeYng
-
DMPoolHead
False
PastIntakeYngAll
-
DMPoolHead
False
PastIntakeYngTag
-
DMPoolHead
False
SuppIntake
-
DMPoolHead
False
SuppIntakeAll
-
DMPoolHead
False
SuppIntakeTag
-
DMPoolHead
False
SuppIntakeYng
-
DMPoolHead
False
SuppIntakeYngAll
-
DMPoolHead
False
SuppIntakeYngTag
-
DMPoolHead
False
MEIntake
MJ/d
double
False
MEIntakeAll
MJ/d
double
False
MEIntakeTag
MJ/d
double
False
MEIntakeYng
MJ/d
double
False
MEIntakeYngAll
MJ/d
double
False
MEIntakeYngTag
MJ/d
double
False
CPIntake
kg/d
double
False
CPIntakeAll
kg/d
double
False
CPIntakeTag
kg/d
double
False
CPIntakeYng
kg/d
double
False
CPIntakeYngAll
kg/d
double
False
CPIntakeYngTag
kg/d
double
False
CFleeceGrowth
kg/d
double
False
CFleeceGrowthAll
kg/d
double
False
CFleeceGrowthTag
kg/d
double
False
CFleeceGrowthYng
kg/d
double
False
CFleeceGrowthYngAll
kg/d
double
False
CFleeceGrowthYngTag
kg/d
double
False
FibreGrowthDiam
um
double
False
FibreGrowthDiamAll
um
double
False
FibreGrowthDiamTag
um
double
False
FibreGrowthDiamYng
um
double
False
FibreGrowthDiamYngAll
um
double
False
FibreGrowthDiamYngTag
um
double
False
MilkWt
kg/d
double
False
MilkWtAll
kg/d
double
False
MilkWtTag
kg/d
double
False
MilkME
MJ/d
double
False
MilkMEAll
MJ/d
double
False
MilkMETag
MJ/d
double
False
RetainedN
kg/d
double
False
RetainedNAll
kg/d
double
False
RetainedNTag
kg/d
double
False
RetainedNYng
kg/d
double
False
RetainedNYngAll
kg/d
double
False
RetainedNYngTag
kg/d
double
False
RetainedP
kg/d
double
False
RetainedPAll
kg/d
double
False
RetainedPTag
kg/d
double
False
RetainedPYng
kg/d
double
False
RetainedPYngAll
kg/d
double
False
RetainedPYngTag
kg/d
double
False
RetainedS
kg/d
double
False
RetainedSAll
kg/d
double
False
RetainedSTag
kg/d
double
False
RetainedSYng
kg/d
double
False
RetainedSYngAll
kg/d
double
False
RetainedSYngTag
kg/d
double
False
Faeces
-
DMPoolHead
False
FaecesAll
-
DMPoolHead
False
FaecesTag
-
DMPoolHead
False
FaecesYng
-
DMPoolHead
False
FaecesYngAll
-
DMPoolHead
False
FaecesYngTag
-
DMPoolHead
False
FaecesInorg
-
InorgFaeces
False
FaecesInorgAll
-
InorgFaeces
False
FaecesInorgTag
-
InorgFaeces
False
FaecesInorgYng
-
InorgFaeces
False
FaecesInorgYngAll
-
InorgFaeces
False
FaecesInorgYngTag
-
InorgFaeces
False
EnergyUse
-
EnergyUse
False
Methane
kg/d
double
False
MethaneAll
kg/d
double
False
MethaneTag
kg/d
double
False
MethaneYng
kg/d
double
False
MethaneYngAll
kg/d
double
False
MethaneYngTag
kg/d
double
False
UrineN
kg/d
double
False
UrineNAll
kg/d
double
False
UrineNTag
kg/d
double
False
UrineNYng
kg/d
double
False
UrineNYngAll
kg/d
double
False
UrineNYngTag
kg/d
double
False
UrineP
kg/d
double
False
UrinePAll
kg/d
double
False
UrinePTag
kg/d
double
False
UrinePYng
kg/d
double
False
UrinePYngAll
kg/d
double
False
UrinePYngTag
kg/d
double
False
UrineS
kg/d
double
False
UrineSAll
kg/d
double
False
UrineSTag
kg/d
double
False
UrineSYng
kg/d
double
False
UrineSYngAll
kg/d
double
False
UrineSYngTag
kg/d
double
False
RDPIntake
kg/d
double
False
RDPIntakeAll
kg/d
double
False
RDPIntakeTag
kg/d
double
False
RDPIntakeYng
kg/d
double
False
RDPIntakeYngAll
kg/d
double
False
RDPIntakeYngTag
kg/d
double
False
RDPReqd
kg/d
double
False
RDPReqdAll
kg/d
double
False
RDPReqdTag
kg/d
double
False
RDPReqdYng
kg/d
double
False
RDPReqdYngAll
kg/d
double
False
RDPReqdYngTag
kg/d
double
False
RDPFactor
0-1
double
False
RDPFactorAll
0-1
double
False
RDPFactorTag
0-1
double
False
RDPFactorYng
0-1
double
False
RDPFactorYngAll
0-1
double
False
RDPFactorYngTag
0-1
double
False
IntakeModifier
-
double
False
IntakeModifierAll
-
double
False
IntakeModifierTag
-
double
False
IntakeModifierYng
-
double
False
IntakeModifierYngAll
-
double
False
IntakeModifierYngTag
-
double
False
Links (Dependencies)
Name
Type
IsOptional?
systemClock
Clock
False
locWtr
IWeather
False
suppFeed
Supplement
True
outputSummary
ISummary
False
paddocks
Zone
False
Methods (callable from manager)
Name
Description
ByTag
AnimalGroup ByTag(int32 tag)
Return animal groups that have a specific tag number.
Add
void Add(StockAdd animals)
Causes a set of related age cohorts of animals to enter the simulation.Each age cohort may contain animals that are pregnant and/or lactating, in which case distributions of numbers of foetuses and/or suckling offspring are computed automatically.This event is primarily intended to simplify the initialisation of flocks and herds in simulations.
Buy
void Buy(StockBuy stock)
Buys animals (i.e. they enter the simulation). The purchased animals will form a new animal group that is placed at the end of the list of animal groups.
Buy
void Buy(String genotype, double number, ReproductiveType sex, double age, double weight, double fleeceWeight, int32 tag)
Buys animals (i.e. they enter the simulation). The purchased animals will form a new animal group that is placed at the end of the list of animal groups.
Sell
int32 Sell(int32 number, AnimalGroup group)
Remove the specified number of animals (not including unweaned lambs/calves).
Sell
int32 Sell(int32 number, AnimalGroup groups)
Shear
double Shear(boolean shearAdults, boolean shearYoung, AnimalGroup group)
Shears sheep. The event has no effect on cattle.
Move
void Move(String paddockName, AnimalGroup group)
Moves animals to a specified paddock.
Join
void Join(String mateTo, int32 mateDays, AnimalGroup group)
Commences mating of a particular group of animals.  If the animals are not empty females, or if they are too young, has no effect
Castrate
void Castrate(int32 number, AnimalGroup group)
Converts ram lambs to wether lambs, or bull calves to steers.  If the animal group(s) denoted by group has no suckling young, has no effect.If the number of male lambs or calves in a nominated group is greater than the number to be castrated, the animal group will be split;the sub-group with castrated offspring will remain at the original index and the sub-group with offspring that were not castrated willbe added at the end of the set of animal groups.
Wean
void Wean(int32 number, boolean weanMales, boolean weanFemales, AnimalGroup group)
Weans some or all of the lambs or calves from an animal group.The newly weaned animals are added to the end of the list of animal groups, with males and females in separate groups.
DryOff
void DryOff(int32 number, AnimalGroup group)
Ends lactation in cows that have already had their calves weaned.  The event has no effect on other animals.If the number of cows in a nominated group is greater than the number to be dried off, the animal group will be split;the sub-group that is no longer lactating will remain at the original index and the sub-group that continues lactating will be added at the end of the set of animal groups
SplitByAge
AnimalGroup SplitByAge(int32 age, AnimalGroup group)
Split animal group by age
SplitByWeight
AnimalGroup SplitByWeight(double weight, AnimalGroup group)
Split animal group by weight
SplitByYoung
AnimalGroup SplitByYoung(AnimalGroup group)
Split animal group by young.
Sort
void Sort()
TramplingMass
double TramplingMass(String paddockName)
Get the trampling mass for the specified paddock
5.2 Genotypes
Encapsulates a collection of stock genotype parameters. It can read the GrazPlan .prm
files as well as the APSIM ruminant JSON file format.
Properties (Outputs)
Name
Description
Units
Type
Settable?
All
GenotypeWrapper
False
Names
String
False
Methods (callable from manager)
Name
Description
ReadPRM
void ReadPRM(String xmlString)
Read a parameter set and append to the json array.
Add
void Add(Genotype animalParameterSet)
Set the user specified genotypes.
Get
Genotype Get(String genotypeName)
Get a genotype. Throws if not found.
5.3 StockList
StockList is primarily a list of AnimalGroups. Each animal group has a
"current paddock" (function getInPadd() ) and a "group tag" (function getTag()
associated with it. The correspondences between these and the animal
groups must be maintained.
In addition, the class maintains two other lists:
FPaddockInfo  holds paddock specific information.  Animal groups are
related to the members of FPaddockInfo by the FPaddockNos
array.
FSwardInfo    holds the herbage availabilities and amounts removed from
each sward (i.e. all components which respond to the
call for "sward2stock").  The animal groups never refer to
this information directly; instead, the TStockList.Dynamics
method (1) aggregates the availability in each sward into
a paddock level total, and (2) once the grazing logic has
been executed it also allocates the amounts removed between
the various swards.  Swards are allocated to paddocks on
the basis of their FQDN's.
N.B. The use of a fixed length array for priorities and paddock numbers
limits the number of animal groups that can be stored in this
implementation.
N.B. The At property is 1 offset.  In many of the management methods, an
index of 0 denotes "do to all groups".
Properties (Outputs)
Name
Description
Units
Type
Settable?
Animals
AnimalGroup
False
Paddocks
PaddockInfo
False
Enterprises
EnterpriseInfo
False
ForagesAll
ForageProviders
False
Methods (callable from manager)
Name
Description
ComputeStepAvailability
void ComputeStepAvailability(int32 posIdx)
Caluculate ration availability
ComputeIntakeLimit
void ComputeIntakeLimit(AnimalGroup group)
Calculate the intake limit
Add
int32 Add(AnimalGroup animalGroup, PaddockInfo paddInfo, int32 tagNo)
Add a group of animals to the listReturns the group index of the group that was added. 0->n
Add
int32 Add(Animals animalInits)
Returns the group index of the group that was added. 0->n
Add
void Add(StockAdd stockInfo)
Adds animals.
Delete
void Delete(int32 posn)** N.B. posn is 1-offset; stock list is effectively also a 1-offset array*
Count
int32 Count()
HighestTag
int32 HighestTag()
PlaceSuppInPadd
void PlaceSuppInPadd(String paddName, double suppKG, FoodSupplement supplement, boolean feedSuppFirst)
Place the supplement in the paddock
Dynamics
void Dynamics()
ReturnMassPerArea
double ReturnMassPerArea(String paddockName, String units)
Get the mass of animals per ha
ReturnMassPerArea
double ReturnMassPerArea(PaddockInfo thePadd, ForageProvider provider, String units)
Get the mass for the area
ReturnExcretion
void ReturnExcretion(PaddockInfo thePadd, ExcretionInfo excretion)
SexString
String SexString(int32 idx, boolean useYoung)
Return the reproductive status of the group as a string.  These stringsare compatible with the ParseRepro routine.
GrowthCurve
double GrowthCurve(int32 ageDays, ReproType reprodStatus, Genotype parameters)
Calculate the growth from the standard growth curve
AddCohorts
void AddCohorts(CohortsInfo cohortsInfo, int32 dayOfYear, double latitude, int32 newGroups)
Wean
void Wean(int32 groupIdx, int32 number, boolean weanFemales, boolean weanMales)
See the notes to the Castrate method; but weaning is even furthercomplicated because males and/or females may be weaned.
Wean
int32 Wean(AnimalGroup group, int32 number, boolean weanFemales, boolean weanMales)
Wean animals in an animal group.
DryOff
void DryOff(AnimalGroup groups, int32 number)
Split
int32 Split(int32 groupIdx, int32 numToKeep)
Break an animal group up in various ways; by number, by age, by weightor by sex of lambs/calves.  The new group(s) have the same priority andpaddock as the original.  SplitWeight assumes a distribution of weightsaround the group average.
Split
int32 Split(AnimalGroup group, int32 numToKeep)
Break an animal group up in various ways; by number, by age, by weightor by sex of lambs/calves.  The new group(s) have the same priority andpaddock as the original.  SplitWeight assumes a distribution of weightsaround the group average.
SplitByAge
AnimalGroup SplitByAge(int32 ageDays, AnimalGroup groups)
SplitByWeight
AnimalGroup SplitByWeight(double splitWt, AnimalGroup groups)
SplitByYoung
AnimalGroup SplitByYoung(AnimalGroup groups)
Sort
void Sort()
IsGiven
boolean IsGiven(double x)
Tests for a non-MISSING, non-zero value
DaysFromDOY365Simple
int32 DaysFromDOY365Simple(int32 firstDOY, int32 secondDOY)
Calculate the days from the day of year in a non leap year
Add
void Add(AnimalGroup animalList, PaddockInfo paddInfo, int32 tagNo)
Buy
void Buy(StockBuy stockInfo)
Buy animals.
5.4 SupplementEaten[]
Methods (callable from manager)
Name
Description
Get
SupplementEaten Get(int32 )
Set
void Set(int32 , SupplementEaten )
Address
SupplementEaten Address(int32 )
5.5 DMPoolHead[]
Methods (callable from manager)
Name
Description
Get
DMPoolHead Get(int32 )
Set
void Set(int32 , DMPoolHead )
Address
DMPoolHead Address(int32 )
5.6 DMPoolHead
Dry matter pool
Properties (Outputs)
Name
Description
Units
Type
Settable?
Weight
kg/d
double
True
N
kg/d
double
True
P
kg/d
double
True
S
kg/d
double
True
AshAlk
mol/d
double
True
5.7 InorgFaeces[]
Methods (callable from manager)
Name
Description
Get
InorgFaeces Get(int32 )
Set
void Set(int32 , InorgFaeces )
Address
InorgFaeces Address(int32 )
5.8 InorgFaeces
Inorganic faeces type
Properties (Outputs)
Name
Description
Units
Type
Settable?
N
kg/d
double
True
P
kg/d
double
True
S
mol/d
double
True
5.9 EnergyUse[]
Methods (callable from manager)
Name
Description
Get
EnergyUse Get(int32 )
Set
void Set(int32 , EnergyUse )
Address
EnergyUse Address(int32 )
6 Science Documentation
View science documentation here
7 References
M Freer, A.D Moore, J.R Donnelly, 1997. GRAZPLAN: Decision support systems for Australian grazing enterprises—II. The animal biology model for feed intake, production and reproduction and the GrazFeed DSS. Agricultural Systems 54 (1), 77 - 126.
Sprague, S. J., Kirkegaard, J. A., Dove, H., Graham, J. M., McDonald, S. E., Kelman W. M., 2015. Integrating dual-purpose wheat and canola into high-rainfall livestock systems in south-eastern Australia. 1. Crop forage and grain yield. Crop and Pasture Science 66, 365-376.
