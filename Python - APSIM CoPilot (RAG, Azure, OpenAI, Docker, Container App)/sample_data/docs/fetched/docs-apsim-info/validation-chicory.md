# Chicory documentation

Source URL: https://docs.apsim.info/validation/Chicory
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:51:50.885703+00:00

1 The APSIM Chicory Model
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
Rogerio Cichota, AgResearch, New Zealand
###Preamble
Chicory (
Cichorium intybus
L.) is a perennial herb of the family Asteraceae native of Europe and Asia, but which can now be found in meadows worldwide ([Garnock-Jones_1987];
Hare et al., 1990
;
Alloush et al., 2003
;
Li et al., 2005
).  Two main varieties have been selected for agricultural uses, leaf chicory (var.
foliosum
), which has been cultivated for salads as well as a forage for livestock, and root chicory (var.
sativum
), grown for the taproots, which are used as a coffee substitute and more recently as dietary supplement (
Dielen et al., 2005
;
Ernst et al., 1995
;
Jurgonski et al., 2011
).  Wild varieties are considered a weed in many countries.
The chicory plant has large shiny hairless leaves that grow from a basal rosette (
Hare et al., 1990
;
Ernst et al., 1995
;
Reaume, 2010
), it has a thick deep taproot which holds carbohydrate reserves that allow prompt re-growth after winter or defoliation.  When vernalised, chicory will produce stems which contain several bright blue flowers, the stems become woody when flower buds start to appear.  Leaves do grow on stems but are quite small and decrease in size from the base to apex.  Flower buds form sequentially in the axils of the upper leaves of the main stem and branches.  Seeds reach maturity rapidly so the same plant will have young flowers and ripe seeds at the same time (
Hare et al., 1990
;
Moloney et al., 1993
;
Clapham et al., 2001
;
Reaume, 2010
).
Forage chicory is considered a moderately persistent herb under grazing conditions, its initial rosette can split into multiple crowns after the first growing season (
Moloney et al., 1993
;
Li et al., 1997
;
Clapham et al., 2001
;
Li et al., 2005
).  Rapid establishment during spring/summer months provides a great competitive advantage for forage chicory over weeds, and it can be used as a monoculture or in mixed swards.  Chicory has shown resilience and potential to produce high yields at a wide range of management strategies (
Clark et al., 1990
;
Jung et al., 1996
;
Li et al., 1997
;
Labreveux et al., 2004
), and is highly palatable to ruminants (
Clark et al., 1990
;
Jung et al., 1996
).  The palatability of forage chicory will lead to preferential grazing and result in a competitive disadvantage for chicory in mixed swards, compromising its persistence.  Although it is classified as a perennial plant, the population in a chicory sward will decrease over time due to a variety of reasons, such as disease, selective grazing, and physical damage by trampling.
Objective:
The model presented here has been built using the Plant Modelling Framework (PMF) of
Brown et al., 2014
to simulate the growth of a forage chicory crop.  Currently, the focus is mainly on describing biomass accumulation and regrowth after harvest on monocultural swards.  As part of APSIM's PMF, using chicory for multicropping is possible, but this has not been fully tested.  Likewise, describing the effect of population decrease on plant growth was not attempted; for swards free of weeds and within typical population ranges, the changes in plant size (number of crowns) and LAI should compensate for loss of population (e.g.
Li et al., 1997
), but competition with weeds would alter this pattern significantly.
Simulating variations in plant N concentration (seasonal or due to changes in soil status)  is not a focus of the current model.  Further development of PMF's supply/demand processes are needed for capturing the variations in N content typically seen in perennial species.
Generic background
Presentation
This model has been developed to simulate the growth of a forage chicory crop.  The chicory model focus, thus, on describing primarily the vegetative growth, with a simplified account of the reproductive phase, without explicit considering flowers and seeds (these may be included in future releases).  The model was built using the Plant Modelling Framework (PMF) described by
Brown et al., 2014
.  To simulate the aboveground plant structure, including the photosynthesis process, the Chicory model uses the SimpleLeaf organ type of PMF.  The model describes a semi-perennial crop, with phenology rewinding to the vegetative stage at the end of the reproductive phase.
Inclusion in APSIM simulations
A forage chicory crop can be included in a simulation the same as any other APSIM crop.
The chicory object can be dragged or copied from the Crop folder in the tool box into a Field in your simulation;
To become active, chicory needs to be sown using a manager script with a sowing rule. e.g.:
Chicory.Sow(cultivar: Puna, population: 200, depth: 10, rowSpacing: 150);
If a specified cultivar is not available, a fatal error will be thrown.
Harvest and biomass removal
Chicory biomass can be removed by raising one of the valid methods: Harvest, Cut, Graze, or Prune; this is done using a manager script, like for other crops.  The proportion of the biomass of each organ that is removed from the system and/or added to the residue pools may be specified; otherwise defaults will be used.  Note that the sum of fractions removed and added to residue should be <= 1.0.  To specify the proportions for removal in a manager script, use a RemovalFractions class as shown below:
[EventSubscribe("Commencing")]
private void OnSimulationCommencing(object sender, EventArgs e)
{
RemoveFraction = new RemovalFractions(Chicory.Organs);
}
[EventSubscribe("DoManagement")]
private void OnDoManagement(object sender, EventArgs e)
{
if (Clock.Today.Date == HarvestDate)
{
RemoveFraction.SetFractionToRemove("Leaf", 0.80);
RemoveFraction.SetFractionToRemove("Stem", 0.50);
RemoveFraction.SetFractionToResidue("leaf",0.05);
Chicory.Harvest(RemoveFraction);
}
}
The RemovalFractions class can be sent with Harvest, Cut, Graze, or Prune events. All parameters are optional, defaults are used whenever any value is not specified.
Crop termination
To fully terminate a crop the EndCrop event should be raised:
Chicory.EndCrop();
Once a crop has been ended the field is open to be used by another APSIM plant model, or another chicory crop.  Note that ending chicory is not necessary before sowing another crop, competition for resources will take place between crops when there is more than one in the field.
Acknowledgements
This model was developed with help from Russel McAuliffe and Brittany Paton organising data and simulations. Datasets were kindly shared by Julia M. Lee, Hamish E. Brown, and the Forages for Reduced Nitrogen Leaching (FRNL) programme.
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
Stem
Models.PMF.Organs.GenericOrgan
Inflorescence
Models.PMF.Organs.GenericOrgan
Taproot
Models.PMF.Organs.GenericOrgan
Root
Models.PMF.Organs.Root
PerPlantBelowGroundWt
Models.Functions.DivideFunction
ShootRootRatio
Models.Functions.PhaseLookup
TargetShootRootRatio
Models.Functions.PhaseLookup
StemsLeafRatio
Models.Functions.PhaseLookup
TargetStemsLeafRatio
Models.Functions.PhaseLookup
FlowerStemRatio
Models.Functions.PhaseLookup
TargetFlowerStemRatio
Models.Functions.PhaseLookup
TaprootRootRatio
Models.Functions.PhaseLookup
TargetTaprootRootRatio
Models.Functions.PhaseLookup
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
BelowGround
Models.PMF.CompositeBiomass
BelowGroundLive
Models.PMF.CompositeBiomass
Total
Models.PMF.CompositeBiomass
1.3 Cultivars
Cultivar Name
Alternative Name(s)
Puna
Puna
Choice
Choice
1.4 Child Components
1.4.1 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
The duration of each phenologic phase in Chicory is controlled in general by the accumulation of thermal time; for the reproductive phase, vernalisation and photoperiod are also used.
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
Represents all leaves in the plant.  There is no distiction between age or place in the canopy.
1.4.4 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note:
Represents all the stems and branches in the plant, without any distinction between age or the position in the canopy.
1.4.5 Inflorescence
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note:
Represents the reproductive parts of the plant, flowers, pods, seeds, etc.
1.4.6 Taproot
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note:
This represents all the taproots of the plant.
Taproots are quite important for chicory growth, they are the a major reserve organ and can supply N and sugars whenever the plant needs are not fully satified by uptake or photosynthesis.  This can prompt growth especially at the end of winter and following defoliation.  To mimic this process, the chicory model can use the non-structural fraction of taproots' biomass to complement demand for new growth; this is done via biomass retranslocation.  There is a considerable amount of information about the build up of reserves in roots of chicory, although most of it is for root cultivars, some developed specifically for high sugar content in roots ().  Mono- and di-saccharides (fructose and sucrose) and especilly fructans (inulin) reserves build up over the growing season in the taproot and their content can reach 60-80% of dry mater before winter (
Améziane et al., 1995
;
Ernst et al., 1995
;
Demeulemeester et al., 1998
;
Monti et al., 2005
).  This content is much lower for leaf varieties, reaching 20-30% (
Li et al., 1997
;
Ernst et al., 1995
;
Quijada, 2015
), and this can be further reduced if defoliation is severe or the plants have bolted ([Arias-carbajalThesis_1994];
Li et al., 1997
;
Schittenhelm, 2001
;
Cranston et al., 2016
).  Build up of N reserves (as proteins and amino-acids) in taproots has also been show to occur at the end of growing season, and then being used for shoot development after winter (
Limami et al., 1996
;
Bewley, 2002
).  The controls for the build up of reserves of the rate that they can be used for new growth are still mostly unknown.  Defoliation and environmental stress (drought) have been shown to reduce the reserves (
Li et al., 1997
,
Monti et al., 2005
), but it is not clear whether this is a direct affect or due to the greater usage of reserves to cope with the stresses.
For the chicory model the set of parameters related to the storage of reserves and their utilisation (StructuralFraction, DMRetranslocationFactor, and NRetranslocationFactor) were manipulated in order to reflect the trends shown in the literature review and attempting to reproduce the results of the available experimental datasets (used in the validation tests).  These should be upgraded when data more specific to the retranslocation process is made available.
1.4.7 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
Note:
this represents all the fine roots of the plant.
There is no distinction of age, but root biomass is allocated separately for each soil layer within the root zone.   The depth of the root zone can change over time as root grows.
1.4.8 PerPlantBelowGroundWt
A class that divides all child functions.
Returns zero if nominator is zero, returns double.maxValue if denominator is zero.
This represents the live biomass dry weight below ground for a specific plant (in g/plant)
1.4.9 ShootRootRatio
Look up a value based upon the current growth phase.
1.4.10 TargetShootRootRatio
Look up a value based upon the current growth phase.
The target shoot:root ratio is used by the chicory model to ensure prompt regrowth after a defoliation event.   The model will change the allocation of new growth attempting to keep a balance in organ biomass distribution. This is a simplified approach to biomass allocation plasticity (e.g.
Wilson, 1988
;
Levang-Brilz et al., 2002
), it assumes that the plant adjusts the allocation biomass whenever the current shoot:root ratio differs from the target value.
Estimated values for the shoot:root ratio of chicory based on published data vary considerably and are linked to cultivar (e.g.
Li et al., 1997
;
Zagal et al., 2001
;
Belesky et al., 2004
).  Varieties bred for root harvesting can have shoot:root ratios al low as 1.0 whereas for leaf cultivars the values may be up to around 5.0.  Data for forage chicory suggest values around 2.0 for adult plants (
Li et al., 1997
;
Labreveux, 2002
;
Alloush et al., 2003
;
Cranston, 2015
).  This value is likely to be affected by environmental conditions, with water or nutrient deficit causing a shift towards root growth, while leaf growth is favoured during low light conditions.  However, these relationships can vary considerably in different plants or cultivars and little data is available for forage chicory.  The model currently does not account fort the influence of environmental factors on the shoot:root partition, but it does account for vernalisation.
Stem elongation, or bolting, only occurs when chicory is vernalised and it has been shown happen in only a fraction of the plants in a sward each year (
Clapham et al., 2001
;
Sanderson, 2003
;
Dielen et al., 2005
).  This variation seems to be linked to the extent of vernalisation (
Gianquinto, 1997
;
Dielen et al., 2005
).  Thus, to account for the growth of both bolting and non-bolting plants, the chicory model assumes that the changes in biomass allocation are a function of vernalisation as well as phenological phase.
1.4.11 StemsLeafRatio
Look up a value based upon the current growth phase.
1.4.12 TargetStemsLeafRatio
Look up a value based upon the current growth phase.
The chicory model will change the allocation of biomass above ground attempting to keep a given proportion among the various organs.  The allocation of biomass to leaves is defined primarily by the target shoo:root ratio, but during the stem elongation and reproductive phases, the partition is modified favouring stems and inflorescence.  This is controlled by the target ratio of stems+inflorescence to leaves. This ratio may be affected by defoliation (
Clark et al., 1990
;
Li et al., 1994
;
Quijada, 2015
) and is probably affected by enviromental factors too. There is not enough data to describe these interactions.
The allocation of biomass to stems is difficult to inferr because there are many confounding issues, such as growth stage, vernalisation level, graze intensity, etc.  Under lax grazing conditions and after some vernalisation, stem fraction is about 25-50%, but can be as high as 70%, values around 15-25% are reported for intensive grazing (
Clark et al., 1990
;
Li et al., 1994
;
Jung et al., 1996
;
Li et al., 1997
;
Li et al., 1997
).  The Stem:leaf ratio is thus between 0.25 and 1.0 for grazed plants.
Bolting only occur in a fraction of plant each year, around 50% (
Clapham et al., 2001
;
Sanderson, 2003
;
Dielen et al., 2005
). So, as a compromise, it assumed in the chicory model that the stem:leaf ratio increases sharply at the beginning of the stem elongation phase, reaching a maximum value and then is constant until near the end of flowering when if decreases again.  This maximum should be not too high to account for non bolting plants.
1.4.13 FlowerStemRatio
Look up a value based upon the current growth phase.
1.4.14 TargetFlowerStemRatio
Look up a value based upon the current growth phase.
The allocation of biomass during the reproductive phase will attempt to keep a ratio between stem and inflorescence biomass.  This ratio is difficult to define as there is little data available and because of the flowering behaviour of chicory (plants can have ripe seeds as well as young flower buds at the same time).  Inference from published values suggest that the ratio of flower biomass to stems should be around 0.1 (
Hare et al., 1990
;
Clark et al., 1990
;
Jung et al., 1996
;
Clapham et al., 2001
).
1.4.15 TaprootRootRatio
Look up a value based upon the current growth phase.
1.4.16 TargetTaprootRootRatio
Look up a value based upon the current growth phase.
The chicory model assumes that biomass is allocated to the various organs in a manner to mantain a given proportion each each organ (this can vary with phenological phase and other factors).  The ratio between taproot and root biomass is assumed to increase as the biomass below ground per plant increases, but it approaches a maximum target asymptotically.  This is a simple approach and can describe the general behaviour of chicory plants, it may be upgraded if deemed necessary when more data becomes available.
1.4.17 MortalityRate
A constant function (name=value)
1.4.18 SeedMortalityRate
A constant function (name=value)
2 Validation
The performance of the chicory model is evaluated in simulations based on experiments conducted primarily in New Zealand.  Datasets from USA have been added, but they should be taken with caution as there was limited information about the experiments and published results corresponded to annual or seasonal aggregates only.  Values for population are included, but they were not explicitly simulated by the model; only a decrease in plant numbers as function of cuts or grazings is considered.  There is indication in the literature that severe defoliation may lead to higher death rate for chicory, but when the data from several publications were analysed together, no relationship could be found.  All experiments did show a decline in plant population over time.  It is likely that the timing of defoliation (soon before winter) and the severity of trampling (depending animal type and numbers, as well as soil conditions) are better indicators of plant mortality, but detailed information about this is lacking.
The evaluation of the model with regards to plant N concentration is only superficial and currently the model should not be seen as a proper tested with respect to N concentration.  However, the values for a few of the experiments do show reasonable agreement for magnitude and seasonal trends.  The overall lack of agreement are mainly caused by experiments with low N inputs, in some of these weeds may (especially clovers) may have provided some N to chicory, as in general measurements are higher than the model predictions.  There are also uncertainties regarding whether some of the data refer to whole plant, whole sward, or only fresh leaves.
So, the poor evaluation for N concentration can be attributed, on one hand, to the lack of data, in quantity and quality, for N content.   On the other hand, and more importatly, some of the underlying code don't seem to be able to describe some plant processes that are important under cut/grazing and re-growth conditions.  The PMF processes for computing N demand and supply (especially regarding remobilisation within living tissues) are still being developed.  One issue in particular is caused because the model attempts to keep all the plant tissues at maximum N concentration, which can lead to high N demands when the plant is in deficit.  The high demand and low supply end up restricting growth in the model beyond expectations. This may also reduce mobilisation of N (as well as DM) from storage organs (especially taproot in chicory) towards new growth.  Due to these issues, the current model cannot be considered fully fit for simulating N balance without expert judgment.  The model will be upgraded in the near future as the work on the code to improve the description of those specific processes is ongoing.
All available data
Notes:
Plant population is inclued here as but it is not considered for validation as it is not explictly simulated by the model;
Data of harvested biomass from USA simulations are not included as only seasonal or annual totals were available;
N concentration is currently poorly described and its predictedÃ—observed graph is not included;
Cumulative harvested material is typically used for evaluation of cut/grazed conditions;
2.1 New Zealand
All data from New Zealand
2.1.1 ScottFarm
All data from Scott Farm
2.1.1.1 ScottFarmC
Experiment Name
Design (Number of Treatments)
ScottFarmC
17a (1)
Simulation based on unpublished data from a field trial, part of SuperP farmlet, run at Scott farm, Hamilton, New Zealand, from 2008 to 2010.  Trial plots were sown with chicory only, cultivar '
Choice
'; seeds drilled (6 kg/ha) and broadcasted (2 kg/ha).  Sward was grazed by dairy cows.  Measurements taken comprise plant density (quadrat method) and yield (estimated assuming a fixed 5 cm residue).
Data supplied by Julia Lee, DairyNZ.
2.1.1.2 ScottFarmE
Experiment Name
Design (Number of Treatments)
ScottFarmE
stablishment (1)
Simulation based on unpublished data from a chicory establishment trial at Scott farm, Hamilton, New Zealand, run from 2009 to 2010.  Trial plots were sown with chicory, cultivar '
Choice
'; direct drilled after spraying existing pasture with herbicide.  Sward was mechanically mowned.  Measurements comprised of yield (plate meter technique) and population (quadrat method).
Data supplied by Julia Lee, DairyNZ.
2.1.1.3 ScottFarmD
Experiment Name
Design (Number of Treatments)
ScottFarmD
9a (1)
Simulation based on a field trial, part of ESP farmlet, run at Scott farm, Hamilton, New Zealand, from 2010 to 2011.  Trial plots were sown with chicory, cultivar '
Choice
'; seeds drilled (6 kg/ha) and broadcasted (2 kg/ha).  Measurements taken comprised plant density (quadrat method) and yield (raising plate techinique).  Sward was grazed by dairy cows.  The trial was similar to that of C17a, but is noted that yields here were much lower due to the dry period after sowing which reduced plant establishment and hence density.
Data supplied by Julia Lee, DairyNZ.
2.1.1.4 ScottFarmFD902
Experiment Name
Design (Number of Treatments)
ScottFarmFD902
Defoliation (8)
This simulation was setup based on observed data from a defoliation trial at Scott farm, Hamilton, New Zealand, run from 2010 to 2012.  The study investigated the effect of cut height (i.e. rotation length) and residual height on chicory growth.  The treatments consisted of trigerring cuts when the sward reached heights of 150, 250, 350, and 500 mm, in factorial combination with two residual heights, 30-50 or 60-80 mm.  Harvest was avoided over winter.
Trial plots were sown with chicory, cultivar '
Choice
' (6.7 kg/ha), after previous pasture was sprayed with herbicide and conventionally drilled.  Irrigation was applied in the first year and fertiliser was applied over summer.  Results reported include yield, botanical composition, plant density, leaf area and light interception (the later two only during one growing period - between two cuts).
References:
Lee, J.M.;  Hemmingson, N.R.; Minnee, E.M.K.; Clark, C.E.F. 2015. Management strategies for chicory (Cichorium intybus) and plantain (Plantago lanceolata): impact on dry matter yield, nutritive characteristics and plant density. Crop and Pasture Science, 66, 168â€“183.
2.1.2 Iversen
Experiment Name
Design (Number of Treatments)
Iversen
Irrigation (2)
This simulation setup was based on a dataset supplied by Hamish Brown, which was collected for his PhD thesis.  The trial was conducted at the Iversen field of Lincoln University, in Lincoln, New Zealand between 1996 and 2002.  The work was focused on understanding water use and its interaction with yield of dryland and irrigated forage crops (lucerne, chicory and red clover were used).
The trial was implemented in spring of 1996, after conventional cultivation, including subsoiling and liming.  Chicory (cultivar '
Puna
') was sown at a rate of 3.5 kg/ha and was grazed regularly by sheep.  There were annual applications of superphosphate but no N fertilisation (except excreta from grazing sheep).  The treatments consisted of  irrigation versus dryland conditions.  The major parameters analysed were herbage growth and yield, population, nutritive characteristics, and water utilisation.
Soil data was obtained from measurements presented by Brown (2004) and complemented by S-MAP database from Landcare Research Limited. Weather data was obtained from the nearby Broadfields met station in Lincoln.
References:
Brown, H.E. 2004. Understanding yield and water use of dryland forage crops in New Zealand. PhD thesis, Lincoln University, Lincoln, NZ. 288 p.
Brown, H.E.; Moot, D.J.; & Pollock, K.M. 2003. Long term growth rates and water extraction patterns of dryland chicory, lucerne and red clover. Legumes for dryland pastures. New Zealand Grassland Association, Research Practice Series, 11:91-99.
Brown, H.E.; Moot, D.J.; & Pollock, K.M. 2005. Herbage production, persistence, nutritive characteristics and water use of perennial forages grown over 6 years on a Wakanui silt loam. New Zealand Journal of Agricultural Research, 48(4):423-439.
2.1.3 FRNLLincoln
Experiment Name
Design (Number of Treatments)
FRNLLincoln
NRate (6)
Simulation setup based on field trial performed at Lincoln University Research Dairy Farm, New Zealand, between 2014 and 2016.  The experiment was part of the FRNL (Forages for Reduced Nitrogen Leaching) program and the data is basically unpublished (references will be added when they become available).
The experiment consisted of six fertiliser treatments (N rates of 0, 50, 100, 200, 350 and 500 kg/ha/yr) in three replicates, and measurements comprised yield and quality indicators (here N content is used).
Data supplied by Grant Edwards, Lincoln University.
The soil at the experimental site was a Templeton fine sandy loam (an Immature Pallic soil, USDA: Udic Haplustept), the required parameters were inferred based on data from the New Zealand National Soils Database (Landcare Research).
Weather data was obtained from nearby Broadfields weather station (NIWA).
2.1.4 FRNLRuakura
Experiment Name
Design (Number of Treatments)
FRNLRuakura
NRate (6)
This simulation setup was based on field trials performed at DairyNZ's Scott Farm, in Ruakura, New Zealand, between 2014 and 2016.  The experiment was part of the FRNL (Forages for Reduced Nitrogen Leaching) program and the data is basically unpublished (references will be added when they become available).
The experiment consisted of six fertiliser treatments (N rates of 0, 50, 100, 200, 350 and 500 kg/ha/yr) in three replicates, and measurements comprised yield and quality indicators (here N content is used).
Observed data supplied by Grant Edwards, Lincoln University.
The soil at the experimental site was a Horotiu silt loam (an Orthic Allophanic soil; USDA: Typic Udivitrand), the parameters required were inferred using data from the New Zealand National Soils Database (Landcare Research).
Weather data was obtained from nearby Ruakura weather station (NIWA).
2.2 USA
All data from USA
2.2.1 AFSRC_01
Experiment Name
Design (Number of Treatments)
AFSRC_01
CutFrequency (2)
This simulation was adapted from a defoliation trial in West Virginia, USA, conducted from 1994 to 1996 at the Appalachian Farming Systems Research Center.  The study investigated production and the nutritive value of chicory swards (cultivar '
Puna
' - among other species) as a function of clipping frequency (3- and 6-wk intervals).  Each treatment was replicated three times on an upland site of Dekalb soil series (loamy-skeletal, mixed, subactive, mesic Typic Dystrochrept).  Modest rates of N, P, and K were applied annually.  Herbage mass, botanical composition, in vitro organic matter disappearance (IVOMD), and crude protein (CP) were determined.
Met data for nearby location was obtained from NOAA.  The data set was largely complete except for solar radiation, which was added from an alternative interpolated set for this location, also from NOAA.
Information to assemble the soil data was based on a Ramsey series obtained from USGS.
References:
Belesky, D.P.; Fedders, J.M.; Turner, K.E.; Ruckle, J.M. 1999. Productivity, botanical composition, and nutritive value of swards including forage chicory. Agronomy Journal, 91(3): 450-456.
2.2.2 AFSRC_02
Experiment Name
Design (Number of Treatments)
AFSRC_02
NRate (5)
This simulation setup was based on a field experiment run for 3 years at the Appalachian Farming Systems Research Center in southern West Virginia, USA.  The sol at this location was a Ramsey soil (loamy, siliceous, subactive, mesic Lithic Dystrudept).  The study aimed to determine whether fertilizer N would influence forage chicory (cultivar '
Puna
') nutritive value and NO3-N concentration.  The treatments consisted of five N rates (0, 80, 160, 240, or 480 kg N/ha) replicated three times in a randomized block design. Swards were clipped at 6-wk intervals during the growing season.  Yield was reported either as annual totals or seasonal, using these the values for each harvest event were arbitrarily inferred (treat this data as rough approximation).
Met data was based on nearby location obtained from NOAA.  The data set was largely complete except for solar radiation, which was added from an alternative interpolated set for this location, also from NOAA.
Information to assemble the soil data was obtained from USGS.
References:
Belesky, D.P.; Turner, K.E.; Ruckle, J.M. 2000. Influence of nitrogen on productivity and nutritive value of forage chicory. Agronomy Journal, 92(3): 472-478.
2.2.3 RELARC_01
Experiment Name
Design (Number of Treatments)
RELARC_01
Cultivar x CutFrequency (4)
This simulation was adapted from a defoliation trial conducted in 1998-1999, at Russell E. Larson Agricultural Research Center, Pennsylvania, USA.  The trial consisted of small plots where two chicory cultivars ('
Puna
' and '
Feast
') were sown separately in May-1997 (at 4.5 kg seed/ha).  Plots were fertilized with 27 kgP and 72 kgK/ha in October 1997 and April 1999. Fertilizer N was applied at 56 kg/ha in June and July of 1998 and 1999.  Treatments were replicated five times and the soil at the site was a Hagerstown silt loam (fine, mixed, semiactive mesic Typic Hapludalfs).
Each plot was divided in two, one-half was harvested every 3 wk and the other half every 5 wk. Each strip was cut to a 7-cm height with a rotary mower.  Dates for each cut were given, but only annual and treatment yields are reported;  population numbers were provided for few specific dates.  Yield for each harvest were arbitrarily inferred, therefore they should be seen as rough indication only.
Met file was built using data obtained from NOAA website.  The data set was largely complete except for solar radiation, which was added from an alternative interpolated set for this location, also from NOAA.
Information to assemble the soil data was obtained from USGS.
References:
Sanderson, M.A.; Labreveux, M.; Hall, M.H.; Elwinger, G.F. 2003. Forage yield and persistence of chicory and english plantain. Crop Science, 43: 995-1000.
2.2.4 RELARC_02
Experiment Name
Design (Number of Treatments)
RELARC_02
Cultivars (3)
This simulation was adapted from a defoliation trial conducted in 2000-2001, at Russell E. Larson Agricultural Research Center, Pennsylvania, USA.  The trial was established in small plots, replicated five times, with chicory sown in April-1999 (Cultivars '
Puna
', '
Feast
' and '
La Certa
'.  All plots received 4.5 t/ha of limestone and were fertilized with 56 kg N/ha in April, June and July of 2000 and 2001.  The soil at the site was a Hagerstown silt loam (fine, mixed, semiactive mesic Typic Hapludalfs).
Each plot was mechanically mowed to height of 7cm every 4 weeks.  Only annual and treatment yields are reported, however population was given for specific dates.  Dates for each harvest were reported.  From these data, yield was arbitrarily inferred for each harvest, therefore they should be seen as rough approximation only (N variation between cultivars were considered here).
Met file was built using data obtained from NOAA website.  The data set was largely complete except for solar radiation, which was added from an alternative interpolated set for this location, also from NOAA.
Information to assemble the soil data was obtained from USGS.
References:
Sanderson, M.A.; Labreveux, M.; Hall, M.H.; Elwinger, G.F. 2003. Forage yield and persistence of chicory and english plantain. Crop Science, 43: 995-1000.
3 Interface
3.1 Chicory
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
Alloush, G. A.; Belesky, D. P., Clapham, W. M., 2003. Forage chicory: A plant resource for nutrient-rich sites. Journal of Agronomy and Crop Science 189 (2), 96-104.
Améziane, Rafiqa; Limami, Anis M.; Noctor, G, Morot-Gaudry, Jean-François, 1995. Effect of nitrate concentration during growth on carbon partitioning and sink strength in chicory. Journal of Experimental Botany 46, 1423-1428.
Belesky, D. P.; Ruckle, J. M., Clapham, W. M., 2004. Dry-matter production, allocation and nutritive value of forage chicory cultivars as a function of nitrogen. Journal of Agronomy and Crop Science 190 (2), 100-110.
Bewley, J. Derek, 2002. Root storage proteins, with particular reference to taproots. Canadian Journal of Botany 80 (4), 321-329.
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Clapham, W. M.; Fedders, J. M.; Belesky, D. P., Foster, J. G., 2001. Developmental dynamics of forage chicory. Agronomy Journal 93 (2), 443-450.
Clark, D. a.; Anderson, C. B., Berquist, T., 1990. Growth rates of ‘Grasslands Puna' chicory ( Cichorium intybus L.) at various cutting intervals and heights and rates of nitrogen. New Zealand Journal of Agricultural Research 33 (2), 213-217.
Clark, D.; Anderson, C., Hongwen, G., 1990. Liveweight gain and intake of Friesian bulls grazing 'Grasslands Puna' chicory (Cichorium intybus L.) or pasture. New Zealand Journal of Agricultural Research 33 (2), 219-224.
Cranston, L. M.; Kenyon, P. R.; Morris, S. T.; Lopez-Villalobos, N., Kemp, P. D., 2016. Morphological and physiological responses of plantain (Plantago lanceolata) and chicory (Cichorium intybus) to water stress and defoliation frequency. Journal of Agronomy and Crop Science 202 (1), 13-24.
Cranston, L. M.; Kenyon, P. R.; Morris, S. T.; Lopez-Villalobos, N.; & Kemp, P. D., 2015. Morphological and physiological responses of plantain (Plantago lanceolata) and chicory (Cichorium intybus) to water stress and defoliation frequency. Journal of Agronomy and Crop Science, 1-12.
Demeulemeester, M.A.C.; Verdoodt, V., De Proft, M.P., 1998. Interaction between physiological age and cold treatment on the composition and concentration of carbohydrates in chicory roots (Cichorium intybus L.). Journal of Plant Physiology 153 (3-4), 437-445.
Dielen, Vincent; Notté, Christine; Lutts, Stanley; Debavelaere, Vianney; Van Herck, Jean Claude, Kinet, Jean Marie, 2005. Bolting control by low temperatures in root chicory (Cichorium intybus var. sativum). Field Crops Research 94 (1), 76-85.
Ernst, Michael; Chatterton, N. Jerry, Harrison, Philip A., 1995. Carbohydrate changes in chicory (Cichorium intybus L. var. foliosum) during growth and storage. Scientia Horticulturae 63 (3-4), 251-261.
Gianquinto, G., 1997. Morphological and physiological aspects of phase transition in radicchio ( Cichorium intybus L . var . silvestre Bisch .): influence of daylength and its interaction with low temperature. Scientia Horticulturae 71, 13-26.
Hare, M. D.; Rowarth, J. S.; Archie, W. J.; Rolston, M. P., Guy, B. R., 1990. Chicory seed production: research and practice. Proceedings of the New Zealand Grassland Association 52, 91-94.
Jung, Gerald A.; Shaffer, John A.; Varga, Gabriella A., Everhart, John R., 1996. Performance of 'Grasslands Puna' chicory at different management levels. Agronomy Journal 88 (1), 104-111.
Jurgonski, Adam; Milala, Joanna; Juskiewicz, Jerzy; Zdunczyk, Zenon, Krol, Boguslaw, 2011. Composition of chicory root , peel , seed and leaf ethanol extracts and biological properties of their non-inulin fractions. Food Technology & Biothechnology 49 (1), 40-47.
Labreveux, M. E., 2002. Productivity of forage cultivars of chicory and plantain in the Northeast Region of the United States., 112.
Labreveux, María; Hall, Marvin H., Sanderson, Matt A., 2004. Productivity of chicory and plantain cultivars under grazing. Agronomy Journal 96 (3), 710-716.
Levang-Brilz, N., M. E. Biondini, 2002. Growth rate, root development and nutrient uptake of 55 plant species from the Great Plains Grasslands, USA. Plant Ecology 165 (1), 117-114.
Li, G. D.; Kemp, P. D., Hodgson, J., 1997. Biomass allocation, regrowth and root carbohydrate reserves of chicory (Cichorium intybus) in response to defoliation in glasshouse conditions. The Journal of Agricultural Science 129, 447-458.
Li, G. D.; Kemp, P. D., Hodgson, J., 1997. Herbage production and persistence of Puna chicory (Cichorium intybus L.) under grazing management over 4 years. New Zealand Journal of Agricultural Research 40 (1), 51-56.
Li, G. D.; Kemp, P. D., Hodgson, J., 1997. Regrowth, morphology and persistence of Grasslands Puna chicory (Cichorium intybus L.) in response to grazing frequency and intensity. Grass and Forage Science 52 (1), 33-41.
Li, Guangdi D.; Kemp, Peter D., Hodgson, John, 1994. Control of reproductive growth in Puna chicory by grazing management. Proceedings of the New Zealand Grassland Association 56, 213-217.
Li, Guangdi, Kemp, Peter D., 2005. Forage Chicory (Cichorium intybus L.): A Review of Its Agronomy and Animal Production. Advances in Agronomy 88 (05), 187-222.
Limami, M. A.; Dufosse, C.; RichardMolard, C.; Fouldrin, K.; Roux, L., MorotGaudry, J. F., 1996. Effect of exogenous nitrogen (15NO3) on utilization of vegetative storage proteins (VSP) during regrowth in chicory (Cichorium intybus). Journal of Plant Physiology 149 (5), 564-572.
Moloney, S., Milne, G., 1993. Establishment and management of Grasslands Puna chicory used as a specialist , high quality forage herb. Proceedings of the New Zealand Grassland Association 55, 113-118.
Monti, Andrea; Amaducci, Maria Teresa; Pritoni, Giuseppe, Venturi, Gianpietro, 2005. Growth, fructan yield, and quality of chicory (Cichorium intybus L.) as related to photosynthetic capacity, harvest time, and water regime. Journal of Experimental Botany 56 (415), 1389-1395.
Quijada, S. D. C. N., 2015. Evaluation of herb pastures for New Zealand dairy systems., 237.
Reaume, Tom, 2010. Chicory, Cichorium intybus. Nature Manitoba, 5-7.
Sanderson, M.A.; Labreveux, M.; Hall, M.H.; Elwinger, G.F., 2003. Forage yield and persistence of chicory and english plantain. Crop Science 43 (3), 995-1000.
Schittenhelm, Siegfried, 2001. Effect of sowing date on the performance of root chicory. European Journal of Agronomy 15 (3), 209-220.
Wilson, J. B., 1988. A review of evidence on the control of shoot:root ratio, in relation to models. Annals of Botany 61 (4), 433-449.
Zagal, E.; Rydberg, I., Mårtensson, A., 2001. Carbon distribution and variations in nitrogen-uptake between catch crop species in pot experiments. Soil Biology and Biochemistry 33 (4-5), 523-532.
