# PlantainForage documentation

Source URL: https://docs.apsim.info/validation/PlantainForage
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:20.706868+00:00

1 The APSIM PlantainForage Model
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
Rogerio Cichota, AgResearch, New Zealand
Preamble
Plantain forage (
Plantago lanceolata
), also know as English plantain, ribgrass, or ribwort, is a herbaceous perennial herb with a broad distribution in the native grasslands of Eurasia (
Sagar et al., 1964
;
Stewart, 1996
).  It is now common around the world in suitable habitats with temperate and subtropical climates.  The plant consists of a rosette of basal semi-erect leaves which may have one or more flowering stalks.  The leaves are green to grey-green in colour, have a characteristic lanceolate shape with 3 to 5 parallel veins along the length and have few or no hairs.  The stems are leafless, growing some 10-15cm above the leaves and have scattered hairs toward the base of the plant.  Each stalk terminates in an ovoid inflorescence containing many small flowers each with a pointed bract.  The inflrescence is 1-4cm long and changes shape over time (from a cone to a cilinder) as the flowers mature from the based to the top, the colour also changes from the initial grey-green to light brown when mature.  The flowering period goes from late spring to early fall and varies between plants, it can last several months for a population of plants in a given locale.  Forage plantain does not need vernalisation to trigger flowering.  The flowers are wind-pollinated and are replaced by a small seed capsule containing two small seeds, with weight varying between 1.5 and 2.0 mg per seed. Seeds have an oblong shape and the colour varies from dark brown to black.  The root system consists of a relatively shallow crown of coarse fibrous roots including adventitious roots (
Sanderson, 2000
) and although technically a taprooted species it does not have the typical solid deep taproot.  Plantain forage can allocate greater proportion of roots at depth, giving a competitive advantage over shallow root grasses (
Stewart, 1996
;
Nie et al., 2008
;
van den Berge, 2014
).
Selection of plantain cultivars for use as forage have focused mostly on narrow leaf varieties, which have an erect, bushy, growth habit and ability to tiller under grazing (
Stewart, 1996
), more recently some emphasis has also been put on the ability to growth over cold periods.  Forage plantain can grow large leaves, up to around 40cm instead of typical 15-25cm, and these are highly palatable to grazing animals, being specifically targeted by sheep (
Sagar et al., 1964
;
Stewart, 1996
;
Rumball, 1997
;
Quijada, 2015
). Reproductive organs are less palatable and mature stalks are avoided by grazers (
Stewart, 1996
;
Moorhead, 2009
;
Ayala et al., 2011
).  Plantain forage is highly nutritious, has good digestibility, and the mineral composition usually is higher than ryegras-clover swards (
Stewart, 1996
;
Jacobs et al., 2010
;
Elgersma, 2014
;
Lee, 2015
).  Potential medicinal effects of plantain have been suggested and few bio-active compounds that may alter rumen fermentation and N partitionin in the animal are being investigated (
Stewart, 1996
;
Quijada, 2015
;
Jansma, 2016
).
Objective:
The model presented here has been built using the Plant Modelling Framework (PMF) of
Brown et al., 2014
to simulate the growth of a forage plantin crop.  Currently, the focus is on describing biomass accumulation and regrowth after harvest on monocultural swards. As part of APSIM's PMF, using plantain forage in mixed swards is possible, but this has not been properly tested yet.  Likewise, describing the effect of population decrease on plant growth was not attempted; for swards free of weeds and within typical population ranges, the changes in plant size (number of tillers) and LAI should compensate for loss of population, but competition with weeds and others would alter this pattern significantly.
Simulating variations in plant N concentration (seasonal or due to changes in soil status)  is not a major focus of the current model.  Further development of PMF's supply/demand processes are needed for capturing the variations in N content typically seen in perennial species.
Presentation
This model has been built using the Plant Modelling Framework (PMF) of
Brown et al., 2014
to simulate the growth of a plantain forage crop (
Plantago lanceolata
).  The model focus, thus, on describing primarily the vegetative growth, with a simplified account of the reproductive phase, without explicit considering flowers and seeds (these may be included in future releases).  To simulate the aboveground plant structure, including the photosynthesis process, the PlantainForage model uses the SimpleLeaf procedure of PMF.  The model describes a semi-perennial crop, with phenology rewinding to the vegetative stage at the end of the reproductive phase.
Inclusion in APSIM simulations
A forage plantain crop can be included in a simulation the same as any other APSIM crop.
The PlantainForage object can be dragged or copied from the Crop folder in the tool box into a Field in your simulation;
To become active and grow, plantain needs to be sown using a manager script with a sowing rule. e.g.:
PlantainForage.Sow(cultivar: Tonic, population: 300, depth: 10, rowSpacing: 150);
If a specified cultivar is not available, a fatal error will be thrown.
Harvest and biomass removal
Plantain forage biomass can be removed by raising one of the valid methods: Harvest, Cut, Graze, or Prune; this is done using a manager script, similarly to other crops.  The proportion of the biomass of each organ that is removed from the system and/or added to the residue pools may be specified; otherwise defaults will be used.  Note that the sum of fractions removed and added to residue should be <= 1.0.  To specify the proportions for removal in a manager script, use a RemovalFractions class as shown below:
[EventSubscribe("Commencing")]
private void OnSimulationCommencing(object sender, EventArgs e)
{
RemoveFraction = new RemovalFractions(PlantainForage.Organs);
}
[EventSubscribe("DoManagement")]
private void OnDoManagement(object sender, EventArgs e)
{
if (Clock.Today.Date == HarvestDate)
{
RemoveFraction.SetFractionToRemove("Leaf", 0.80);
RemoveFraction.SetFractionToRemove("Stem", 0.50);
RemoveFraction.SetFractionToResidue("leaf",0.05);
PlantainForage.Harvest(RemoveFraction);
}
}
The RemovalFractions class can be sent with Harvest, Cut, Graze, or Prune events. All parameters are optional, defaults are used whenever any value is not specified.
Crop termination
To fully terminate a crop the EndCrop event should be raised:
PlantainForage.EndCrop();
Once a crop has been ended the field is open to be used by another APSIM plant model, or another plantain crop.  Note that ending plantain forage is not necessary before sowing another crop, competition for resources will take place between crops when there is more than one in the field.
Acknowledgements
This model was developed with help from Brittany Paton and Russel McAuliffe organising data and simulations. Datasets were kindly shared by Julia M. Lee and the Forages for Reduced Nitrogen Leaching (FRNL) programme.
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
PerPlantBiomassWt
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
TotalLive
Models.PMF.CompositeBiomass
1.3 Cultivars
Cultivar Name
Alternative Name(s)
Tonic
Tonic
Lancelot
Lancelot
1.4 Child Components
1.4.1 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
The duration of each phenologic phase in plantain forage is controlled either by the accumulation of thermal time or photoperiod.
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
this organ represents all the leaves in the plant, without distinction between ages or placement in the canopy.
1.4.4 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note:
this represents all the stems/stalks in the plant, without distinction between ages or placement in the canopy.
1.4.5 Inflorescence
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note:
this represents all the reproductive parts of the plant, flowers, pods, seed, etc. No distinction is made between ages or placement in the canopy.
1.4.6 Taproot
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Note:
this represents all the taproots of the plant.
Taproots are the primary reserve organs and will supply sugars and N to burst growth especially early in spring and following defoliation.  To accomplish this, the taproots' biomass is partioned into structural and non-structural fractions, the later is available to be used in new growth (via biomass retranslocation).  There is very few data on how much reserves are kept in plantain taproots, results from
Quijada, 2015
suggest that sugars plus fructan made up 4-11% of taproots biomass compared to 15-35% in chicory, which is consistent with plantain forage having much smaller taproots.  Based on these values, and some trial and error, the parameters related to retranslocation (StructuralFraction, DMRetranslocationFactor, and NRetranslocationFactor) were estimated.  These should be upgraded when more data is made available.
1.4.7 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
Note:
this represents all the fine roots of the plant.  The root organ demands and is partitioned N and DM and its depth increases through time to provide a water uptake supply
1.4.8 PerPlantBiomassWt
A class that divides all child functions.
Returns zero if nominator is zero, returns double.maxValue if denominator is zero.
This represents the average live biomass dry weight per plant (in g/plant).
1.4.9 ShootRootRatio
Look up a value based upon the current growth phase.
1.4.10 TargetShootRootRatio
Look up a value based upon the current growth phase.
To ensure prompt regrowth of forage plantain after a defoliation, the model will adjust the allocation of new growth following any defoliation event. This is a simplified approach to biomass allocation plasticity (e.g.
Wilson, 1988
;
Levang-Brilz et al., 2002
), it assumes that the plant switches allocation of towards leaves whenever the current shoot:root ratio differs from the target value.
Published data for the shoot:root ratio in plantain is highly variable, with values as low as 1.0 for wild varieties and as high as 6.0 for seedlings of commercial cultivars with no water of nutrient limitations (
Schippers et al., 2000
;
Labreveux, 2002
;
Cranston, 2015
;
Pankoke, 2015
). The data suggests mean values to be around 1.75-2.0 for adult plants. This value is likely to be affected by environmental conditions, with water or nutrient deficit favouring root growth, while low light conditions leading to higher allocation above ground. However, these relationships can vary considerably in different plants and there is little data for plantain forage, so the model current ignores the influence of environmental factors on the shoot:root partition, this should be upgraded when data becomes available.
1.4.11 StemsLeafRatio
Look up a value based upon the current growth phase.
1.4.12 TargetStemsLeafRatio
Look up a value based upon the current growth phase.
The PlantainForage model defines allocation of biomass above ground following a simple approach, it attempts to keep a given proportion among the various organs. The allocation of biomass to leaves is defined primarily by the target shoot:root ratio, but during the reproductive phase, the partition is further modified based on the ratio of stems+inflorescence to leaves. The model will use this ratio to allocate some biomass to the stems and inflorescence during the reproductive phase (i.e. when the target ratio is reater than zero). As plantain is a 'long-days' plant, the model assumes that the ratio increase with the length of the day (photoperiod). This ratio may be affected by defoliation (
Ayala et al., 2011
;
Quijada, 2015
) and is probably affected by enviromental factors too. There is, however, not enough data to define a good description for these interactions.
Published results suggest that reproductive parts represent a relatively small proportion of the biomass, about 10-25% in grazed swards, and about 40% if non-grazed ones (
Schippers et al., 2000
;
Labreveux, 2002
;
Moorhead, 2009
;
Ayala et al., 2011
).
1.4.13 FlowerStemRatio
Look up a value based upon the current growth phase.
1.4.14 TargetFlowerStemRatio
Look up a value based upon the current growth phase.
Following on the simple allocation of biomass in the PlantainForage model, the allocation during the reproductive phase will attempt to keep a ratio between inflorescence and stem biomass. The model assumes that this ratio increases from spring, when only stems are set, to a maximum in autumn, when the last seeds finish ripenning. This ratio is probably affected by enviromental factors, and perhaps defoliations, but there is not data to determine how these interactions could be modelled. Because the model attempts to simulate a whole sward, with potentially plants of different ages, and due to the short period between stem elongation, flowering, and seed maturity, and a relatively long reproductive phase, forage plantain plants will have ripe seeds as well as young stalks at the same time. This makes it quite difficult to define the variations in biomass allocation to stems and inflorescence. Inference from published values suggest that the ratio of flower biomass to stems varies from around 0.1 to nearly 0.4 (
Schippers et al., 2000
).
1.4.15 TaprootRootRatio
Look up a value based upon the current growth phase.
1.4.16 TargetTaprootRootRatio
Look up a value based upon the current growth phase.
The PlantainForage model assumes that biomass is allocated so to mantain a given proportion of the biomass among the various organs (varying with phenological phase and other factors). The ratio between taproot and root biomass is assumed to vary proportional to the plant biomass (healthy well develop plants can add more reserves to the taproots). The ratio increases with the total biomass per plant, approaching a maximum target when plants are adult (this also implies that less biomass goes to taproots following defoliation). Furthermore, a seasonal variation is also considered, with greater proportion of biomass being allocated to taproots in late summer/autumn and less in spring. This is a simple approach and can describe the general behaviour of plantain forage plants, it may be upgraded if deemed necessary when more data becomes available.
1.4.17 MortalityRate
A constant function (name=value)
1.4.18 SeedMortalityRate
A constant function (name=value)
2 Validation
The performance of the plantain forage model is evaluated in simulations based on experiments conducted primarily in New Zealand.  Datasets from USA have been added, but they should be taken with caution as there was limited information about the experiments and published results corresponded to annual or seasonal aggregates only.  Note that population is not explicitly simulated by the model, only a decrease in plant numbers as function of cuts or grazings is considered.  The literature suggest that severe defoliation may reduce population by increasing death rate, but mostly is infered from other species and even then the variation is high.  Plantain persistency has been shown to be low, usually restricted to 2-4 years, but very little actuall data is available.
The model describes N content in the above ground tissues reasonably well only for treatments where N deficit was not high (See results for the FRNL datasets with annual fertiliser rates of 200 kgN/ha and above).  The measured data showed little effect on plant N concentration for the treatment  with nil or low N rates, wehreas the model predicts differences between them.  The variability among replicates is high and there wer only three or four replicates, this may blur some of the differences.   Also, there was no information from which potential sources of nitrogen, such as soil mineralisation or N fixation from 'weeds' (the present of white clover was noted), could be identified and used to adjust the simulations.  Besides these isses, some of the underlying PMF code may need to be upgraded in order to describe some processes under plant cut/grazing and re-growth conditions.  Processes for computing N demand and supply (especially regarding remobilisation within living tissues) are still under developement and this needs especific detailed datasets.  The SimpleLeaf module was employed to simulate the plant canopy in the plantain forage model, this was done because there was not enough data to parameterise the more complex PMF approach.  This means that the model has only one leaf cohort with simple N thresholds (only maximum and minimum), this may be too simplistic to capture the nuances of the re-growth processes especially when sustained by the remobilisation from storage organs (taproot and old tissue).  The model will be upgraded in the near future as the work on the code to improve the description of those specific processes is ongoing.
2.1 New Zealand
All data from New Zealand
Experiment Name
Design (Number of Treatments)
FRNLLincoln
NRate (6)
FRNLRuakura
NRate (6)
ScottFarmFD902
Defoliation (8)
LincolnRDF
NRate (2)
MasseyRDF1
GrazeFrequency (2)
####FRNL Lincoln
Simulation setup based on field trial performed at Lincoln University Research Dairy Farm, New Zealand, between 2014 and 2016.  The experiment was part of the FRNL (Forages for Reduced Nitrogen Leaching) program and the data is basically unpublished (references will be added when this come about).  Here only plots with pure plantain are used.
The experiment consisted of six fertiliser treatments (N rates of 0, 50, 100, 200, 350 and 500 kg/ha/yr) in four replicates, and measurements comprised yield and quality indicators (here N content is used).
Data supplied by Grant Edwards, Lincoln University.
The soil at the experimental site was a Templeton fine sandy loam (an Immature Pallic soil, USDA: Udic Haplustept), the required parameters were inferred based on data from the New Zealand National Soils Database (Landcare Research).
Weather data was obtained from nearby Broadfields weather station (NIWA).
####FRNL Ruakura
This simulation setup was based on field trials performed at DairyNZ's Scott Farm, in Ruakura, New Zealand, between 2014 and 2016.  The experiment was part of the FRNL (Forages for Reduced Nitrogen Leaching) program and the data is basically unpublished (references will be added when this come about).  Here only plots with pure plantain are used.
The experiment consisted of six fertiliser treatments (N rates of 0, 50, 100, 200, 350 and 500 kg/ha/yr) in three replicates, and measurements comprised yield and quality indicators (here N content is used).
Observed data supplied by Grant Edwards, Lincoln University.
The soil at the experimental site was a Horotiu silt loam (an Orthic Allophanic soil; USDA: Typic Udivitrand), the parameters required were inferred using data from the New Zealand National Soils Database (Landcare Research).
Weather data was obtained from nearby Ruakura weather station (NIWA).
This simulation was setup based on a defoliation trial at Scott farm, Hamilton, New Zealand, run from 2010 to 2012 by DairyNZ.  The study investigated the effect of cut height (which define rotation length) and residual height on chicory and plantain growth.  The treatments consisted of trigerring cuts when the sward reached target heights of 150, 250, 350, and 450 mm, in factorial combination with two residual heights, 30-50 or 60-80 mm.  Harvest was avoided over winter.  Note that not always the target height was reached.
Trial plots were sown with plantain, cultivar '
Ceres Tonic
' (at 10 kg/ha), after previous pasture was sprayed with herbicide and conventionally drilled.  Irrigation was applied in the first year and fertiliser was applied over summer.  Results reported include biomass yield, botanical composition (presence of weeds), plant density, leaf area, and light interception (the later two only during one growing period - between two cuts).
References:
Lee, J.M.;  Hemmingson, N.R.; Minnee, E.M.K.; Clark, C.E.F. 2015. Management strategies for chicory (Cichorium intybus) and plantain (Plantago lanceolata): impact on dry matter yield, nutritive characteristics and plant density. Crop and Pasture Science, 66, 168–183.
####Lincoln university research farm
Simulation setup based on field trial performed at Lincoln University Research Dairy Farm, New Zealand, between 2010 and 2012. The experiment aimed to compare the nutritive value and DM production of a range of pasture species throughout the growing season under a high and low N fertiliser regime. Here only plots with pure plantain are used.
The experiment had two nitrogen treatments, nitrogen was applied to half of each plot after grazing, ensuring similar distribution over what were previously ProGibb, N and control plots. The first N rate was called low (125 kg N/ha/yr) – to encourage legume growth and to mimic rates used by Frisco; the second N rate was called high (at 250 kg N/ha/yr); for both treatments, fertiliser was applied into three events. Yield cuts were taken every 4 weeks by mowing to 4.5cm. After mowing for yield, animals were allowed to graze the areas down to 4.5cm, followed by further mowing if necessary to tidy up the plots. When grazing dates coincided with the allotted N schedule, urea was then applied later by hand within seven days of grazing.
The soil at the experimental site was a Templeton fine sandy loam (an Immature Pallic soil, USDA: Udic Haplustept), the required parameters were inferred based on data from the New Zealand National Soils Database (Landcare Research).
Weather data was obtained from nearby Broadfields weather station (NIWA).
####Massey university research farm
Simulation setup based on field trial performed at No1 Dairy Unit at Massey University in Palmerston North. A grazing experiment was conducted during 2011-2012 (first growing season) and 2012-2013 (second growing season), New Zealand (40° 22′S, 175° 36′E). Three pasture treatments were evaluated: (i) chicory, (ii) plantain, and (iii) an herb-clover mix pasture, containing chicory, plantain, red clover and white clover, under two grazing frequencies; every two or four weeks. Perennial ryegrass/white clover pasture was also established and grazed to provide a benchmark.
The soil at the experimental site was a Manawatu silt loam over sand. Soils tests to 75 mm depth were conducted
at the experimental site and showed levels of soil fertility of Olsen phosphate of 31 ppm, sulphate of 13 ppm, potassium of 0.42 me/100g, and pH 5.7. Before sowing, the paddock was ploughed, power harrowed, fertilised with 27, 18, 18 kg/ha nitrogen (N), phosphorus (P) and potassium (K), respectively.
Weather data was obtained from nearby weather station (NIWA).
2.2 USA
All data from USA
Experiment Name
Design (Number of Treatments)
RELARC_01
Cultivar x CutFrequency (4)
RELARC_02
Cultivar (2)
####RELARC 01
This simulation was adapted from a defoliation trial conducted in 1998-1999, at Russell E. Larson Agricultural Research Center, Pennsylvania, USA.  The trial consisted of small plots where two plantain forage cultivars ('
Lancelot
' and '
Tonic
') were sown separately in May-1997 (at 11.0 kg seed/ha).  Population decreased markedly after the 1998/99 winter and plantain plots were no longer used. Plots were fertilized with 27 kgP and 72 kgK/ha in October 1997. Fertilizer N was applied at 56 kg/ha in June and July of 1998.  Treatments were replicated five times and the soil at the site was a Hagerstown silt loam (fine, mixed, semiactive mesic Typic Hapludalfs).
Each plot was divided in two, one-half was harvested every 3 wk and the other half every 5 wk. Each strip was cut to a 7-cm height with a rotary mower.  Dates for each cut were given, but only annual and treatment yields are reported;  population numbers were provided for few specific dates.  Yield for each harvest were arbitrarily inferred, therefore they should be seen as rough indication only.
Met file was built using data obtained from NOAA website.  The data set was largely complete except for solar radiation, which was added from an alternative interpolated set for this location, also from NOAA.
Information to assemble the soil data was obtained from USGS.
References:
Sanderson, M.A.; Labreveux, M.; Hall, M.H.; Elwinger, G.F. 2003. Forage yield and persistence of chicory and english plantain. Crop Science, 43: 995-1000.
####RELARC 02
This simulation was adapted from a defoliation trial conducted in 2000-2001, at Russell E. Larson Agricultural Research Center, Pennsylvania, USA.  The trial was established in small plots, replicated five times, with plantain forage sown in April-1999 (Cultivars '
Lancelot
' and '
Tonic
'.  Only data from the first year is used as the population declined markedly in the second winter.  All plots received 4.5 t/ha of limestone and were fertilized with 56 kg N/ha in April, June and July of 2000.  The soil at the site was a Hagerstown silt loam (fine, mixed, semiactive mesic Typic Hapludalfs).
Each plot was mechanically mowed to height of 7cm every 4 weeks.  Only annual and treatment yields are reported, however population was given for specific dates.  Dates for each harvest were reported.  From these data, yield was arbitrarily inferred for each harvest, therefore they should be seen as rough approximation only (N variation between cultivars were considered here).
Met file was built using data obtained from NOAA website.  The data set was largely complete except for solar radiation, which was added from an alternative interpolated set for this location, also from NOAA.
Information to assemble the soil data was obtained from USGS.
References:
Sanderson, M.A.; Labreveux, M.; Hall, M.H.; Elwinger, G.F. 2003. Forage yield and persistence of chicory and english plantain. Crop Science, 43: 995-1000.
3 Interface
3.1 PlantainForage
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
Ayala, W.; Barrios, E.; Bermudez, R., Serron, N., 2011. Effect of defoliation strategies on the productivity, population and morphology of plantain (Plantago lanceolata). Pasture persistence symposium. Grassland Research and Practice Series 15, 69-72.
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Cranston, L. M.; Kenyon, P. R.; Morris, S. T.; Lopez-Villalobos, N.; & Kemp, P. D., 2015. Morphological and physiological responses of plantain (Plantago lanceolata) and chicory (Cichorium intybus) to water stress and defoliation frequency. Journal of Agronomy and Crop Science, 1-12.
Elgersma, A; Soegaard, K; Jensen,S.K., 2014. Herbage dry-matter production and forage quality of three legumes and four non-leguminous forbs grown in single-species stands. Grass and Forage Science 69 (4), 705-716.
Jacobs, J. L., G. N. Ward, 2010. Effect of nitrogen application on dry matter yields, nutritive characteristics and mineral content of summer-active forage crops in southern Australia. Animal Production Science 51 (1), 77-86.
Jansma, A. P., 2016. Performance of two plantain cultivars during the first months after establishment., 13.
Labreveux, M. E., 2002. Productivity of forage cultivars of chicory and plantain in the Northeast Region of the United States., 112.
Lee, J. M.; Hemmingson, N. R.; Minnee, E. M. K.; & Clark, C. E. F., 2015. Management strategies for chicory (Cichorium intybus) and plantain (Plantago lanceolata): impact on dry matter yield, nutritive characteristics and plant density. Crop and Pasture Science 66 (2), 168-183.
Levang-Brilz, N., M. E. Biondini, 2002. Growth rate, root development and nutrient uptake of 55 plant species from the Great Plains Grasslands, USA. Plant Ecology 165 (1), 117-114.
Moorhead, A.; & Piggot, G., 2009. The performance of pasture mixes containing ‘Ceres Tonic’plantain (Plantago lanceolata) in Northland. Proceedings of the New Zealand Grassland Association 71, 195-199.
Nie, Z. N.; Miller, S.; Moore, G. A.; Hackney, B. F.; Boschma, S. P.; Reed, K. F. M.; Mitchell, M.; Albertsen, T. O.; Clark, S.; Craig, A. D.; Kearney, G.; Li, G. D., Dear, B. S., 2008. Field evaluation of perennial grasses and herbs in southern Australia. 2. Persistence, root characteristics and summer activity. Australian Journal of Experimental Agriculture 48 (4), 424-35.
Pankoke, H.; Höpfner, I.; Matuszak, A.; Beyschlag, W.; & Müller, C., 2015. The effects of mineral nitrogen limitation, competition, arbuscular mycorrhiza, and their respective interactions, on morphological and chemical plant traits of Plantago lanceolata. Phytochemistry 118, 149-161.
Quijada, S. D. C. N., 2015. Evaluation of herb pastures for New Zealand dairy systems., 237.
Rumball, W.; Keogh, R.G., 1997. Grasslands Lancelot (Plantago lanceolata L.). New Zealand Journal of Agricultural Research 40 (3), 373-377.
Sagar, G. R., Harper, J. L., 1964. Plantago Major L.; P. Media L. and P. Lanceolata L.. Journal of Ecology 52 (1), 189-221.
Sanderson, M.A.;Elwinger, G.F., 2000. Seedling development of chicory and plantain. Agronomy Journal 92 (1), 69-74.
Schippers, P., Olff, H., 2000. Biomass partitioning, architecture and turnover of six herbaceous species from habitats with different nutrient supply. Plant Ecology 149 (2), 219-231.
Stewart, A., 1996. Plantain (Plantago lanceolata)-a potential pasture species. Proceedings of the New Zealand Grasslands Association 58, 77-86.
van den Berge, J.; K. Naudts, de Boeck, H. J.; Ceulemans, R.; Nijs, I., 2014.  Do interactions with neighbours modify the above-ground productivity response to drought? A test with two grassland species. Environmental and Experimental Botany 105, 18-24.
Wilson, J. B., 1988. A review of evidence on the control of shoot:root ratio, in relation to models. Annals of Botany 61 (4), 433-449.
