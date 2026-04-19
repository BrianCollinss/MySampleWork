# Pinus documentation

Source URL: https://docs.apsim.info/validation/Pinus
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:16.042966+00:00

1 The APSIM Pinus Model
The APSIM Pinus Model
Pinus Model Notes
Plant Modelling Framework
The APSIM Pinus model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
within  APSIM Next Generation
Holzworth et al., 2014
. This new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a model in much the same way that models can be coupled to construct a simulation. This means that dynamic composition of lower level processes and organ classes (e.g. photosynthesis, Leaf) into larger constructions (e.g. maize, wheat, Pinus) can be achieved by the model developer without additional coding.
###Pecularities of Pinus and This Model
The Pinus model consists of:
a phenology model to simulate development through sequential growth phases
a collection of organs to simulate the various plant parts
an arbitrator to allocate resources (N, biomass) to the various plant organs
This work builds upon earlier APSIM forest models such as described by
Huth et al., 2002
,
Huth et al., 2001
,
Huth et al., 2008
, and
Smethurst et al., 2020
.
Pinus is a reasonably straight forward perennial crop to model. This model has been set up for simulation of even-aged plantations (transplanted seedlings) or native forests (also assumed to start the simulation as a transplanted seedling, but in practice it would be sown from naturally distributed seed). Plants grow in accordance with available resources and conditions, which in this version of the model are temperature, radiation, available soil water, and available soil nitrogen. Leaf, branch and roots senesce, remain attached for some time, then detach to produce litter. Above-ground biomass is the main target of production, which is made up of organs that develop from default partitioniing targets that is modified daily due to organ demand. Forest managers also deal with tree size, which are set in the model as a function of aboveground biomass. During model development, we found that Pinus model performance (plant or stand development) was particularly sensitive to Leaf lifespan/longevity, specific Leaf area, dead Leaf detachment, partitioning to roots and shoots, mortality and thinning, and weeds (if present).
There are many Pinus genotypes (species, closely related genera, provenances, families, clones, and hybrids) that can behave differently in response to their growing environment. This build of the Pinus model was calbirated on datasets of three species (
P. radiata
,
P. caribaea
,
P. elliottii
) and F1 and F2 hybrids of (
P. caribaea
,
P. elliottii
).  Many of the genotype are poorly described, but some are probably from early-stage domestication, whiole others are the result of several generations of tree breeding. Early genetic material was mostly selected, for example, for states in Australian from provenance that performed adequately in state of interest, and tree breeding programes for several decades were state-run. During more recent decades, company and collaborative breeding programs have provided the genetic material for planting. A summary of domestication and genetic material of planted
P. radiata
in Australia and New Zealand is provided in
Burdon et al., 2008
,
Wu et al., 2007
, and
Johnson et al., 2008
. The implications are that genotypic control of physiological traits is poorly defined, requiring the model to be calibrated for assumed rather than measured differences.
###Including a Pinus crop in an APSIM simulation
There will be a Pinus and/or PinusRotation example simulation available by clicking the "Open an Example" icon available on the second tab displayed when APSIM Next Gen is opened.  This provides a useful demonstration of how to simulate a Pinus crop and provides some useful graphs for viewing model behaviour and performance.
To include a Pinus crop in a simulation the "Pinus" model needs to be added to the paddock, field or zone in which it is to be grown.  This can be done by (a) right clicking on the "Paddock", selecting "Add" from the drop down menu and then selecting "Pinus" from the list that comes up, or (b) dragging or copying and pasting the model up from the standard toolbox or an example.  A TreeSowing rule or TreeManagement rule needs to be set up to start the crop off.
This document provides a description of the model, describes the validation and test datasets, and model performance.
Major Eucalyptus model developments:
2020-2021
• Copied the Eucalyptus model and modified it to fit the observed Pinus datasets. The Eucalyptus and Pinus models were expanded (beyond the original Eucaluptus model in APSIM Next Generation) to include an expansion of stem metrics beyond just diameter at breast height (DBH, cm), height (m), and overbark stem volume (Vol, m3/ha). New stem metrics include underbark parameters of stem volume (Volub) and wood density, and basal area (BA, m2/ha) the mean annual increment of overbark and underbark volumes (m3/ha/year).This required partitioning of stem biomass into bark and wood, calculation of bark thickness, and an estimation of volumes overbark and underbark. All stem metrics are empricially calculated rather than process-based. These metrics are known to be	highly effected by stem taper, bark thickness and wood density, which in-turn are highly influenced by site, genetics and management. Few data are available on wood density (underbark, whole tree), so it is included here only as a check that underlying calculations are sensible. Validations of these metrics are included.
Suggested future developments:
Make SLA a function of stress level
Create a set of functional weeds specifically for use with these forestry models, e.g. N-fixing/non-N-fixing X herbaceous/shrub/tree X tropical/sub-tropical/temperate
Self-thinning rule or process-based mortality
Improve effects of stocking, if necessary. Leaf allocation as a function of aboveground.wt (g/m2) rather than individual tree weight (g/tree) has been included, but further checking of this is required to see if that is all that is needed for a wide range of stockings. Allocation patterns might need to be explicitly related to population.
Waterlogging – I (Philip) would have thought it was important, particularly for some euc and pine genotypes, but so far I haven’t run into a really need for it amongst our current observed datasets.
Soil P (and K) and fertiliser responses
Pruning, thinning and various otehr effects on knot-free wood and other wood quality measures
Geo-locate and interact adjacent plots for predicting area-based metrics like run-on (above- and below-ground), stream flow and wood production
Tree and log size class distributions
Development of outputs for greenhouse accounting (water use, C sequestration, greenhouse gases, biodiversity idiexes)
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
Although Pinus trees have needles as their foliage, they are the Leaf component of the model to conform to PMF needs. Although the reproductive component of the model (called Cone here) is of minor interest for wood production, it can make up several percent of tree wieght and for which we found some data to guide calibration. Phenology is simple in this model and could become a focus for future model develompent, e.g. for tree breeding or pollen production. Allometric relationships for height (Ht, m), stem diameter (DBH, cm, over bark at 1.3 m height), and their derivatives (stem volume Vol m3/ha, and mean annual increment MAI m3/ha/year) were developed as a function of above-ground biomass based on observations. Root:shoot ratio is an emergent property of the model based on separate calibrations for root and shoot mass. Wood density remains pooly calibrated as few observations of whole tree wood density are available and based on stem analysis. So here wood density is an emergent proporty of the model, and observed versus predicted graphs of it  are included only as a general check that both wood volume and wood weight are correctly simulated.
The model is constructed from the following list of software components. Details of the implementation and model parameterisation are provided in the following sections.
1.1 Plant Model Components
Component Name
Component Type
Age
Models.Functions.AccumulateFunction
MortalityRate
Models.Functions.Constant
SeedMortalityRate
Models.Functions.Constant
Phenology
Models.PMF.Phen.Phenology
Arbitrator
Models.PMF.OrganArbitrator
IndividualTreeLiveWt
Models.Functions.DivideFunction
IndividualTreeStemWt
Models.Functions.DivideFunction
Cone
Models.PMF.Organs.GenericOrgan
Leaf
Models.PMF.Organs.PerennialLeaf
Branch
Models.PMF.Organs.GenericOrgan
Stem
Models.PMF.Organs.GenericOrgan
CoarseRoot
Models.PMF.Organs.GenericOrgan
FineRoot
Models.PMF.Organs.Root
RootShootRatio
Models.Functions.DivideFunction
1.2 Composite Biomass
Component Name
Component Type
AboveGround
Models.PMF.CompositeBiomass
BelowGround
Models.PMF.CompositeBiomass
TotalWoodBark
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
caribaea
caribaea
elliottii
elliottii
PEE
PEE
elliottiiIMPAC
elliottiiIMPAC
elliottiiQLD
elliottiiQLD
elliottiiXcaribaeaF1
elliottiiXcaribaeaF1
elliottiiXcaribaeaF2
elliottiiXcaribaeaF2
BFG
BFG
GF7
GF7
TowerHill
TowerHill
taeda
taeda
taedaIMPAC
taedaIMPAC
1.4 Child Components
1.4.1 Age
Accumulates a child function between a start and end stage.
1.4.2 MortalityRate
A constant function (name=value)
Mortality, and therefore its efect on population density (stocking), is set by the user of the model as a thinning operation.
1.4.3 SeedMortalityRate
A constant function (name=value)
1.4.4 Phenology
The phenological development is simulated as the progression through a series of developmental phases, each bound by distinct growth stage.
Phenology is currently very simplistic in the Pinus model.
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
1.4.7 IndividualTreeStemWt
A class that divides all child functions.
Returns zero if nominator is zero, returns double.maxValue if denominator is zero.
1.4.8 Cone
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.9 Leaf
This organ is parameterised using a simple leaf organ type which provides the core functions of intercepting radiation, providing a photosynthesis supply and a transpiration demand.  It also calculates the growth, senescence and detachment of leaves.
1.4.10 Branch
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.11 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
Stem metrics useful for plantation forestry are derive here from stem weight using an allometric relationship with stem diameter at breast height (DBH over bark). Basal area (BA, sum of areas of stems in m2 per ha) and stem height (Ht) depends on DBH, and volume (Vol over bark) is provide (g per ha) as a function of both Ht and BA. However, underbark metrics were also required, which necessitated the division of stem weight and volume into wood and bark components using, the volume component, which depends on bark thickness. All these functions are empirical and calibrated to provide adequate model skill. Finally, wood and bark densities are calculated. Tree stems are not exactly conical, and bark thickness and wood and bark densities vary radially and with height up the stem, with age, and between trees due to inter-tree competition and growth rate. All parameters, and even functions, can be redefined within genotype (cultivar) settings.
1.4.12 CoarseRoot
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.13 FineRoot
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
1.4.14 RootShootRatio
A class that divides all child functions.
Returns zero if nominator is zero, returns double.maxValue if denominator is zero.
2 Validation
Validation datasets have been included to assist with validation during model development. Validation datasets cover a range of environmental (soil and climate) conditions, management options (populations, nitrogen rates, irrigation) and genetic backgrounds (different regions, provenence, clones). These datasets have been grouped and ordered alphebetically by climatic zone, country and site. Currently, all temperate data are for
P. radiata
, but a range of tropical and sub-tropical genpotypes are included. Graphs of model performance are provided for stocking, canopy development, biomass production, stem metrics, and soil water.
Observed data are shown compared to predictions, with statistics for model skill.
The following map shows the location of sites used in this validation.
2.1 Map
2.2 Combined Validation
These graphs are for the combined datasets of
Temperate
and
Tropical and SubTropical
climatic zones.
Graphs from individual sites, particularly temporal trends of observed and predicted values, are available but
currently disabled. If you wish to view these graphs, please download the validation from GitHub, run it, and
enable those graphs and-or add others.
Two sites are simulated to ages beyond when there was a reasonable expectation of satisfactory model skill, due to very low stocking being reached where allocation patterns would probably not have been simulated correctly (Puruki site) or due to not correctly predicting disease-induced loss of tree vigor (IMPAC site). However, all data have bene included  in these OvP graphs.
Many of these simualtions for stem metrics could be further improved by better specifying allometric relations between biomass and tree size, which we know are highly environment-by-genotype specific, but what is provided here is a minimal range of genotypes that were needed to achieve the level of model skill currenlty shown.
2.3 Temperate
These are the Temperate datasets.
2.3.1 BFG
These data are for an experiment with fertilizer, irrigation and fertigation treatments established in a 10-year-old
P. radiata
plantation near Canberra, Australia. The experiment was officially entitled Biology of Forest Growth (BFG) as if focused on improving the physiogical understanding of  growth of this species in a somewhat common Australian environment.
Numerous publications about this experiment reported on its management, tree growth, biomass components, phenology, climate, and soils. Many of these publications were produced in a special issue of Forest Ecology and Management (Volume 52, Issues 1–4, September 1992) several from which data have been sourced for developing the APSIM Pimus model:
Benson et al., 1992
,
Benson et al., 1992
,
Crane et al., 1992
,
Cremer, 1992
,
Khanna et al., 1992
,
Kirschbaum, 1999
,
McMurtrie et al., 1992
,
Myers et al., 1992
,
Raison et al., 1992
,
Raison et al., 1992
,
Raison et al., 1992
,
Raison et al., 1990
, and
Snowdon et al., 1992
. A later thesis
Pongracic, 2001
and paper
Waterworth et al., 2007
also provided data. Many of these data and addition data were also provided by Keryn Paul (CSIRO), which we greatly apprecaited.
Experiment Name
Design (Number of Treatments)
BF
G (4)
2.3.2 FSA703A
These data are a plot in operational plantations that were previously managed by Forestry South Australia (FSA). We appreciate the provision of data by Jim O'Hehir. This plot reached an old age relative to many other
P. radiata
plantations in the world.
2.3.3 FSA704
These data are a plot in operational plantations that were previously managed by Forestry South Australia (FSA). We appreciate the provision of data by Jim O'Hehir. This plot reached an old age relative to many other
P. radiata
plantations in the world.
2.3.4 Puruki
These data are from one plot at a highly productive site in New Zealand. Studies at the site included physiology and growth, nutrition, soils and hydrology, which were published. This site was of particular interest for this validation because it had high nutrient availability and high rainfall, and it was well-published. Publications drawn on were
Rijkse et al., 1974
,
Beets et al., 1987
,
Beets et al., 1987
,
Beets et al., 1987
,
Beets et al., 1987
,
Dyck et al., 1987
,
Beets et al., 1996
,
Parfitt et al., 2006
,
Kimberley et al., 2007
,
Oliver et al., 2004
,
Beets et al., 2007
,
Beets et al., 2011
,
Beets et al., 2018
, Beets et al (2018 Puruki field trip - a productive ex-pasture site DOI:10.13140/RG.2.2.16726.50243),
Garrett et al., 2019
, and
Beets et al., 2020
.
Experiment Name
Design (Number of Treatments)
Puruk
i (2)
2.3.5 TowerHill
These data are from an experiment with ferilizer on very shallow soils with moderate rainfall. Substantial rates of accumulated N applications changed the C-N dynamics of the site and stand growth. Data were reported in
Neilsen et al., 1992
and
Neilsen et al., 1998
, and some details were checked in the original files with permission of the current manager of the site (Sustainable Timber Tasmania).
Experiment Name
Design (Number of Treatments)
TowerHil
l (2)
2.3.6 GraphsOvP
Test! These observed versus predicted graphs include all validation datasets and timesteps within a site for which observed data were available.
2.4 Tropical and SubTropical
These are the Tropical and SubTropical datasets.
2.4.1 Australia
These are the Australian sites of the Tropical and SubTropical datasets.
Data come from sites of Forestry Plantations Queensland (FPQ;
Dieters et al., 2007
) and Hancocks Queensland Plantations (HQP).
2.4.1.1 FPQBeerwah
These data are from a genetics experiment in south-east Queensland described by
Dieters et al., 2007
. Four genotypes are compared:
P. elliottii
var. elliottii (PEE),
P. caribaea
var. hondurensis (PCH), the F1 hybrid, and the F2 hybrid. As only stem metrics were provided in the paper, genotypes were not calibrated for biomass components and other physiological attributes.
Experiment Name
Design (Number of Treatments)
FPQBeerwah
_ (4)
2.4.1.2 FPQToolara
These data are from a genetics experiment in south-east Queensland described by
Dieters et al., 2007
. Four genotypes are compared:
P. elliottii
var. elliottii (PEE),
P. caribaea
var. hondurensis (PCH), the F1 hybrid, and the F2 hybrid. As only stem metrics were provided in the paper, genotypes were not calibrated for biomass components and other physiological attributes.
Experiment Name
Design (Number of Treatments)
FPQToolara
_ (4)
2.4.1.3 FPQTuan
These data are from a genetics experiment in south-east Queensland described by
Dieters et al., 2007
. Four genotypes are compared:
P. elliottii
var. elliottii (PEE),
P. caribaea
var. hondurensis (PCH), the F1 hybrid, and the F2 hybrid. As only stem metrics were provided in the paper, genotypes were not calibrated for biomass components and other physiological attributes.
Experiment Name
Design (Number of Treatments)
FPQTuan
_ (4)
2.4.1.4 HQP248
These data were kindly provided by Hancock Queensland Plantations (HQPlantations, HQP) from Experiment 248 MBR, which was established to determine the most appropriate taxa (including stock production systems) and site preparation prescription(s) for use on wet sites in south-east Queensland. As the model was not designed to simulate various details of each treatment, only one of the best performing treatment was simulated (Experiment 248MBR, Treatment F1-Container-LargeMound).
2.4.1.5 HQP251
These data were kindly provided by Hancock Queensland Plantations (HQPlantations) from Experiment 251 MBR, which was established to investigate the impact of various 2R site preparation methods at a high-mounded first rotation (1R) site with hard-setting soil in south-east Queensland. As the model was not designed to simulate various details of each treatment, only one of the best performing treatments was simulated (Experiment 251MBR, Treatment F1-MB6).
2.4.1.6 HQP321R1
These data were kindly provided by Hancock Queensland Plantations (HQPlantations) from Experiment 321GYM, which was established to:(1) examine the impacts of slash management, fertilization and cover crops on tree growth and nutrient status; (2) quantify the effects on soil properties; and (3) contribute to the CIFOR international network of long term experiments designed to identify and develop sustainable inter-rotation management practices. Early results were reported in
Simpson et al., 2000
. Data here are reported only for the first rotation (1R) to age 29.4 years.
2.4.1.7 HQP321R2
These data were kindly provided by Hancock Queensland Plantations (HQPlantations) from Experiment 321GYM, which was established to:(1) examine the impacts of slash management, fertilization and cover crops on tree growth and nutrient status; (2) quantify the effects on soil properties; and (3) contribute to the CIFOR international network of long term experiments designed to identify and develop sustainable inter-rotation management practices. Early results were reported in
Simpson et al., 2000
. Data here are reported only for one of the best performing treatments of the second rotation (2R) to age 8 years (Experiment 321GYM, Treatement BL2-TWC).
2.4.1.8 HQP350
These data were kindly provided by Hancock Queensland Plantations (HQPlantations) from Experiment 350GYM, which was established to assess the relative benefits, both short-term and long-term, of major factors and their interactions on PEE x PCH hybrid plantations when grown on an average site in the Maryborough district. Data here are reported only for one of the best performing treatments (Experiment 350GYM, Treatment High-FWC).
2.4.1.9 GraphsOvP
Test! These observed versus predicted graphs include all validation datasets and timesteps within a site for which observed data were available.
2.4.2 USA
These are the USA sites of the Tropical and SubTropical datasets.
2.4.2.1 IMPAC
These data are from an experiment with ferilizer and herbicide treatments and both
P. taeda
and P.elliottii_ near Gainesville, north-central Florida, USA. As nutrients other than N were probably limiting in some treatments, which the the model couold not be expected to simulate, only the high input treatment was simualted.
Numerous publications report on this experiment:
Jokela et al., 2000
,
Vogel et al., 2011
,
Martin et al., 2004
,
Gonzalez-Benecke et al., 2012
,
Jokela et al., 2010
,
Subedi et al., 2019
,
Jokela et al., 2004
, and
Gonzalez-Benecke et al., 2016
. A consolidated dataset was kindly provided by Eric Jokela and Jason Vogel, University of Florida.
Experiment Name
Design (Number of Treatments)
IMPAC
L x S (2)
2.4.2.2 SETRES
These data are from an experiment of 2x nutrient and water treatments in
P. taeda
near Sandhills, North Carolina, USA. As nutrients other than N were probably limiting in some treatments, which the the model couold not be expected to simulate, only the high input treatment was simualted.
Numerous publications report on this experiment, of which we drew on
Zhao et al., 2016
,
Landsberg et al., 2001
,
Albaugh et al., 2009
,
Albaugh et al., 2004
, and
Albaugh et al., 1998
.
Experiment Name
Design (Number of Treatments)
SETRE
S (4)
3 Sensibility
A series of sensibility tests have been employed to test the behaviour of the model in regions not explicitly included in the previous test set.  Furthermore, these tests explore the emergent behaviour of the model under a range of changing climate, fertility and management scenarios to ensure that simulated patterns agree with expected behaviours.
3.1 MAI in SE Australia
Representative growth rates (MAI) for
P. radiata
in temperate Australia are up to 30 m3/ha/year of stem wood volume overbark, or more where accessing an aquifer or receiving irrigation. Simulations for 4 sites are presented here to capture a range of environmental conditions.  Climate data has been taken from nearby towns and common soil properties have been used for all sites, with soil properties reflecting a relatively high state of fertility. The peak MAI values for these simulations are 13-29 m3/ha/year at 26 years, i.e. within the range of expectations
Turvey, 1983
, and the relativities in relation to rainfall and temperature are also as expected. Rainfall is explored explicitly for the Mount Worth site in the next sensibility test.
3.2 Response to Rainfall
LAI and MAI of
P. radiata
should increase with rainfall. This simulation experiment explores the changes in LAI and MAI with a hypothetical rainfall gradient created for the Mount Worth site (fertile soil and mean annual rainfall 1131 mm) above by multiplying daily rainfall by factors in the range 0.3-1.8, i.e. annual rainfall of 339-2036 mm.  Data from
Specht, 1972
show that canopy cover should be almost complete for the wetter sites in this study, and decrease to approximately 50% at the drier sites, which is approxiamtely simulated here.
Experiment Name
Design (Number of Treatments)
Climate
RainFactor (6)
3.3 Response to Soil Fertility
Site fertility is an important driver of the pattern of tree growth rates.  As site fertility declines, the long term growth rate (e.g. MAI) should also decrease, and the time to obtaining peak MAI can remain the similar or decrease.  This sensibility test uses the Mount Worth location in Victoria, with soil fertility hypothetically altered to three levels.  As expected, simulated MAI decreased with soil fertility, and there was a decrease in time to peak MAI.
Experiment Name
Design (Number of Treatments)
Fertility
Level (3)
3.4 Reponse to N Fertilizer
Pinus response to rate of N fertiliser are often asymptotic, the plateau of which is determined by other limiting factors. In the sensibility test presented here, an infertile soil at Stockdale, Victoria, Australia, was used as the basis for the simulations
P. radiata
was fertilised at 3 years of age and stem volume assessed at 26 years of age.
Operationally,
P. radiata
plantations in Australia receive several hundered kg N/ha during a rotation. The approximate optimum rate simulated (90% of maximum) was 900 kg N/ha, which is therefore within the range of expectation.
Experiment Name
Design (Number of Treatments)
Stockdale
N (8)
3.5 GxE
These graphs show the simulated yields (MAI) of all genotypes in the model grown for 26 years at northern and southern contrasting sites in Australia (Toolara and TowerHill). There is a tendency for Australian genotypes of
P. radiata
(TowerHill and BFG) to grow best in southern Australia, as expected, and conersely Australian tropical and sub-tropical genotypes (e.g. elliottiiXcaribaeaF1) to grow best in northern Australia. The New Zealand genotype (GF7) was simulated to grow best in the long-term at both locations, and the USA genotypes were worst. A wide ranage of yield potentials and patterns are therefore available.
Although the MAI values are generally as expected for all genotypes of
P. radaita
Turvey, 1983
and others, this analysis raises some questions that could help focus further model improvements.
Calibration of the model focussed on Australian datasets.
The elliottiiIMPAC and taedaIMPAC genotypes simulate noticably reduced preformace at later ages. This is probably a consequence of simulations at the IMPAC site in Florida only going to 18 years of age for calibration, as thereafter stocking (and probably vigour of survivors) decreased considerably due to disease, which could not be simulated.
Calibration of the GF7 genotype of
P. radiata
for the Puruki site in New Zealand went only 12 years of age as stocking thereafter went to very low values which was not a focus of the calibrations needed for Australia. If such low stocking values need to be calibrated for in future, one might need to explore biomass allocation a function stocking (population) to attain better model skill in these circumstances.
Several Australian genotypes performed similarly during simulations, indicating little practical difference in their calibration and therefore scope for merging, e.g. the elliottiiXcaribaeaF1 and elliottiiXcaribaeaF2 genotypes.
One would expect more discrimination than simulated between temperate and other genotypes when simulating growth at these northern and southern extremes of commercial plantations in Australia. To achieve that though model capability might be need for disease occurance and impacts on survival and growth, which beyond the current scope of validation.
Note that due to the long-term nature of plantation forestry, observed datasets are necessarily old and probably using out-dated genotypes. Therefore, using the genotypes curently available in the model might not be appropriate for simulating the performance of current or future plantations.
Experiment Name
Design (Number of Treatments)
GxE
Site x Clone (24)
4 Interface
4.1 Pinus
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
Albaugh, Timothy J, Allen, H Lee, Dougherty, Phillip M, Johnsen, Kurt H, 2004. Long term growth responses of loblolly pine to optimal nutrient and water resource availability. Forest ecology and management 192 (1), 3-19.
Albaugh, Timothy J, Allen, H Lee, Dougherty, Phillip M, Kress, Lance W, King, John S, 1998. Leaf area and above-and belowground growth responses of loblolly pine to nutrient and water additions. Forest Science 44 (2), 317-328.
Albaugh, Timothy, Allen, H Lee, Fox, Thomas R, Carlson, Colleen A, Rubilar, Rafael A, 2009. Opportunities for fertilization of loblolly pine in the sandhills of the southeastern United States. Southern Journal of Applied Forestry 33 (3), 129-136.
Beets, Peter N, Garrett, Loretta G, 2018. Carbon fraction of Pinus radiata biomass components within New Zealand. New Zealand Journal of Forestry Science 48 (1), 1-8.
Beets, Peter N, Kimberley, Mark O, Paul, Thomas SH, Garrett, Loretta G, 2011. Planted Forest Carbon Monitoring System-forest carbon model validation study for Pinus radiata.. New Zealand Journal of Forestry Science (New Zealand Forest Research Institute Ltd (trading as Scion)) 41.
Beets, Peter N, Whitehead, David, 1996. Carbon partitioning in Pinus radiata stands in relation to foliage nitrogen status. Tree physiology 16 (1-2), 131-138.
Beets, PN, Beets, JM, 2020. Soil water storage changes in a small headwater catchment in the central North Island of New Zealand following afforestation with Pinus radiata. Forest Ecology and Management 462, 117967.
Beets, PN, Brownlie, RK, 1987. Puruki experimental catchment: site, climate, forest management, and research. New Zealand Journal of Forestry Science 17 (2/3), 137-160.
Beets, PN, Lane, PM, 1987. Specific leaf area of Pinus radiata as influenced by stand age, leaf age, and thinning.. New Zealand Journal of Forestry Science 17 (2), 283-291.
Beets, PN, Pearce, SH, Oliver, GR, Clinton, PW, 2007. Root/shoot ratios for deriving below-ground biomass of Pinus radiata stands. New Zealand Journal of Forestry Science 37 (2), 267.
Beets, PN, Pollock, DS, 1987. Accumulation and partitioning of dry matter in Pinus radiata as related to stand age and thinning.. New Zealand Journal of Forestry Science 17 (2), 246-271.
Beets, PN, Pollock, DS, 1987. Uptake and accumulation of nitrogen in Pinus radiata stands as related to age and thinning.. New Zealand Journal of Forestry Science 17 (2), 353-371.
Benson, ML, Landsberg, JJ, Borough, CJ, 1992. The Biology of Forest Growth experiment: an introduction. Forest Ecology and Management 52 (1-4), 1-16.
Benson, ML, Myers, BJ, Raison, RJ, 1992. Dynamics of stem growth of Pinus radiata as affected by water and nitrogen supply. Forest Ecology and Management 52 (1-4), 117-137.
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Burdon, RD, Carson, MJ, Shelbourne, CJA, 2008. Achievements in forest tree genetic improvement in Australia and New Zealand 10: Pinus radiata in New Zealand. Australian Forestry 71 (4), 263-279.
Crane, WJB, Banks, JCG, 1992. Accumulation and retranslocation of foliar nitrogen in fertilised and irrigated Pinus radiata. Forest Ecology and Management 52 (1-4), 201-223.
Cremer, KW, 1992. Relations between reproductive growth and vegetative growth of Pinus radiata. Forest Ecology and Management 52 (1-4), 179-199.
Dieters, Mark, Brawner, Jeremy, 2007. Productivity of Pinus elliottii, P. caribaea and their F 1 and F 2 hybrids to 15 years in Queensland, Australia. Annals of forest science 64 (7), 691-698.
Dyck, WJ, Mees, CA, Hodgkiss, PD, 1987. Nitrogen availability and comparison to uptake in two New Zealand Pinus radiata forests.. New Zealand journal of forestry science 17 (2), 338-352.
Garrett, LG, Beets, PN, Clinton, PW, Smaill, SJ, 2019. National series of long-term intensive harvesting trials in Pinus radiata stands in New Zealand: Initial biomass, carbon and nutrient pool data. Data in brief 27, 104757.
Gonzalez-Benecke, Carlos A, Jokela, Eric J, Martin, Timothy A, 2012. Modeling the effects of stand development, site quality, and silviculture on leaf area index, litterfall, and forest floor accumulations in loblolly and slash pine plantations. Forest Science 58 (5), 457-471.
Gonzalez-Benecke, Carlos A, Teskey, Robert O, Martin, Timothy A, Jokela, Eric J, Fox, Thomas R, Kane, Michael B, Noormets, Asko, 2016. Regional validation and improved parameterization of the 3-PG model for Pinus taeda stands. Forest Ecology and Management 361, 237-256.
Holzworth, Dean P., Huth, Neil I., deVoil, Peter G., Zurcher, Eric J., Herrmann, Neville I., McLean, Greg, Chenu, Karine, van Oosterom, Erik J., Snow, Val, Murphy, Chris, Moore, Andrew D., Brown, Hamish, Whish, Jeremy P. M., Verrall, Shaun, Fainges, Justin, Bell, Lindsay W., Peake, Allan S., Poulton, Perry L., Hochman, Zvi, Thorburn, Peter J., Gaydon, Donald S., Dalgliesh, Neal P., Rodriguez, Daniel, Cox, Howard, Chapman, Scott, Doherty, Alastair, Teixeira, Edmar, Sharp, Joanna, Cichota, Rogerio, Vogeler, Iris, Li, Frank Y., Wang, Enli, Hammer, Graeme L., Robertson, Michael J., Dimes, John P., Whitbread, Anthony M., Hunt, James, van Rees, Harm, McClelland, Tim, Carberry, Peter S., Hargreaves, John N. G., MacLeod, Neil, McDonald, Cam, Harsdorf, Justin, Wedgwood, Sara, Keating, Brian A., 2014. APSIM – Evolution towards a new generation of agricultural systems simulation. Environmental Modelling  and  Software 62, 327-350.
Huth, N. I., Carberry, P. S., Cocks, B., Graham, S., McGinness, H. M., O'Connell, D. A., 2008. Managing drought risk in eucalypt seedling establishment: An analysis using experiment and model. Forest Ecology and Management 255 (8-9), 3307-3317.
Huth, N. I., Snow, V. O., Keating, B. A., 2001. Integrating a forest modelling capability into an agricultural production systems modelling environment-current applications and future possibilities.., Eds: Zerger, A. and Argent, R.M., 1895-1900.
Huth, Neil Ian, Carberry, P.S., Poulton, P.L., Brennan, L.E., Keating, Brian A., 2002. A framework for simulating agroforestry options for the low rainfall areas of Australia using APSIM. European Journal of Agronomy 18, 171-185.
Johnson, IG, Cotterill, IM, Raymond, Carolyn A, Henson, M, others, 2008. Half a century of Pinus radiata tree improvement in New South Wales. New Zealand Journal of Forestry 52 (4), 7.
Jokela, Eric J, Dougherty, Philip M, Martin, Timothy A, 2004. Production dynamics of intensively managed loblolly pine stands in the southern United States: a synthesis of seven long-term experiments. Forest ecology and management 192 (1), 117-130.
Jokela, Eric J, Martin, Timothy A, 2000. Effects of ontogeny and soil nutrient supply on production, allocation, and leaf area efficiency in loblolly and slash pine stands. Canadian Journal of Forest Research 30 (10), 1511-1524.
Jokela, Eric J, Martin, Timothy A, Vogel, Jason G, 2010. Twenty-five years of intensive forest management with southern pines: Important lessons learned. Journal of Forestry 108 (7), 338-347.
Khanna, PK, Raison, RJ, Falkiner, RA, Willett, IR, Connell, MJ, 1992. Effects of NPK fertilisation on the chemistry of a yellow podzolic soil under Pinus radiata. Forest ecology and management 52 (1-4), 65-85.
Kimberley, MO, Beets, PN, 2007. National volume function for estimating total stem volume of Pinus radiata stands in New Zealand. New Zealand Journal of Forestry Science 37 (3), 355.
Kirschbaum, Miko UF, 1999. CenW, a forest growth model with linked carbon, energy, nutrient and water cycles. Ecological Modelling 118 (1), 17-59.
Landsberg, Joe J, Johnsen, Kurt H, Albaugh, Timothy J, Allen, H Lee, McKeand, Steven E, 2001. Applying 3-PG, a simple process-based model designed to produce practical results, to data from loblolly pine experiments. Forest Science 47 (1), 43-51.
Martin, TA, Jokela, EJ, 2004. Developmental patterns and nutrition impact radiation use efficiency components in southern pine stands. Ecological Applications 14 (6), 1839-1854.
McMurtrie, RE, Landsberg, JJ, 1992. Using a simulation model to evaluate the effects of water and nutrients on the growth and carbon partitioning of Pinus radiata. Forest Ecology and Management 52 (1-4), 243-260.
Myers, BJ, Talsma, T, 1992. Site water balance and tree water status in irrigated and fertilised stands of Pinus radiata. Forest Ecology and Management 52 (1-4), 17-42.
Neilsen, WA, Lynch, T, 1998. Implications of pre-and post-fertilizing changes in growth and nitrogen pools following multiple applications of nitrogen fertilizer to a Pinus radiata stand over 12 years. Plant and soil 202 (2), 295-307.
Neilsen, WA, Pataczek, W, Lynch, T, Pyrke, R, 1992. Growth response of Pinus radiata to multiple applications of nitrogen fertilizer and evaluation of the quantity of added nitrogen remaining in the forest system. Plant and Soil 144 (2), 207-217.
Oliver, GR, Beets, PN, Garrett, LG, Pearce, SH, Kimberly, MO, Ford-Robertson, JB, Robertson, KA, 2004. Variation in soil carbon in pine plantations and implications for monitoring soil carbon stocks in relation to land-use change and forest site management in New Zealand. Forest Ecology and Management 203 (1-3), 283-295.
Parfitt, R. L., Schipper, L. A., Baisden, W. T., Elliott, A. H., 2006. Nitrogen inputs and outputs for New Zealand in 2001 at national and regional scales. Biogeochemistry 80 (1), 71-88.
Pongracic, Silvia, 2001. Influence of irrigation and fertilization on the belowground carbon allocation in a pine plantation..
Raison, RJ, Khanna, PK, Benson, ML, Myers, BJ, McMurtrie, RE, Lang, ARG, 1992. Dynamics of Pinus radiata foliage in relation to water and nitrogen stress: II. Needle loss and temporal changes in total foliage mass. Forest ecology and management 52 (1-4), 159-178.
Raison, RJ, Khanna, PK, Connell, MJ, Falkiner, RA, 1990. Effects of water availability and fertilization on nitrogen cycling in a stand of Pinus radiata. Forest Ecology and Management 30 (1-4), 31-43.
Raison, RJ, Myers, BJ, 1992. The biology of forest growth experiment: linking water and nitrogen availability to the growth of Pinus radiata. Forest Ecology and Management 52 (1-4), 279-308.
Raison, RJ, Myers, BJ, Benson, ML, 1992. Dynamics of Pinus radiata foliage in relation to water and nitrogen stress: I. Needle production and properties. Forest Ecology and Management 52 (1-4), 139-158.
Rijkse, Willem Cornelis, Bell, JL, 1974. Soils of Purukohukohu IHD Experimental Basin, Rotorua County, North Island, New Zealand. NZ soil survey report.
Simpson, JA, Xu, ZH, Smith, T, Keay, P, Osborne, DO, Podberscek, M, others, 2000. Effects of site management in pine plantations on the coastal lowlands of subtropical Queensland, Australia.. Site management and productivity in tropical plantation forests: a progress report. Workshop Proceedings, Kerala, India, 7-11 December 1999, 73-81.
Smethurst, Philip J, Valadares, Rafael V, Huth, Neil I, Almeida, Auro C, Elli, Elvis F, Neves, J'ulio CL, 2020. Generalized model for plantation production of Eucalyptus grandis and hybrids for genotype-site-management applications. Forest Ecology and Management 469, 118164.
Snowdon, P, Benson, ML, 1992. Effects of combinations of irrigation and fertilisation on the growth and above-ground biomass production of Pinus radiata. Forest Ecology and Management 52 (1-4), 87-116.
Specht, RL, 1972. Water use by perennial evergreen plant communities in Australia and Papua New Guinea. Australian Journal of Botany 20 (3), 273-299.
Subedi, Praveen, Jokela, Eric J, Vogel, Jason G, Martin, Timothy A, 2019. Sustained productivity of intensively managed loblolly pine plantations: Persistence of fertilization and weed control effects across rotations. Forest Ecology and Management 446, 38-53.
Turvey, Nigel D, 1983. Soil-type yield curves for Pinus radiata in Gippsland, Victoria. Australian Forestry 46 (2), 118-125.
Vogel, Jason G, Suau, Luis J, Martin, Timothy A, Jokela, Eric J, 2011. Long-term effects of weed control and fertilization on the carbon and nitrogen pools of a slash and loblolly pine forest in north-central Florida. Canadian Journal of Forest Research 41 (3), 552-567.
Waterworth, Robert, Raison, R John, Brack, Cris, Benson, Martin, Khanna, Partap, Paul, Keryn, 2007. Effects of irrigation and N fertilization on growth and structure of Pinus radiata stands between 10 and 29 years of age. Forest ecology and management 239 (1-3), 169-181.
Wu, Harry X, Eldridge, Ken G, Matheson, A Colin, Powell, Mike P, McRae, Tony A, 2007. Successful introduction and breeding of radiata pine to Australia. Growing Forest Values, Proceedings of ANZIF 2007 Conference, Coffs Harbour, NSW, Australia, 3-7.
Zhao, Dehai, Kane, Michael, Teskey, Robert, Fox, Thomas R, Albaugh, Timothy J, Allen, H Lee, Rubilar, Rafael, 2016. Maximum response of loblolly pine plantations to silvicultural management in the southern United States. Forest Ecology and Management 375, 105-111.
