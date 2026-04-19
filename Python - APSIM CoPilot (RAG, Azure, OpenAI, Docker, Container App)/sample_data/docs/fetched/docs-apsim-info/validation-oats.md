# Oats documentation

Source URL: https://docs.apsim.info/validation/Oats
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:08.305857+00:00

1 The APSIM Oats Model
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
Allan Peake, Hamish Brown, Rob Zyskowski, Edmar I. Teixeira, Neil Huth
The APSIM oats model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a model in much the same way that models can be coupled to construct a simulation. This means that dynamic composition of lower level process and organ classes (e.g. photosynthesis, leaf) into larger constructions (e.g. maize, wheat, sorghum) can be achieved by the model developer without additional coding.
The oats model consists of:
a phenology model to simulate development through sequential growth phases
a structure model to simulate plant morphology
a collection of organs to simulate the various plant parts
an arbitrator to allocate resources (N, biomass) to the various plant organs
This work builds upon an earlier APSIM Oats model that was constructed in 2007 and then published at the Australian Agronomy Conference in 2008 using data from Gatton, Tarlee and Pinery (
Peake et al., 2008
). However numerous changes have been made in the APSIM NextGen version, so simulations run under APSIM 7.10 and previous versions will give different predictions when run with APSIM NextGen.
The Oats model is broadly based on APSIM Wheat models such as NWheat (
S Asseng et al., 2002
,
BA Keating, 2001
), NWheatS (
S Asseng et al., 1998
), Cropmod-Wheat (
Wang et al., 2002
), and the earlier versions developed within the Plant Modelling Framework (
Brown et al., 2014
).
The model is constructed from the following list of software components. Details of the implementation and model parameterisation are provided in the following sections.
1.1 Plant Model Components
Component Name
Component Type
Arbitrator
Models.PMF.OrganArbitrator
Phenology
Models.PMF.Phen.Phenology
Structure
Models.PMF.Struct.Structure
Grain
Models.PMF.Organs.ReproductiveOrgan
Leaf
Models.PMF.Organs.Leaf
Stem
Models.PMF.Organs.GenericOrgan
Root
Models.PMF.Organs.Root
Panicle
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
Ear
Models.PMF.CompositeBiomass
StemPlusPanicle
Models.PMF.CompositeBiomass
1.3 Cultivars
Cultivar Name
Alternative Name(s)
Drummond
Drummond
Drummond_orig
Drummond_orig
Milton
Milton
Hokonui
Hokonui
Stampede
Stampede
Coolibah
Coolibah
Taipan
Taipan
Wintaroo
Wintaroo
Kangaroo
Kangaroo
Brusher
Brusher
Algerian
Algerian
Nugene
Nugene
Drover
Drover
Genie
Genie
Aladdin
Aladdin
Comet
Comet
Wizard
Wizard
Bond
Bond
Coronet
Coronet
CROA146
CROA146
107_32
107_32
Armstrong
Armstrong
100_05
100_05
Milton2
Milton2
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
Oat crops exhibit a range of developmental responses to environment and these are strongly influences by genotype characteristics.
Temperature effects development increasing development rates and decreasing phase durations as temperatures increase.  These effects are captured by thermal time.  However, Oat crops also exhibit vernalisation and photoperiod sensitivities in their Vegetative phase and further photoperiod sensitivity in the EarlyReproductive phase.  Photoperiod responses are seen as a reduction in the length of a phase for a photoperiod sensitive genotype in response to a longer photoperiod.  Vernalisation responses are more complicated as they are driven by temperature but interact with photoperiod.  For vernalisation sensitive varieties (Winter types) exposure to cool temperatures or short photoperiods during the Vegetative phase will reduce the thermal time duration of the vegetative phase.
As used in the APSIM wheat model, we draw on the Kirby Framework to capture these vernalisation and photoperiod responses.  This framework assumes that the timing of anthesis is a result of the timing of flag leaf and an additional thermal time passage from there to anthesis.  It also assumes the timing of flag leaf is a result of the Final Leaf Number which sets a target, and leaf appearance rate, which sets the rate of progress toward this target.  Leaf appearance rate is a function of Thermal time and a genotype specific Phyllochron which changes with Haun stage as described by
Jamieson et al., 1995
.
Final Leaf Number (FLN) is modelled as the sum of three numbers:
FLN = MinLeafNumber + VernalLeaves + PhotoLeaves
Where MinLeafNumber is the number of leaves that a wheat crop will produce when vernalisation is satisified early in the crops duration (before 2nd true leaf) and it is grown in a long photoperiod.  VernalLeaves are the number of leaves that are added due to vernalisation effects.  For insensitive varieties this will always be zero but this is potentially a larger number for sensitivie varieties and the number progresively decreases as the crop encounters more vernalisation.  PhotoLeaves are the number of leaves that are added to the minimum leaf number as a result of short day exposure.  For insensitive varieties this will be zero but is potential larger for more sensitivie varieties and decreases as day length increases.  More detailed explinations of the components of phenology are provided below.
1.4.3 Structure
The structure model simulates morphological development of the plant to inform the Leaf class
when and how many leaves and branches appear and provides an estimate of height.
1.4.4 Grain
This organ uses a generic model for plant reproductive components.  Yield is calculated from its components in terms of organ number and size (for example, grain number and grain size).
As with Barley, Oats grain is typically harvested by farmers still in its husk (i.e. the palea and lemma remain attached to the kernel/caryopsis), while the kernel proper is typically smaller than a wheat grain grown under similar conditions. For simplicity (and in the absence of an extensive grain-oats data set) the Oats model uses the same grain size model as wheat, with the assumption that the weight of the oats kernel includes the weight of the husk (which is not included within kernel weight in the wheat model).
Grain number prediction is also derived directly from the wheat model, estimated as a constant number of grains (default=26) per unit of stem+spike dry matter at anthesis. This approach has known limitations and investigation is underway to improve this function in the future.
1.4.5 Leaf
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
1.4.6 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.7 Root
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
1.4.8 Panicle
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
1.4.9 MortalityRate
A constant function (name=value)
1.4.10 SeedMortalityRate
A constant function (name=value)
2 Validation
A test dataset has been developed to test the APSIM Oats model for a range of environmental (soil and climate) conditions, management options (sowing dates, populations, nitrogen rates, row spacing, irrigation), genetic backgrounds (different regions, cultivar types) and for special considerations such as defoliation (or simulated grazing).  These tests have been groups into various geographical regions to allow the user to evaluate the suitability of the model for their particular region of interest.  Graphs of model performance are provided for yield, biomass production, canopy development, phenological development, water and nitrogen uptake, and grain yield components.
2.1 Map
2.2 Combined Results
Simulation results for the combined datasets from the various countries are shown in the following graphs.  The model is able to adequately capture the influence of growing conditions (soil, climate) and management (population, Nitrogen, irrigation, sowing date).
2.3 New Zealand
Experiment Name
Design (Number of Treatments)
RS_Oats_9899
_Irrig (5)
CPT
Sow x Cult (18)
RS_Oats_1718
Nit x Irr (6)
Data from:
Martin et al., 2001
The response of oats (Avena sativa L.) to timing and intensity of drought was determined in a mobile
rainshelter, which excluded rainfall during crop growth. Ten irrigation treatments subjected the crops to
drought of varying duration at different stages during plant growth; the crops were otherwise fully
irrigated. Oat grain yield generally increased linearly with cumulative water use, and decreased linearly as
the maximum potential soil moisture deficit experienced during crop growth increased, regardless of the
timing of drought. The exception to this was the no drought (fully irrigated) treatment, where lodging
reduced panicle number and grain weight compared to early drought treatments. Drought affected yield
mainly by reducing panicle and grain number, effects on grain weight were small. . Relieving early drought
through late irrigation led to more late tillers, more panicles, heavier grains, and later maturity. Full and
late drought increased screenings.
Jamieson, P. D. and C. A. Munro (1999). A simple method for the phenological evaluation of new cereal cultivars. Agronomy Society of New Zealand - Proceedings, Twenty-Ninth Annual Conference, 1999. J. G. Hampton and K. M. Pollock. 29: 63-68.
This experiment is part of the ongoing Cultivar Prediction Trial at Plant & Food Lincoln wher a Cultivar is planted at 3 sowing dates throughout the year and the time of flowering, Final leaf number and Flag leaf are recorded.
####Experiment description
Introduction
This is a simulation of Oats in a crop rotation consisting of a forage species, fodder beet, grown over spring/summer and harvested in autumn, followed by the catch crop, oats. The actual experiment was done in two parts in the rain shelter facility of Plant and Food Research, Lincoln, NZ.  The experiments were designed to help develop the model for the respective plants and here they are used to further demonstrate that ApsimX can simulate water and nitrogen cycling in the field and then to examine whether catch crops are a good option for mitigating N leaching from forages.
Part 1: Co-limitation of water and nitrogen on fodder beet physiology (no detail supplied for this experiment as it is not simulated herein)
Part 2: Ability of oats to act as catch-crops over winter/spring
Irrigation: 2 treatments, low (enough to keep plants growing) or full irrigation (to match PET)
Nitrogen: 3 treatments, 80 kg N/ha, 125 kg N/ha & 320 kg N/ha at sowing (urea applied at sowing)
Oats Experiment Management:
Cultivar: Milton
Sowing density: 300 plants/m2
row spacing: 15 cm
depth: 45 mm
General actions:
05/07/2017 - Sowing
05/07/2017 - Fertiliser application (400kg/ha Potash, Urea: treat4=80kgN/ha, treat5=125kgN/ha, treat6=320kgN/ha)
24/07/2017 - Plants started to emerge
11/09/2017 - Irrigation started in treatements 4, 5, and 6
06/11/2017 - Irrigation extended to all plots
04-09/01/2018 - Plots harvested
2.3.1 SAE
Experiment Name
Design (Number of Treatments)
SAE_Oats_2020
Cultivars (7)
The aim of this SAE funded project is to assess contrasting plant traits in oats.
The focus is on early-vigour as a critical plant trait in winter-sown oat catch crops.
Early vigour influences N uptake rate by the catch crop and by association may also reduce the amount of residual soil N at risk of leaching.
The treatments are 6 oat genotypes with potential differences for early canopy expansion.
The experiment had 4 replicates
400 kg N/ha at sowing to mimic  aurine patch to ne mopped by the oat catch crop
Key differences in early vigour were due to individual leaf size in first 6 leaves
2.3.2 FRNL
Experiment Name
Design (Number of Treatments)
WinterSowing2015
Sow x Fert (4)
WinterSowing2016
Sow (2)
WinterSowing2017
Rate (2)
TempletonLysimeter2017
Sow (2)
BalmoralLysimeter2017
Sow (2)
Southland2018
Sow (4)
FRNL Experiment: Data from
Malcolm et al., 2016
Note: the 2015 version of this experiment established inconsistently and data is not of the same quality as that generated in 2016.
Grazing winter forage crops is an important management strategy for profitable livestock production in many New Zealand regions where there is commonly a pasture feed deficit during this period. However, the high stocking densities that are often associated with these systems can have negative environmental effects, such as nitrogen (N) leaching losses to groundwater. This field study investigated the potential biomass production and reduced risk of N leaching from establishing oats (Avena sativa L.) as a green-chop catch crop after winter-grazed forage kale (Brassica oleracea var. acephala L.) in Canterbury, New Zealand. Oat crops were direct-drilled on two sowing dates (early: 1 July 2015 and late: 1 August 2015) and were managed under high (400 kg N/ha) and low (0 kg N/ha) N load conditions, representing urine-patch and inter urine-patch areas. For the early-sown crops yields were 6.1 (low N) and 11.8 t DM/ha (high N) at final harvest (50% ear emergence) on 19 November 2015. For the late-sown crops yields were 6.7 (low N) and 10.1 t DM/ha (high N) at the same development stage (26 November 2015). By establishing oats as a catch crop in winter soil profile mineral-N (0-120 cm depth) was reduced by up to 86% (early) and 80% (late) in the high N (simulated urine-patch) treatments compared with the respective fallow treatment. These results indicated the technical feasibility of direct drilling oats as catch crops immediately after winter forage grazing, with likely production and environmental benefits such as the reduced risk of N leaching losses.
FRNL Experiment: Data from
Malcolm et al., 2016
Grazing winter forage crops is an important management strategy for profitable livestock production in many New Zealand regions where there is commonly a pasture feed deficit during this period. However, the high stocking densities that are often associated with these systems can have negative environmental effects, such as nitrogen (N) leaching losses to groundwater. This field study investigated the potential biomass production and reduced risk of N leaching from establishing oats (Avena sativa L.) as a green-chop catch crop after winter-grazed forage kale (Brassica oleracea var. acephala L.) in Canterbury, New Zealand. Oat crops were direct-drilled on two sowing dates (early: 1 July 2015 and late: 1 August 2015) and were managed under high (400 kg N/ha) and low (0 kg N/ha) N load conditions, representing urine-patch and inter urine-patch areas. For the early-sown crops yields were 6.1 (low N) and 11.8 t DM/ha (high N) at final harvest (50% ear emergence) on 19 November
2015. For the late-sown crops yields were 6.7 (low N) and 10.1 t DM/ha (high N) at the same development stage (26 November 2015). By establishing oats as a catch crop in winter soil profile mineral-N (0-120 cm depth) was reduced by up to 86% (early) and 80% (late) in the high N (simulated urine-patch) treatments compared with the respective fallow treatment. These results indicated the technical feasibility of direct drilling oats as catch crops immediately after winter forage grazing, with likely production and environmental benefits such as the reduced risk of N leaching losses.
FRNL Experiment conducted in ABlock in 2017
Measurements included crop development, yield, N conc final soil mineral N
Soil - templeton
Target population - 300 plants/m2
Urine patch (fallow versus cover crop) experiment
Report below indicates five irrigation events Oct-Dec. These have been estimated.
Reported in Malcolm B, Teixeira E, Maley S, Johnstone P, Hunt A, de Ruiter J 2018b. FRNL Critical step 2.12 – results summary from the 2017 Canterbury field studies, March 2018. A Plant & Food Research report prepared for: PFR (SAE). Milestone No. 74326. Contract No. NA. Job code: P/442060/09. SPTS No. 16106.
FRNL Lysimeter experiment conducted in ABlock in 2017
Measurements included crop development, yield, N conc final soil mineral N
Soil described as Templeton (see separate simulation Balmoral soil)
Target population - 300 plants/m2
Urine patch (fallow versus cover crop) experiment
Oats sown in July or August.
FRNL Lysimeter experiment conducted in ABlock in 2017
Measurements included crop development, yield, N conc final soil mineral N
Soil described as Balmoral (see separate simulation Templeton soil)
Target population - 300 plants/m2
Urine patch (fallow versus cover crop) experiment
Oats sown in July or August.
Controlled FRNL trial
Trial location: Mossburn, Southland
Coordinates: 45.686°S, 168.265°E
Soil: Morven (considered stony)
Weather station: On-site (alternative closest NIWA station used in simulation)
This experiment evaluated oats sown in June or August targeting 300 plants/m2)
The paddock was previously in wheat
400 kg N/ha applied at sowing to simulate urine patch.
2.3.3 SLMACC
Experiment Name
Design (Number of Treatments)
SLMACC_Canterbury_2020
Rate (8)
SLMACC_Canterbury_2021
Rate (3)
SLMACC_Southland_2020
Rate (4)
SLMACC_Southland_2021
Rate (4)
SLMACC_WestCoast1_2020
Rate (2)
SLMACC_Southland_2021Lys
Sow (3)
Trial location: Springston, Canterbury
Coordinates: 43.641°S, 172.349°E
Soil: Balmoral
Weather station: Broadfield
This experiment evaluated oat populations (i.e. target of 0 [fallow],150, 300, 450 plants/m2)
Two row spacings (75 & 150 cm) were also evaluated in the trial (see factors)
Note that these ppopulations were not achieved so, actual plant counts are used in the model (see factors).
The paddock was previously in a kale crop
This is a grazed field and grazing occured on the winter (kale) crop prior to sowing oats
Irrigation: a total of 210 mm was applied to all treatments (source: BM. SM)
According to the DairyNZ fact sheet (
https://www.dairynz.co.nz/media/253803/1-74_Kale_growing_a_high_yielding_crop.pdf
), Kale yield can vary (12-17 t DM)depending on the variety (short, intermediate & giant). Utilisation efficiency is 80-85%.
According to Chakwizira et al 2015(
https://www.tandfonline.com/doi/epdf/10.1080/00288233.2015.1016538?needAccess=true&role=button
), a well fertilised crop contains ~1.3%.
Based on a 15 t DM/ha (average yield), aboveground residue will be 2.25 t DM/ha.
Root biomass is assumed to be 9.9 % (i.e. 1.485 t DM/ha) according to Urlic et al 2015(
https://www.researchgate.net/publication/273506706_Phosphorus-Use_Efficiency_of_Kale_Genotypes_from_Coastal_Croatia/figures?lo=1
).
We can approximate that Root N conc is ~1.3% based on Canola research data
Trial location: Barhill, Canterbury
Coordinates:43.757°S, 171.932°E
Soil: Templeton
Weather station: Chertsey Cws
This experiment evaluated oat populations (i.e. target of 0 [fallow],150, 300 plants/m2)
Note that these populations were not achieved so, actual plant counts are used in the model (see factors).
The paddock was previously in a kale crop
This is a grazed field and grazing occured on the winter (kale) crop prior to sowing oats
A total of 100 kg N/ha as urea was applied to oats & the crop grown to grain.
Rates adjusted based on measured average plant population - ENK
Trial location: Winton, Southland
Coordinates: 46.198°S, 168.307°E
Soil: Caroline - descried as moderately deep (see
https://greatsouth.nz/storage/app/media/data_sheets/R_13_4_5382.pdf
)
In the above web document, Caroline is said to be similar to Lumsden, Makarewa & Jacobstown soils.
Weather station: On-site. However measurements were not taken daily and so the Gore NIWA station was used
The experiment evaluated oat populations (i.e. target of 0 [fallow],150, 300, 450 plants/m2)- row spacing was 150 cm
Note: target populations were not achieved and so, actual plant counts are used in the model (see factors).
The paddock was previously in a swedes
This is a grazed field.
Trial location: Wrey's Bush, Southland
Coordinates: 46.021°S, 168.090°E
Soil: Aparima
Weather station: Birchwood Wxt Aws
The experiment evaluated oat populations (i.e. target of 0 [fallow],150, 300, 450 plants/m2)- row spacing was 150 cm
Note: target populations were not achieved and so, actual plant counts are used in the model (see factors).
The paddock was previously in a swedes
This is a grazed field.
Rates adjusted based on measured average plant population - ENK
Negative values in interception data zeroed - ENK
Trial location: Hokitika, West Coast
Coordinates: 42.771°S, 170.909°E
Soil: Descriped as heavy glay - using Huangarua soil in this simulation
Weather station: Hokitika AWS
The experiment evaluated oat populations (i.e. target of 0 [fallow], 300 plants/m2)- row spacing was 150 cm
Note: target populations were not achieved and so, actual plant counts are used in the model (see factors).
The paddock was previously in a turnip-swedes
This is a grazed field.
Lysimeter experiment
Trial location: Wallacetown, Southland
Coordinates: 46.311°S, 168.311°E
Soil: Waikiwi
Weather station: Invercargill Aero 2 Ews
Urine (200 kg N/ha) and non-urine comparison
Shallow (10 cm) and deep (25 cm) cultivation
Oats sown in July or September
Measurements = Soil properties, N & P leaching, biomass, N uptake, soil N (taken at the end of trial)
The paddock was previously in long-term pasture (10yrs+)
Texture
Depth		% sand		% silt		% clay
0-10 cm		3.4		68.5		28.2
10-20 cm	3.4		70.7		25.9
20-30 cm	4.4		68.5		27.1
30-40 cm	4.2		69.5		26.3
40-50 cm	3.9		70.8		25.3
50-60 cm	3.3		71.2		25.4
60-70 cm	6.8		66.9		26.3
2.4 Australia
The Oats model construction for Australian varieties was based primarily around the experiment at Gatton in 2007. Supporting phenology data was collected at the Leslie Research Centre (LRC) and Wellcamp Research Station for northern varieties, data kindly supplied by Bruce Winter, Oats Breeder with DAFQ. The model was then extended to data sets at Roma in QLD, and also sites in South Australia (Tarlee, Pinery) where additional adjustments were made to the relevant varieties.
The Gatton, Roma, Tarlee, and Pinery experiments were used to construct the original APSIM Oats model and published at the Australian Agronomy Conference in 2008.
Peake A, Whitbread A, Davoren B, Braun J and Limpus, S. (2008). The development of a model in APSIM for the simulation of grazing oats and oaten hay. In: Global Issues, Paddock Action. Proceedings of the 14th ASA Conference, 21-25 September 2008, Adelaide, South Australia.
2.4.1 Gatton2007
This experiment comprised an early and late sowing date, but full growth and soil water data was only collected for the early sowing date. The late sowing date was comprised of observation plots for phenology data collection only.
Extinction coefficient and 'kl' values were measured on the early sowing date in addition to partitioned biomass data. Note that the kl values measured and used to simulated this irrigated trial were high, and may not apply to many dryland situations. Other simulations in the Australian validation data sets used the traditional wheat kl values for dryland situations.
2.4.1.1 Gatton2007early
Experiment Name
Design (Number of Treatments)
Gatton2007
Cv (2)
2.4.1.2 Gatton2007LATEsown
Experiment Name
Design (Number of Treatments)
Gatton2007Late
Cv (2)
The late sown experiment at Gatton in 2007 was comprised of observation plots of the two varieties used in the early sowing date. 4 plants were monitored for phenology (Leaf Appearance Rate and Zadoks stage) from each observation plot.
2.4.2 Wellcamp2006
This experiment was run at DPI Wellcamp Research Station, Queensland in 2006.
Phenology data (Zadok stage) was the only data collected.
This experiment was a phenology experiment based on small and narrow observation plots (4 rows) in conjunction with a QDAF Oats breeing trial. The decision was taken to use a nearby SILO met-data set rather than the onsite weather station, due to the possibility that the onsite met station had not been calibrated for some time.
Experiment Name
Design (Number of Treatments)
Wellcamp2006
Cv x Cut (15)
2.4.3 Roma2006
This experiment was run at Roma Research Station, Queensland in 2006.
Two reps were used to generate cut and regrowth data (two cuts taken).
The third rep was split in two, and one half was used to allow unlimited growth (no cutting), and the other half was only cut once. Soil water data for the ungrazed treatment was sourced exclusively from rep 3 except for the sowing PAW which was measured bulked across the experiment.
Inaccuracy in the prediction of SLA and LAI was probably due to measurement error. The trial was heavily water stressed and it was difficult to accurately measure leaf area of curled, drought stress leaves, particularly when they were stored overnight.
Experiment Name
Design (Number of Treatments)
Roma2006
Cv x Cut (9)
2.4.4 Tarlee2007
This experiment was run at Tarlee, South Australia, in 2007. Leaf disease (probably yellow spot) was observed to take hold in the latter stages of the experiment that may explain differences between observed and simulated LAI, leaf biomass and dead leaf biomass toward the end of the experiment.
Experiment Name
Design (Number of Treatments)
Tarlee2007
AgRegime (2)
2.4.5 LRCphenology
These simulations were used to develop phenology parameters for a range of cultivars used in northern Australia. Observed data was kindly supplied by Bruce Winter (Oats Breeder) and Richard Uebergang from experiments conducted at DAFQ Leslie Research Centre, Holberton St Toowoomba. SILO weather data from the nearby Toowoomba Airport was used. Approximated irrigation data and fertiliser regime was provided by LRC staff who managed the phenology screening trials to be non-water and N limiting.
Experiment Name
Design (Number of Treatments)
LRC2011
Cv x Management (12)
LRC2012
Cv x Management (15)
LRC2014
Cv x Management (14)
LRC2015
Cv x Management (21)
LRC2016
Cv x Management (21)
LRC2017
Cv x Management (21)
3 Sensibility
Experiment Name
Design (Number of Treatments)
OatsExampleTarlee2007
AgRegime (2)
SLMACC_Southland_2021_Pop
Sow (10)
Lysimeter experiment
Trial location: Wallacetown, Southland
Coordinates: 46.311°S, 168.311°E
Soil: Waikiwi
Weather station: Invercargill Aero 2 Ews
Urine (200 kg N/ha) and non-urine comparison
Shallow (10 cm) and deep (25 cm) cultivation
Oats sown in July or September
Measurements = Soil properties, N & P leaching, biomass, N uptake, soil N (taken at the end of trial)
The paddock was previously in long-term pasture (10yrs+)
Texture
Depth		% sand		% silt		% clay
0-10 cm		3.4		68.5		28.2
10-20 cm	3.4		70.7		25.9
20-30 cm	4.4		68.5		27.1
30-40 cm	4.2		69.5		26.3
40-50 cm	3.9		70.8		25.3
50-60 cm	3.3		71.2		25.4
60-70 cm	6.8		66.9		26.3
4 Interface
4.1 Oats
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
PlantType
String
True
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
BA Keating, Commonwealth Scientific, H Meinke, ME Probert, NI Huth, IG Hills, 2001. NWheat: documentation and performance of a wheat module for APSIM. CSIRO Division of Agriculture Technical Memorandum, CSIRO Division of Tropical Agriculture, Brisbane.
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Jamieson, P. D., Brooking, I. R., Porter, J. R., Wilson, D. R., 1995. Prediction of leaf appearance in wheat: a question of temperature. Field Crops Research 41 (1), 35-44.
Lawless, Conor, Semenov, MA, Jamieson, PD, 2005. A wheat canopy model linking leaf area and phenology. European Journal of Agronomy 22 (1), 19-32.
Malcolm, B, Teixeira, E, Johnstone, P, Maley, S, de Ruiter, J, Chakwizira, E, 2016. Catch crops after winter grazing for production and environmental benefits. Agronomy New Zealand 46, 99-108.
Martin, RJ, Jamieson, PD, Gillespie, RN, Maley, S, 2001. Effect of timing and intensity of drought on the yield of oats (Avena sativa L.). Screening 1 (5.0), 7-90.
Peake, A., Whitbread, A., Davoren, B., Braun, J., Limpus, S., 2008. The development of a model in APSIM for the simulation of grazing oats and oaten hay. In: Global Issues, Paddock Action. Proceedings of the 14th ASA Conference, 21-25 September 2008, Adelaide, South Australia., Eds: Unkovich, M..
S Asseng, A Bar-Tal, J.W Bowden, B.A Keating, A Van Herwaarden, J.A Palta, N.I Huth, M.E Probert, 2002. Simulation of grain protein content with APSIM-Nwheat . European Journal of Agronomy  16 (1), 25 - 42.
S Asseng, B.A Keating, I.R.P Fillery, P.J Gregory, J.W Bowden, N.C Turner, J.A Palta, D.G Abrecht, 1998. Performance of the APSIM-wheat model in Western Australia . Field Crops Research  57 (2), 163 - 179.
Wang, E., Robertson, M. J., Hammer, G. L., Carberry, P. S., Holzworth, D., Meinke, H., Chapman, S. C., Hargreaves, J. N. G., Huth, N. I., McLean, G., 2002. Development of a generic crop model template in the cropping system model APSIM. European Journal of Agronomy 18 (1-2), 121-140.
