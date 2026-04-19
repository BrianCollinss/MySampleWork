# Eucalyptus documentation

Source URL: https://docs.apsim.info/validation/Eucalyptus
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:51:52.851529+00:00

1 The APSIM Eucalyptus Model
The model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
. This
new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a
model in much the same way that models can be coupled to construct a simulation.This means that dynamic composition
of lower level process and organ classes(e.g.photosynthesis, leaf) into larger constructions(e.g.maize, wheat,
sorghum) can be achieved by the model developer without additional coding.
Eucalyptus Model Notes
Plant Modelling Framework
The APSIM Eucalyptus model has been developed using the Plant Modelling Framework (PMF) of
Brown et al., 2014
within  APSIM Next Generation
Holzworth et al., 2014
. This new framework provides a library of plant organ and process submodels that can be coupled, at runtime, to construct a model in much the same way that models can be coupled to construct a simulation. This means that dynamic composition of lower level processes and organ classes (e.g. photosynthesis, leaf) into larger constructions (e.g. maize, wheat, eucalyptus) can be achieved by the model developer without additional coding.
###Pecularities of Eucalyptus and This Model
The Eucalyptus model consists of:
a phenology model to simulate development through sequential growth phases
a collection of organs to simulate the various plant parts
an arbitrator to allocate resources (N, biomass) to the various plant organs
This work builds upon earlier APSIM forest models such as described by
Huth et al., 2002
,
Huth et al., 2001
and
Huth et al., 2008
.
Eucalyptus is a reasonably straight forward perennial crop to model. This model has been set up for simulation of even-aged plantations (transplanted seedlings) or native forests (also assumed to start the simulation as a transplanted seedling, but in practice it would be sown from naturally distributed seed). Plants grow in accordance with available resources and conditions, which in this version of the model are temperature, radiation, available soil water, and available soil nitrogen. Leaves, branches and roots senesce, remain attached for some time, then detach to produce litter. Above-ground biomass is the main target of production, which is made up of organs that develop from default partitioniing targets that are modified daily in response to organ demand. Forest managers also deal with tree size, which are set in the model as empricial functions of aboveground biomass. During model development, we found that Eucalyptus model performance (plant or stand development) was particularly sensitive to leaf lifespan/longevity, specific leaf area, dead leaf detachment, partitioning to roots and shoots, mortality and thinning, and weeds (if present).
After stem dry weight is determined, it is then empirically split into bark and wood (based on individual stem weight), and the volumes of each set using bark thickness. This enables under and over bark properties to be calculated for BA, Vol and MAI, and finally a the calculation of wood and bark densities. Volume is calculated as a stand rather than summing individual trees. Many of these attributes of stem metrics are highly site, genotype and management specific, and some forestry plantation companies keep there own parameterisations confidential.
There are many Eucalyptus genotypes (species, closely related genera, provenances, families, clones, and hybrids) that can behave differently in response to their growing environment. The Eucalyptus model was calbirated on datasets of species (E. globulus, E. grandis, E. nitens, E. saligna), hybrids (E. grandis x E. urophylla, E. globulus x E. urophylla), and two clones of the E. grandis x E. urophylla hybrid.
###Including a Eucalyptus crop in an APSIM simulation
A example Eucalyptus simulation is available by clicking the "Open an Example" tab available when APSIM Next Gen is opened.  This provides a demonstration of how to simulate a Eucalyptus crop, and it provides some useful graphs as suggestins for viewing model behaviour and performance.
To incluce a Eucalyptus crop in a simulation the "Eucalyptus" model needs to be added to the paddock, field or zone in which it is to be grown.  This can be done by (a) right clicking on the "Paddock", selecting "Add model..." then "PMF", then selecting "Eucalyptus" from the list that comes up, or (b) copying and pasting the model from the example simulation.  A TreeSowingRule needs to be set up to start the crop. Harvesting and replanting are included in the 'EucalyptusRotation' example.
This document provides a more detailed description of the model, describes the validation and test datasets, and model performance.
Major Eucalyptus model developments:
2017-2019
Developed and released the first version of the Eucalptus model in APSIM Next Generation. That verison was based on Australian and Brazilian datasets covering tropical and sub-tropical
genotypes - mainly E. grandis. Publications include
Smethurst et al., 2020
and
Elli et al., 2020
.
2020-2022
Included temperate species, i.e. E. globulus and E. nitens.
Included an expansion of stem metrics beyond just diameter at breast height (DBH, cm), height (m), and overbark stem volume (Vol, m
3
/ha). New stem metrics include underbark parameters of stem volume (Volub) and wood density, and basal area (BA, m
2
/ha) the mean annual increment of overbark and underbark volumes (m
3
/ha/year).This required partitioning of stem biomass into bark and wood, calculation of bark thickness, and an estimation of volumes overbark and underbark. All stem metrics are empricially calculated rather than process-based. These metrics are known to be 	highly effected by stem taper, bark thickness and wood density, which in-turn are highly influenced by site, genetics and management. Few data are available on wood density (underbark, whole tree), so it is included here only as a check that underlying calculations are sensible. Validations of these metrics are included. In comparison, it remains that only DBH, height and overbark stem volume are validated for the tropical and sub-tropical genotypes.
Suggested future developments:
Create a set of functional weeds specifically for use with these forestry models, e.g. N-fixing/non-N-fixing X herbaceous/shrub/tree X tropical/sub-tropical/temperate.
Coppicing
Self-thinning rule or process-based mortality
Improve effects of stocking, if necessary. Leaf allocation as a function of aboveground.wt (g/m
2
) rather than individual tree weight (g/tree) has been included, but further checking of this is required to see that if that is all that is needed for a wide range of stockings.
Waterlogging – I (Philip) would have thought it was important, particularly for some euc and pine genotypes, but so far I haven’t run into a really need for it amongst our current observed datasets.
Add observed data for the tropical and sub-tropical genotypes for the more advanced stem metrics, and recalibrate the model for those genotypes if necessary. This would be a good postgrad project for a Brazilian student with industry collaborators.
Soil P (and K) and fertiliser responses
Pruning and effects on knot-free wood (wood quality)
Geo-locate and interact adjacent plots for predicting area-based metrics like stream flow and wood production
Tree and log size class distributions
Development of outputs for greenhouse accounting (water use, C sequestration, greenhouse gases, biodiversity idiexes)
Root:shoot ratio, and allometric relationships for height (Ht, m), stem diameter (DBH, cm, over bark at 1.3 m height), and their derivatives (stem volume Vol, and mean annual increment MAI) were developed as a function of above-ground biomass from
Almeida, 2003
,
Almeida et al., 2004
,
Borges, 2009
,
Cromer et al., 1993
, and
Nogueira, 2005
. Similarly, above-ground biomass as a function of stem weight or wood weight was developed from the same datasets plus
Turner, 1986
,
Byrne, 1989
,
Bradstock, 1981
,
Polglase et al., 1995
,
Snow et al., 1999
,
Snow et al., 1999
,
Myers et al., 1996
,
Myers et al., 1998
, and
Melo et al., 2015
.
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
StemAndBranch
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
grandis
grandis
grandisCoffsHarbour
grandisCoffsHarbour
grandisC15
grandisC15
grandisC22
grandisC22
grandisXurophylla
grandisXurophylla
grandisXurophyllaC3334
grandisXurophyllaC3334
grandisXurophyllaC3336
grandisXurophyllaC3336
urophyllaXglobulus
urophyllaXglobulus
BrazilPlasticClone
BrazilPlasticClone
BrazilTropicalClone
BrazilTropicalClone
BrazilSubTropicalClone
BrazilSubTropicalClone
saligna
saligna
nitens
nitens
nitensLewisham
nitensLewisham
globulus
globulus
globulusShepparton
globulusShepparton
WABlueGum
WABlueGum
FSABlueGum
FSABlueGum
1.4 Child Components
1.4.1 Age
Accumulates a child function between a start and end stage.
1.4.2 MortalityRate
A constant function (name=value)
1.4.3 SeedMortalityRate
A constant function (name=value)
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
Note that this  property does not include branches. It is used in calculating DM Demands of several plant components.
1.4.7 IndividualTreeStemWt
A class that divides all child functions.
Returns zero if nominator is zero, returns double.maxValue if denominator is zero.
1.4.8 Leaf
This organ is parameterised using a simple leaf organ type which provides the core functions of intercepting radiation, providing a photosynthesis supply and a transpiration demand.  It also calculates the growth, senescence and detachment of leaves.
1.4.9 Branch
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
For many of the parameters in this organ, see comments for similar parameters in the leaf organ.
1.4.10 Stem
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
For many of the parameters in this organ, see comments for similar parameters in the leaf organ.
1.4.11 CoarseRoot
This organ is simulated using a GenericOrgan type.  It is parameterised to calculate the growth, senescence, and detachment of any organ that does not have specific functions.
For many of the parameters in this organ, see comments for similar parameters in the leaf organ.
1.4.12 FineRoot
The root model calculates root growth in terms of rooting depth, biomass accumulation and subsequent root length density in each soil layer.
For many of the parameters in this organ, see comments for similar parameters in the leaf organ.
1.4.13 RootShootRatio
A class that divides all child functions.
Returns zero if nominator is zero, returns double.maxValue if denominator is zero.
2 Validation
Validation datasets have been included to assist with validation during model development. Validation datasets cover a range of environmental (soil and climate) conditions, management options (populations, nitrogen rates, irrigation) and genetic backgrounds (different regions, provenence, clones).  These datasets have been grouped and ordered alphebetically by site within a climatic zone.  Graphs of model performance are provided for stocking, canopy development, biomass production, stem metrics, and soil water. Where a tropical or sub-tropical dataset did not include aboveground biomass, but instead a related parameter like stem volume or biomass, the latter was converted to aboveground biomass by a regression based on the rest of the validation dataset for that climatic region. However, this estimation was not conducted for temperate datasets.
Observed data are shown compared to predictions, with statistics for model skill.
2.1 Combined Validation
These graphs are for the combined datasets of
Tropical and SubTropical
and
Temperate
climatic zones.
Graphs from individual sites, particularly temporal trends of observed and predicted values, are available but
currently disabled. If you wish to view these graphs, please download the validation from GitHub, run it, and
enable those graphs and-or add others.
2.2 Tropical and SubTropical
2.2.1 Tropical and SubTropical Validation
These graphs are for the
Tropical and SubTropical
datasets.
2.2.2 Australia Egrandis
2.2.2.1 CoffsHarbour
Data are from
Turner, 1986
. The research is also reported in
Byrne, 1989
and
Bradstock, 1981
. These data are from a chronosequence of approximately even-aged stands of native forests of
E grandis
and include the oldest stands simulated so far by the APSIM Eucalyptus model. They are the only native forest data in this set of simulations; all other simulations are of plantations.
2.2.2.2 Gympie
This experiment is described in
Cromer et al., 1993
and
Cromer et al., 1993
. Some soil input data are from
Ross, 1991
. Experimental treatments were factorial combinations of two levels each of irrigation and fertilisation applied to
E. grandis.
Most growth response was to fertiliser, which included NPK, but only N is simulated, which assumes that other nutrients were present at adequate levels.
Experiment Name
Design (Number of Treatments)
Gympie
Treatment (4)
2.2.2.3 Wagga
This experiment is described in
Polglase et al., 1995
,
Snow et al., 1999
,
Snow et al., 1999
,
Myers et al., 1996
, and
Myers et al., 1998
. The experiment included effluent-irrigated
E. grandis
and some weeds; only the irrigated treatement was included here.
2.2.3 Brazil Egrandis and others
2.2.3.1 Aracruz
These data are described in
Almeida, 2003
and
Almeida et al., 2004
. Two clones of
E. grandis
are included that had measured differences in root:shoot ratio. These differences were simulated by specifiying partitioning targets as a genetic property.
Experiment Name
Design (Number of Treatments)
Aracruz
Cult (2)
2.2.3.2 Curvelo
These data are described in
Borges, 2009
. Two clones of the hybrid E. grandis x E. urophylla were included (C3334, C3336), but their measured differences were not significantly different. Hence, clonal differences were not simulated by specifiying partitioning targets as a genetic property.
Experiment Name
Design (Number of Treatments)
Curvelo
Site (2)
2.2.3.3 Itacambira
These data are described in
Borges, 2009
. Two clones of the hybrid E. grandis x E. urophylla were included (C3334, C3336), but their measured differences were not significantly different. Hence, clonal differences were not simulated by specifiying partitioning targets as a genetic property.
Experiment Name
Design (Number of Treatments)
Itacambira
Site (2)
2.2.3.4 Luisantonio
These data are described in
Melo et al., 2015
. One clone of the hybrid
E. grandis x E. urophylla
was grown with 4 rates of N fertilisation.
Experiment Name
Design (Number of Treatments)
Luisantonio
N (4)
2.2.3.5 Mogiguacu
These data are described in
Melo et al., 2015
. One clone of the hybrid
E. grandis x E. urophylla
was grown with 4 rates of N fertilisation.
Experiment Name
Design (Number of Treatments)
Mogiguacu
N (4)
2.2.3.6 MonteDourado
These data are described in
Silva, 2006
. One clone of the hybrid
E. grandis x E. urophylla
was included, and it was grown at two sites on contrasting soils.
2.2.3.7 Paulistania
These data are described in
Melo et al., 2015
. One clone of the hybrid
E. grandis x E. urophylla
was grown with 4 rates of N fertilisation.
Experiment Name
Design (Number of Treatments)
Paulistania
N (4)
2.2.3.8 Ribasdoriopardo
These data are described in
Melo et al., 2015
. One clone of the hybrid
E. grandis x E. urophylla
was grown with 4 rates of N fertilisation.
Experiment Name
Design (Number of Treatments)
Ribasdoriopardo
N (4)
2.2.3.9 SantanadoParaiso
These data are described in
Nogueira, 2005
. One clone of the hybrid
E. grandis
was included, and it was grown in a factorial experiment of two levels each of irrigation and fertilisation.
Experiment Name
Design (Number of Treatments)
SantanadoParaiso
Irr x N (4)
2.3 Temperate
2.3.1 DroughtRiskSites
These experiments are described in
Mendham et al., 2011
,
White et al., 1996
,
White et al., 1998
,
White et al., 2009
,
White et al., 2010
and
White et al., 2014
. Experimental treatments were combinations of stocking and N fertilisation starting 2 years after planting. Additional data were provided by D. Mendham. A second rotation is described in these papers and data included in the observed file, but treatments were reallocated at the begining of the second rotation, and coppice rather than seedlings were used in most treatments for the second rotation. It would be useful and possible to include coppicing in the Eucalytpus model, but this has not yet been attempted. Other models in APSIM provide a basis for including coppicing, e.g. gliricidia, and lucerne.
Experiment Name
Design (Number of Treatments)
ScottRiver
N (4)
Wellstead
N (4)
BoyupBrook
N (4)
2.3.2 Forico
These data describe adjacent plantations of E. globulus and E. nitens, with part of the E. nitens plantation being thinned. The site was known as St Georges Road. We thank G. Holz, K. Joyce, and L. Cannon of Forico for data and other information about the site (formally the site was owned or managed by Gunns, North Forest Products and APPM).
Experiment Name
Design (Number of Treatments)
Forico
StGRd (3)
2.3.3 FSAGrowthPlots
These data are for Forestry SA growth plots, and were provided Jim O'Hehir, University of South Australia. As they are in a region with a water table containing nitrate that can be reached by roots, these components were added to the simulation. However, nitrate was not described as a concentration in groundwater, but instead nitrate was applied as nitrate fertiliser at 2.5 m depth or greater four times per year. Thinned and unthinned stands are included.
Experiment Name
Design (Number of Treatments)
FSAGPs
A (4)
2.3.4 Furadouro
This experiment is described in
Madeira et al., 1990
,
Madeira et al., 1995
,
Pereira et al., 2012
,
Madeira et al., 2002
,
Fabi~ao et al., 1995
,
K\atterer et al., 1995
,
Quilh'o et al., 2001
,
Pereira et al., 1994
,
Pereira et al., 1989
, and
Fontes et al., 2006
. Experimental treatments were combinations of irrigation and fertilisation applied to
E. globulus.
Most growth response was to fertiliser, which included NPK, but only the IL treatment was simulated here, which assumes that nutrients othere than N were also present at adequate levels.
Experiment Name
Design (Number of Treatments)
Furadour
o (1)
2.3.5 Lewisham
This experiment is described in
White et al., 1998
,
White et al., 1996
, and
Worledge et al., 1998
. The experiment included a comparison of
E. globulus
and
E. nitens
under supplemented-rainfed and well-irrigated conditions. Rainfed plots were up-slope of the irrigated plots. Soil was mostly derived from basalt, which was present at a shallow depth.
Experiment Name
Design (Number of Treatments)
Lewisham
E (2)
2.3.6 Shepparton
This experiment is described in
Bren et al., 1993
,
Baker, 1998
,
Baker et al., 2005
,
Duncan et al., 1998
,
Wong et al., 2000
,
Hopmans et al., 1990
, and
Stewart et al., 1990
. Additional data and information were provided by T.G. Baker and H. Stewart. The experiment included coppiced and seedling
E. grandis
and
E. globulus
, which were irrigated with sewerage effluent. The soil was a duplex, and there for poorly drained. Eearly growth was quite impressive, but by 10 years trees had noticably mortality due to pests and diseases, and also due to other conditions that did not suit these species (poor drainage, frost).
Experiment Name
Design (Number of Treatments)
Shepparton
E (2)
2.3.7 Westfield
This experiment is described in
Smethurst et al., 1997
,
Smethurst et al., 2003
,
Smethurst et al., 2004
,
Smethurst et al., 2004
,
Misra et al., 1998
,
Misra et al., 1998
, and
Resh et al., 2003
. The experiment included nil to high cumulative rates of N and P fertilisers in an
E. nitens
plantation. Other research suggested that three was little or no response to the P component of the fertiliser. Very high rates of fertiliser might have started to induced a base cation deficiency (Ca, Mg or K) by the latter stage of the rotation, as some acidification had occurred, but this was not investigated further.
Experiment Name
Design (Number of Treatments)
Westfield
T (6)
3 Sensibility
A series of sensibility tests have been employed to test the behaviour of the model in regions not explicitly included in the previous test set.  Furthermore, these tests explore the emergent behaviour of the model under a range of changing climate, fertility and management scenarios to ensure that simulated patterns agree with expected behaviours.
3.1 MAI in SE Australia
Representative growth rates for Eucalyptus grandis have been published for south-eastern Australia (Victoria and South Australia) by [Wong2000forecasting].  Some of the sites within this publication had previously been used for improved pasture and had minimual fertility constraints.  Simulations for 4 sites have been presented here to capture a range of environmental conditions.  Stocking rates used at each site match those obtained at each site within the published study.  MAI at age 10 years should be approximately 10 cubic metres per annum for Mount Worth and Stockdale and approximately 20 cubic metres per annum for Tostaree and Mount Lofty.  Climate data has been taken from nearby towns and common soil properties have been used for all sites, with soil properties reflecting a relatively high state of fertility.
3.2 Response to Rainfall
Eucalyptus should respond to changes in rainfall such that peak MAI should increase with rainfall.  Long term "climax LAI" should also increase with mean annual rainfall.  This simulation experiment explores the changes in MAI and canopy cover along a rainfall gradient within SE Queensland Australia.  Mean annual rainfall decreases from approximately 1200 mm to 660 mm.  Fertiliser is applied within the simulations to remove any confounding of results due to site fertility.  Data from
Specht, 1972
show that canopy cover should be almost complete for the wetter sites in this study, and decrease to approximately 50% at the drier sites.
Experiment Name
Design (Number of Treatments)
Climate
Site (5)
3.3 Response to Soil Fertility
Site fertility is an important driver of the pattern of tree growth rates.  As site fertility declines, the long term growth rate (e.g. MAI) should also decrease, but the time to obtaining peak MAI should increase.  This sensibility test uses a single location in SE Queensland at which Eucalyptus grandis occurs naturally.  A range of soil fertility states are applied in this experiment.  Peak MAI should decrease with decreasing fertility, but the time required to achieve this should increase.
Experiment Name
Design (Number of Treatments)
Fertility
Level (3)
3.4 Reponse to N Fertilizer
Eucalyptus responses to rate of fertiliser are often asymptotic, the plateau of which is determined by other limiting factors (
Rubilar et al., 2018
). In the sensibility tests presented here, soil from Wodonga, Australia, was used as the basis for the simulations, except soil organic C and C:N were set to represent site 1 in Columbia in
Albaugh et al., 2015
and
Rubilar et al., 2018
. Two climates are used in these simualtions ('WodongaI0ClimateWodonga' and 'WodongaI0ClimateCoffs'. Management was set similar to site 1 in
Rubilar et al., 2018
, i.e. E grandis was fertilised at 2 years of age and the 3-year stem volume response assessed at 5 years of age. For a highly responsive site in Columbia, a plateau in growth response occurred at about 800 kg N/ha, when it was speculated that other factors became limiting.
In 'WodongaI0ClimateWodonga', a low rainfall site (739 mm/year average longterm), the response to N was simulated to plateau at a rate  of about 800 kg N/ha. The N rate inflexion point here is similar to that in
Rubilar et al., 2018
, but the limiting factor above this N rate in this simulation was mainly water, whereas in
Albaugh et al., 2015
we can specualte that it was base cation deficiency.
In 'WodongaI0ClimateCoffs', a high rainfall site (1635 mm/year average longterm), the water limitation was removed and the response to N has not plateaued even at a rate of 3000 kg N/ha.
Values of MAI and other outputs are in the range of expectation.
Experiment Name
Design (Number of Treatments)
WodongaI0ClimateWodonga
N (8)
WodongaI0ClimateCoffs
N (8)
3.5 GXE Brazil
The graphs shown here demosntrate a genotypeXsite interaction.
Inhambupe is a dry site in NE Brazil.
Botucatu is a highly productive site in SE Brazil.
Sao Gabriel is a wet site in S Brazil.
Bocaiuve is dry site in SE Brazil.
Experiment Name
Design (Number of Treatments)
GxEBrazil
Site x Clone (12)
3.6 ScottRiverNResponse
This experiment is described in
Cromer et al., 1993
and
Cromer et al., 1993
. Some soil input data are from
Ross, 1991
. Experimental treatments were factorial combinations of two levels each of irrigation and fertilisation applied to
E. grandis.
Most growth response was to fertiliser, which included NPK, but only N is simulated, which assumes that other nutrients were present at adequate levels.
Experiment Name
Design (Number of Treatments)
ScottRiverNResponse
N (7)
4 Interface
4.1 Eucalyptus
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
Albaugh, Timothy J, Rubilar, Rafael A, Fox, Thomas R, Allen, H Lee, Urrego, John B, Zapata, Marcela, Stape, Jose L, 2015. Response of Eucalyptus grandis in Colombia to mid-rotation fertilization is dependent on site and rate but not frequency of application. Forest Ecology and Management 350, 30-39.
Almeida, AC, 2003. Application of a process-based model for predicting and explaining growth in Eucalyptus plantation..
Almeida, Auro C, Landsberg, Joe J, Sands, Peter J, 2004. Parameterisation of 3-PG model for fast-growing Eucalyptus grandis plantations. Forest Ecology and Management 193 (1-2), 179-195.
Baker, T, Duncan, M, Stackpole, D, others, 2005. Growth and silvicultural management of irrigated plantations. New forests: wood production and environmental services, 113-134.
Baker, TG, 1998. Tree growth in irrigated plantations at Wodonga and Kyabran, Victoria (1976-1994)..
Borges, JS, 2009. Parameterization calibration and validation of the 3PG model for the cerrado of Minas Gerais..
Bradstock, R, 1981. Biomass in an age series of Eucalyptus grandis plantations. Australian Forest Research 11 (2), 111-127.
Bren, L, Hopmans, P, Gill, B, Baker, T, Stackpole, D, 1993. Commercial tree-growing for land and water care. 1. Soil and Groundwater Characteristics of the Pilot Sites. Trees for Profit Research Centre, University of Melbourne, 46.
Brown, Hamish E., Huth, Neil I., Holzworth, Dean P., Teixeira, Edmar I., Zyskowski, Rob F., Hargreaves, John N. G., Moot, Derrick J., 2014. Plant Modelling Framework: Software for building and running crop models on the APSIM platform. Environmental Modelling  and  Software 62, 385-398.
Byrne, GF, 1989. Above-ground dry matter accumulation by Eucalyptus grandis and its relation to standard meteorological data. Agricultural and forest meteorology 46 (1-2), 65-73.
Cromer, RN, Cameron, DM, Rance, SJ, Ryan, PA, Brown, M, 1993. Response to nutrients in Eucalyptus grandis. 1. Biomass accumulation. Forest Ecology and Management 62 (1-4), 211-230.
Cromer, RN, Cameron, DM, Rance, SJ, Ryan, PA, Brown, M, 1993. Response to nutrients in Eucalyptus grandis. 2. Nitrogen accumulation. Forest Ecology and Management 62 (1-4), 231-243.
Duncan, M, Baker, TG, Wall, G, 1998. Wastewater irrigated tree plantations: productivity and sustainability. Proceedings of the 61st Annual Water Industry Engineers and Operators’ Conference, 18-26.
Elli, Elvis, Huth, Neil, Sentelhas, Paulo, Carneiro, Rafaela, Alcarde Alvares, Clayton, 2020. Global sensitivity-based modelling approach to identify suitable Eucalyptus traits for adaptation to climate variability and change. in silico plants 2.
Fabi~ao, Ant'onio, Madeira, Manuel, Steen, E, K\atterer, T, Ribeiro, C, Ara'ujo, C, 1995. Development of root biomass in an Eucalyptus globulus plantation under different water and nutrient regimes. Plant and Soil 168 (1), 215-223.
Fontes, Luis, Landsberg, Joe, Tom'e, Jos'e, Tom'e, Margarida, Pacheco, Carlos A, Soares, Paula, Araujo, Clara, 2006. Calibration and testing of a generalized process-based model for use in Portuguese eucalyptus plantations. Canadian Journal of Forest Research 36 (12), 3209-3221.
Holzworth, Dean P., Huth, Neil I., deVoil, Peter G., Zurcher, Eric J., Herrmann, Neville I., McLean, Greg, Chenu, Karine, van Oosterom, Erik J., Snow, Val, Murphy, Chris, Moore, Andrew D., Brown, Hamish, Whish, Jeremy P. M., Verrall, Shaun, Fainges, Justin, Bell, Lindsay W., Peake, Allan S., Poulton, Perry L., Hochman, Zvi, Thorburn, Peter J., Gaydon, Donald S., Dalgliesh, Neal P., Rodriguez, Daniel, Cox, Howard, Chapman, Scott, Doherty, Alastair, Teixeira, Edmar, Sharp, Joanna, Cichota, Rogerio, Vogeler, Iris, Li, Frank Y., Wang, Enli, Hammer, Graeme L., Robertson, Michael J., Dimes, John P., Whitbread, Anthony M., Hunt, James, van Rees, Harm, McClelland, Tim, Carberry, Peter S., Hargreaves, John N. G., MacLeod, Neil, McDonald, Cam, Harsdorf, Justin, Wedgwood, Sara, Keating, Brian A., 2014. APSIM – Evolution towards a new generation of agricultural systems simulation. Environmental Modelling  and  Software 62, 327-350.
Hopmans, P, Stewart, HTL, Flinn, DW, Hillman, TJ, 1990. Growth, biomass production and nutrient accumulation by seven tree species irrigated with municipal effluent at Wodonga, Australia. Forest Ecology and Management 30 (1-4), 203-211.
Huth, N. I., Carberry, P. S., Cocks, B., Graham, S., McGinness, H. M., O'Connell, D. A., 2008. Managing drought risk in eucalypt seedling establishment: An analysis using experiment and model. Forest Ecology and Management 255 (8-9), 3307-3317.
Huth, N. I., Snow, V. O., Keating, B. A., 2001. Integrating a forest modelling capability into an agricultural production systems modelling environment-current applications and future possibilities.., Eds: Zerger, A. and Argent, R.M., 1895-1900.
Huth, Neil Ian, Carberry, P.S., Poulton, P.L., Brennan, L.E., Keating, Brian A., 2002. A framework for simulating agroforestry options for the low rainfall areas of Australia using APSIM. European Journal of Agronomy 18, 171-185.
K\atterer, Thomas, Fabi~ao, Ant'onio, Madeira, Manuel, Ribeiro, Carlos, Steen, Eliel, 1995. Fine-root dynamics, soil moisture and soil carbon content in a Eucalyptus globulus plantation under different irrigation and fertilisation regimes. Forest Ecology and Management 74 (1-3), 1-12.
Madeira, M, Ara'ujo, MC, Pereira, JS, 1995. Effects of water and nutrient supply on amount and on nutrient concentration of litterfall and forest floor litter in Eucalyptus globulus plantations. Plant and Soil 168 (1), 287-295.
Madeira, M, Pereira, JS, 1990. Productivity, nutrient immobilization and soil chemical properties in an Eucalyptus globulus plantation under different irrigation and fertilization regimes. Water, Air, and Soil Pollution 54 (4), 621-634.
Madeira, MV, Fabiao, A, Pereira, JS, Ara'ujo, MC, Ribeiro, C, 2002. Changes in carbon stocks in Eucalyptus globulus Labill. plantations induced by different water and nutrient availability. Forest Ecology and Management 171 (1-2), 75-85.
Melo, Eduardo Aparecido Sereguin Cabral de, Gon\ccalves, Jos'e Leonardo de Moraes, Rocha, Jos'e Henrique Tertulino, Hakamada, Rodrigo Eiji, Bazani, Jos'e Henrique, Wenzel, Andrea Virginia Athayde, Arthur, Jos'e Carlos, Borges, Jarbas Silva, Malheiros, Rog'erio, Lemos, Cristiane Camargo Zani de, others, 2015. Responses of clonal eucalypt plantations to N, P and K fertilizer application in different edaphoclimatic conditions. Forests 7 (1), 2.
Mendham, DS, White, DA, Battaglia, M, McGrath, JF, Short, TM, Ogden, GN, Kinal, J, 2011. Soil water depletion and replenishment during first-and early second-rotation Eucalyptus globulus plantations with deep soil profiles. Agricultural and Forest Meteorology 151 (12), 1568-1579.
Misra, RK, Turnbull, CRA, Cromer, RN, Gibbons, AK, LaSala, AV, 1998. Below-and above-ground growth of Eucalyptus nitens in a young plantation: I. Biomass. Forest Ecology and Management 106 (2-3), 283-293.
Myers, BJ, Benyon, RG, Theiveyanathan, S, Criddle, RS, Smith, CJ, Falkiner, RA, 1998. Response of effluent-irrigated Eucalyptus grandis and Pinus radiata to salinity and vapor pressure deficits. Tree physiology 18 (8-9), 565-573.
Myers, BJ, Theiveyanathan, S, O'brien, ND, Bond, Warren J, 1996. Growth and water use of Eucalyptus grandis and Pinus radiata plantations irrigated with effluent. Tree Physiology 16 (1-2), 211-219.
Nogueira, CAS, 2005. Estado nutricional e produtividade de plantios de clones hybridos de Eucalyptus grandis x E. urophylla submetidos a diferentes niveis de adubacao no vale do Rio Jari, Pará e Amapá..
Pereira, Jo~ao S, Landsberg, Joseph John, 2012. Biomass production by fast-growing trees. 166.
Pereira, JS, Linder, S, Ara'ujo, MC, Pereira, H, Ericsson, T, Borralho, N, Leal, LC, 1989. Optimization of Biomass Production in Eucalyptus Globulus Plantations.—A Case Study. Biomass production by fast-growing trees, 101-121.
Pereira, JS, Madeira, MV, Linder, S, Ericsson, T, Tom'e, M, Ara'ujo, MC, 1994. Biomass production with optimised nutrition in Eucalyptus globulus plantations. Eucalyptus for biomass production, 13-30.
Polglase, PJ, Tompkins, D, Stewart, LG, Falkiner, RA, 1995. Mineralization and leaching of nitrogen in an effluent-irrigated pine plantation. Journal of Environmental Quality 24 (5), 911-920.
Quilh'o, Teresa, Pereira, Helena, 2001. Within and between-tree variation of bark content and wood density of Eucalyptus globulus in commercial plantations. Iawa Journal 22 (3), 255-265.
Resh, Sigrid C, Battaglia, Michael, Worledge, Dale, Ladiges, Sven, 2003. Coarse root biomass for eucalypt plantations in Tasmania, Australia: sources of variation and methods for assessment. Trees 17 (5), 389-399.
Ross, DJ, 1991. Soils at sites selected for eucalypt research in Toolara State Forest, Gympie, Queensland..
Rubilar, Rafael A, Allen, H Lee, Fox, Thomas R, Cook, Rachel L, Albaugh, Timothy J, Campoe, Ot'avio C, 2018. Advances in Silviculture of Intensively Managed Plantations. Current Forestry Reports 4 (1), 23-34.
Silva, GGC, 2006. Nutricao, crescimento a sua modelagem em provoamentos de eucalipto em resposta a disponibilidade de agua e nutrientes..
Smethurst, Philip J, Valadares, Rafael V, Huth, Neil I, Almeida, Auro C, Elli, Elvis F, Neves, J'ulio CL, 2020. Generalized model for plantation production of Eucalyptus grandis and hybrids for genotype-site-management applications. Forest Ecology and Management 469, 118164.
Smethurst, Philip, Baillie, Craig, Cherry, Maria, Holz, Greg, 2003. Fertilizer effects on LAI and growth of four Eucalyptus nitens plantations. Forest ecology and management 176 (1-3), 531-542.
Smethurst, Philip, Holz, Greg, Moroni, Martin, Baillie, Craig, 2004. Nitrogen management in Eucalyptus nitens plantations. Forest Ecology and Management 193 (1-2), 63-80.
Smethurst, PJ, Herbert, AM, Ballard, LM, 1997. A paste method for estimating concentrations of. Australian journal of soil research 35 (1), 209-225.
Smethurst, PJ, Mendham, DS, Battaglia, M, Misra, R, 2004. Simultaneous prediction of nitrogen and phosphorus dynamics in a Eucalyptus nitens plantation using linked CABALA and PCATS models..
Snow, VO, Bond, Warren J, Myers, BJ, Theiveyanathan, S, Smith, CJ, Benyon, RG, 1999. Modelling the water balance of effluent-irrigated trees. Agricultural Water Management 39 (1), 47-67.
Snow, VO, Smith, CJ, Polglase, PJ, Probert, ME, 1999. Nitrogen dynamics in a eucalypt plantation irrigated with sewage effluent or bore water. Soil Research 37 (3), 527-544.
Specht, RL, 1972. Water use by perennial evergreen plant communities in Australia and Papua New Guinea. Australian Journal of Botany 20 (3), 273-299.
Stewart, HTL, Hopmans, P, Flinn, DW, Hillman, TJ, 1990. Nutrient accumulation in trees and soil following irrigation with municipal effluent in Australia. Environmental pollution 63 (2), 155-177.
Turner, John, 1986. Organic matter accumulation in a series of Eucalyptus grandis plantations. Forest Ecology and Management 17 (2-3), 231-242.
White, DA, Beadle, CL, Worledge, D, 1996. Leaf water relations of Eucalyptus globulus ssp. globulus and E. nitens: seasonal, drought and species effects. Tree physiology 16 (5), 469-476.
White, Don, Beadle, Chris, Worledge, Dale, Honeysett, John, Cherry, Maria, 1998. The influence of drought on the relationship between leaf and conducting sapwood area in Eucalyptus globulus and Eucalyptus nitens. Trees 12 (7), 406-414.
White, Donald A, Battaglia, Micheal, Mendham, Daniel S, Crombie, D Stuart, Kinal, JOE, McGrath, John F, 2010. Observed and modelled leaf area index in Eucalyptus globulus plantations: tests of optimality and equilibrium hypotheses. Tree physiology 30 (7), 831-844.
White, Donald A, Crombie, D Stuart, Kinal, Joe, Battaglia, Michael, McGrath, John F, Mendham, Daniel S, Walker, Scott N, 2009. Managing productivity and drought risk in Eucalyptus globulus plantations in south-western Australia. Forest Ecology and Management 259 (1), 33-44.
White, Donald A, McGrath, John F, Ryan, Michael G, Battaglia, Michael, Mendham, Daniel S, Kinal, Joe, Downes, Geoffrey M, Crombie, D Stuart, Hunt, Mark E, 2014. Managing for water-use efficient wood production in Eucalyptus globulus plantations. Forest ecology and management 331, 272-280.
Wong, J., Baker, T., Duncan, M., McGuire, D., Bulman, P., 2000. Forecasting Growth of Key Agroforestry Species in South-Eastern Australia. (Publication 00/68).
Worledge, D, Honeysett, JL, White, DA, Beadle, CL, Hetherington, SJ, 1998. Scheduling irrigation in plantations of Eucalyptus globulus and E. nitens: a practical guide. TASFORESTS-HOBART- 10, 91-102.
