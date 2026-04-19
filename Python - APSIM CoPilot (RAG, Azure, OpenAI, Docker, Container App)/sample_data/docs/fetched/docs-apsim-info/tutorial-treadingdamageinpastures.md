# TreadingDamageInPastures documentation

Source URL: https://docs.apsim.info/tutorial/TreadingDamageInPastures
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:54:25.430541+00:00

1 TreadingDamageInPastures Tutorial
Enabling Livestock Treading Effects on Soil and Pasture
Val Snow and Mike Dodd (New Zealand Bioeconomy Science Institute) and Dean Holzworth (CSIRO, Australia)
(Last updated 30 October 2025)
The intention is that this documentation is read alongside the simulation that produced it to see the detail of the examples. The simulation is to be found under the "Examples" button, then select the “Tutorials” folder and open “TreadingDamageInPastures.apsimx”.
Overview
Treading damage is defined as the (negative) effect of stock hooves on soil properties and pasture growth. Some of the key features of treading damage are that:
treading damage compacts the soil mainly affecting the macropores (the soil pores that hold the water between DUL and Saturation) by reducing their prevalence in the soil;
the reduced macroporosity reduces the aeration (air content) at DUL and also reduces the saturated hydraulic conductivity;
the heavier the stock, the greater the number of stock, and the longer the duration of grazing, the greater the damage; and
if the soil is drier than DUL, there is negligible treading damage but as the soil gets wetter it becomes more susceptible to damage.
These changes in the soil properties indirectly affect pasture growth through lack of aeration to the roots, and various effects of changed soil properties on C and N cycling.
Treading damage also affects the pasture directly through direct damage to growing points and burial of biomass (including growing points. These effects recover over time - generally in a matter of a few months unless the treading damage is catastrophic (e.g., heavy pugging).
These effects can be captured in APSIM using the
TreadingDamage
manager - found in the
Management Toolbox
under the
Pasture Graze Stock
folder. This manager will allow the user to link treading damage to grazing events. Alternatively, the user can specify dates for mock grazing (stock pressure but no pasture biomass removal).
Assumptions
There are several assumptions in this damage effect. They include:
the plant above and below ground biomass is not directly altered by treading events;
damage effects are additive meaning that:
if there is a pre-existing damage effect of 0.2 and a new treading effect of 0.3 happens then the damage effect is increased to 0.5; and
already-damaged soil or pasture is not more susceptible to damage than an undamaged soil/pasture (no concept of changing soil strength is included).
TreadingDamage
does not capture the effect of catastrophic pugging to the soil and pasture such that there is a need for cultivation and resowing; and
the recovery rate implicitly assumes that the soil/pasture will be sensibly managed after a damage event so that it has the opportunity to recover.
Research background to model development
Livestock pressure on soils and pastures can be quantified with a combination of three key variables: livestock density (instantaneous animal numbers per hectare), livestock mean mass (kg liveweight per individual animal) and duration of grazing (generally a matter of hours). An additional factor is required to relate pressure to soil and sward damage (Finlayson et al., 2002). This has commonly been measured as the proportion of the grazed area affected by hoof impact (puddling, ridge/clod, skid, cleft and compaction, e.g., Sheath & Boom, 1997). The damage value is scaled to a proportion of surface area. This surface damage proportion is distinct from the proportion of bare soil, which is also present in normal undamaged swards. While there is evidence that the form is not linear, the exponent parameter is not consistent across all studies,  reflecting soil type and soil water content conditions at the time of damage.
Figure 1: Data from three studies on different soil types, of the impact of animal pressure on soil surface damage. Growth function model fitted to Sheath & Carlson (1998) and Nie et al. (2001) data
Damage affects two key soil properties:
Macroporosity
, the large pores (typically >30 µm) that are usually air-filled when soils are below DUL and only partially to fully water-filled for short periods when soil is between DUL and saturation. Treading will compress these pores and reduce the macropore space as a proportion of total porosity. The relationship between damage extent and macroporosity appears to be linear (Figure 2a) although there is clearly a lower theoretical limit of 0% macroporosity, when all macropores are eliminated. The effective conversion of macropores to mesopores will increase potentially available water (Drewry et al. 2022);
Infiltration rate
– the reduction in large pore space means that water is held more tightly to soil in smaller pores and drainage flow is reduced. A reduction in infiltration rate leads to greater ponding (on flat soils and where soil is pugged, thus creating small pools) or greater runoff (on sloping soils). The relationship between damage extent and infiltration rate can also be approximated by a linear equation (Figure 2b), which also has a theoretic limit of 0 mm/hr for a sealed soil surface.
Figure 2: The relationship between soil damage and (a) macroporosity; or (b) infiltration rate of a Pallic soil (Pande 2002) and a Gley soil (Meneer et al. 2025, Zegwaard 2006). Linear models are fitted to Zegwaard (2006) data only.
The effects of wet soil treading damage on macroporosity and infiltration rate will occur to varying depths, depending on soil properties and the severity of the damage (Drewry 2003).
Recovery of soil properties over time since a damage event is mediated by biological activity (essentially a recovery of the air-filled pore space), including new root growth and turnover and macrofaunal burrowing, such as by earthworms . Various studies have reported recovery rates for specific soil characteristics of 6 to 20 weeks (e.g., Zegwaard 2006).
The reduction in macroporosity and infiltration rates from treading damage lead to extended periods of elevated soil moisture. Waterlogging affects root growth and plant yield (e.g., up to 59% reduction in shoot biomass accumulation) primarily through the impact of decreased soil oxygen levels. Tolerance to waterlogging varies between pasture species (Di Bella et al. 2022). These effects are already incorporated in AgPasture.
Damage also affects pasture swards in two main ways (over and above associated livestock consumption):
Tissue wastage, whereby above-ground plant tissue is sheared off and contributes to plant litter, resulting in an immediate reduction in leaf area. At the same time, root tissue is also abscised as the plant puts more energy into recovering photosynthetic tissue;
Tissue damage, whereby growing points are subject to contusion, reducing tiller numbers and herbage accumulation via a reduction in leaf area index. Damage also increases the susceptibility of the plant to microbial infection, thereby increasing on-going senescence. Studies of sward basal cover (e.g., Pande 2002, Fig. 3a) and tiller populations  (e.g., Nie et al. 2001, Fig 3b) indicate linear reductions in pasture regrowth capacity.
Figure 3: The relationship between soil damage and (a) basal area; or (b) tiller populations. Linear models are fitted to the data.
Recovery of plant growth relies on both the normal post-defoliation regrowth from surviving tissue and new growing point initiation and expansion of plant basal area into the open soil surface spaces created by the treading damage. Various studies have reported recovery rates for sward characteristics and herbage accumulation rates of 7 to 23 weeks (e.g., Betteridge et al. 2003).
How
TreadingDamage
works
The effects of treading damage are coded into
TreadingDamage
which in turn interacts with
SimpleGrazing
which controls pasture harvesting, and with the
Soil Physical
model that describes the relevant properties of the soil profile (macroporosity, drained upper limit and infiltration rate). In the current implementation, the user specifies the livestock grazing pressure by specifying the instantaneous stocking density (head /ha) ), the duration of grazing (hours) and the per head liveweight (kg LW /head). On the days when there is a positive stocking density (meaning that there is either a grazing event from
SimpleGrazing
or the user has specified a damage event)
TreadingDamage
will:
calculate a water factor that rises from zero at the drained upper limit to a value of one at saturation;
calculate an effective grazing pressure by multiplying the grazing pressure by the water factor;
calculate an empirically-derived treading damage effect that rises asymptotically from zero at no effective grazing pressure to a value of 0.95 at about 4800 t LW hr /ha (see Figure 1);
calculate a damage value, separately for soil and pasture components, by adding the treading damage to any pre-existing damage;
impose the damage on the pasture components and soil (more information on this below); and
if there is no additional damage on a given day then there is a recovery of damage towards the undamaged state.
Figure 4 shows the relationship between effective grazing pressure and treading damage effect for the default parameters. For reference, 500 kg LW dairy cows mobbed up for winter grazing at 400 cows /ha for a full 24-hours would generate a grazing pressure of 4800 t LW hr/ha. If the soil was halfway between DUL and saturation then the effective grazing pressure would be 2400 t LW hr /ha and that would generate a treading effect of 0.78.
Figure 4. Relationship between Effective Grazing Pressure and Treading Damage Effect when using the default parameters.
For the pasture components, currently only relevant for species in the AgPasture template, the damage effect is imposed using a 1:1 effect on the parameter
GLFGeneric
.
Two soil properties are affected by the damage function. Saturated hydraulic conductivity is decreased when the soil is damaged. The maximum effect (damage effect = 1) is defined as a reduction to a saturated hydraulic conductivity twice that at DUL. Often a value of 1 mm /day is used for conductivity at DUL.
The second soil property affected is the macroporosity. We assume that total porosity and bulk density are unchanged so the damage effect is imposed by increasing DUL towards saturation with the maximum effect leaving only 0.02 between saturation and DUL. This method is consistent with a transfer of some of the larger pores into a greater number of smaller pores. A side-effect is that the plant-available water (DUL-LL15) will also increase.
The soil damage function is imposed with depth in the soil with a constant effect to a specified depth with a curvilinear decline in the effect until it is less than 5% of the surface damage at a second specified depth. Values of 100 and 250 mm are often suitable.
In the absence of clear data on the temporal pattern of plant and soil recovery in the literature, this was implemented in
TreadingDamage
via a first-order recovery rate. The default parameter value is 0.015 and that equates to recovery within a 6-month time frame. At a threshold of damage value <0.05 the damage value is set to zero.  Separate recovery rates for the pasture species and soil are allowed for in the user interface.
Explanatory scenarios
Two series of simulations with (a) varying soil moisture and (b) grazing pressure were run to demonstrate the working of
TreadingDamage
. Both of these utilised the defined damage day option and were applied onto a pasture set up for a constant growth rate (the same weather each day, daily additions of fertiliser, pasture harvesting every day to a residual of 2500 kg DM /ha) so that the treading effects could best be illustrated.
In the first series of simulations the grazing pressure is zero except for day 15 when it was set to 1200 t LW hr /ha (100 cows /ha for 24 hours, 500 kg LW /head). The water content of the soil was constant throughout an individual simulation but varied in the first series of simulations by changing the net precipitation input, which ranged from 5 to 10 mm /day.
Figure 5. shows how the same grazing pressure results in differing effective grazing pressure and pasture growth depending on the soil water content at the time of the damaging event. At the lowest net precipitation, the water factor does not rise above 0.0. This means that the effective grazing pressure is also 0.0 so there is no effect on soil properties or pasture growth. At higher values of net precipitation, the water factor rises, effective grazing pressure also rises (to a maximum value shown of 403 t LW hr /ha) and the effect on soil properties (not directly shown) and pasture growth is greater as the recovery time slows.
Figure 5. Explanatory diagram showing a single damage event affecting several intermediate variables and pasture growth as affected by varying soil water content.
In the second series of simulations the soil moisture level was constant (the 10 mm net precipitation from above) but grazing pressure varied from 0 to 1200 t LW hr /ha by changing the cow liveweight. Figure 6 shows how increasing grazing pressure for the same soil water content increases the treading effect. Note that although the soil water content was largely constant throughout the simulation, the water factor varied. This is because the treading event on day 15 ‘moved’ some of the pore space between DUL and SAT (macropores) to below DUL (by increasing DUL) so increasing the mesopore space. The follow-on effect of this is that the water factor also changes even though soil water content was constant. Pasture growth is more severely affected at the higher grazing pressure and the recovery time is extended.
Figure 6. Explanatory diagram showing a single damage event affecting several intermediate variables and pasture growth as affected by varying grazing pressure under a non-varying soil water content.
Example simulation
An example simulation is also supplied where there is the potential for treading damage at each grazing event. “GrazingExample” in the tutorial file shows the annual accumulation of harvested pasture with and without
TreadingDamage
enabled. For some events, the damage value is increased, as indicated by a greater differential in harvested biomass, while others show no additional damage (Figure 7).
Figure 7. Annual accumulation of pasture harvested with
TreadingDamage
disabled (black) or enabled (ochre).
Acknowledgements
Funding was provided by the New Zealand Ministry for Business Innovation and Employment (MBIE) via the SSIF Future Coasts programme of NIWA (now Earth Science New Zealand) and via the SSIF Maximising Impact programme of AgResearch (now the New Zealand Bioeconomy Science Institute).
References
Betteridge, K., Drewry, J., Mackay, A., & Singleton, P. (2003). Managing treading damage on dairy and beef farms in New Zealand. AgResearch Ltd.
Di Bella, C. E., Grimoldi, A. A., & Striker, G. G. (2022). A quantitative revision of the waterlogging tolerance of perennial forage grasses. Crop and Pasture Science, 73(10), 1200-1212.
https://doi.org/https://doi.org/10.1071/CP21707
Dodd MB, Snow VO, Triadis D (2025) Incorporating treading damage on wet soils in APSIM. MODSIM 2025
Drewry, J. J. (2003). Dairy grazing strategies to minimise soil pugging and compaction in the Waikato. In Proceedings of the New Zealand Grassland Association (Vol. 65, pp. 99-103). New Zealand Grassland Association.
Drewry JJ, Carrick S, Mesman NM, Almond P, Muller K, Shanhun FL & and Chau, H. (2022) The effect of irrigated land-use intensification on the topsoil physical properties of a pastoral silt loam. New Zealand Journal of Agricultural Research, 65(6), 561-572.
https://doi.org/10.1080/00288233.2021.1905670
Finlayson, J. D., Betteridge, K., MacKay, A., Thorrold, B., Singleton, P., & Costall, D. A. (2002). A simulation model of the effects of cattle treading on pasture production on North Island, New Zealand, hill land. New Zealand Journal of Agricultural Research, 45(4), 255-272.
https://doi.org/10.1080/00288233.2002.9513516
Meneer, J. C., Ledgard, S. F., McLay, C. D. A., & Silvester, W. B. (2005). The effects of treading by dairy cows during wet soil conditions on white clover productivity, growth and morphology in a white clover-perennial ryegrass pasture. Grass & Forage Science, 60, 46-58.
Nie, Z. N., Ward, G. N., & Michael, A. T. (2001). Impact of pugging by dairy cows on pastures and indicators of pugging damage to pasture soil in south-western Victoria. Australian Journal of Agricultural Research, 52(1), 37-43.
https://doi.org/https://doi.org/10.1071/AR00063
Pande, T. N. (2002). Pasture dynamics under cattle treading [PhD Thesis, Massey University]. Palmerston North, New Zealand.
https://mro.massey.ac.nz/server/api/core/bitstreams/3f24f24b-4f3f-46d2-acb7-d624f87e107a/content
Sheath, G. W., & Boom, C. J. (1997). Impact of beef cattle grazing systems on treading damage and forage supply. Proceedings of the New Zealand Grassland Association, 59, 87-92.
https://doi.org/https://doi.org/10.33584/jnzg.1997.59.2271
Sheath, G. W., & Carlson, W. T. (1998). Impact of cattle treading on hill land: 1. Soil damage patterns and pasture status. New Zealand Journal of Agricultural Research, 42(2), 271-278.
https://doi.org/https://doi.org/10.1080/00288233.1998.9513311
Snow VO, Dodd MB, Nichols S, Triadis D (2025) Quantifying the impacts of higher water tables on pasture production. Report for NIWA, Contract CO1X2107. Client Report Number RE450/2025/008
Zegwaard, K. E. (2006). Effects of severe cattle treading on soil physical properties and pasture productivity The University of Waikato]. Hamilton, New Zealand.
https://hdl.handle.net/10289/12707
