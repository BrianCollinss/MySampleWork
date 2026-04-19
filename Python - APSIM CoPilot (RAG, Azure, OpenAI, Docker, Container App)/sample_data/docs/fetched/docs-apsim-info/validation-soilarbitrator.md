# SoilArbitrator documentation

Source URL: https://docs.apsim.info/validation/SoilArbitrator
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:45.050839+00:00

1 The APSIM SoilArbitrator Model
The APSIM Soil Arbitrator Model
Huth, N.I., Fainges, J.L. and Holzworth, D.P.
The APSIM farming systems model has a long history of use for simulating mixed or intercropped systems.  Doing this requires methods for simulating the competition of above and below ground resources.  Above ground competition for light has been calculated within APSIM assuming a mixed turbid medium using the Beer Lambert analogue as described by
Keating et al., 1993
.  The MicroClimate
Snow et al., 2004
model now used within APSIM builds upon this by also calculating the impact of mutual shading on canopy conductance and partitions aerodynamic conductance to individual species in applying the Penman Monteith model for calculating potential crop water use.  The arbitration of below ground resources of water and nitrogen is calculated by this model.
Traditionally, below ground competition has been arbitrated using two approaches.  Firstly, the early approaches [Adiku1995Intercrop; Carberry1996Ley] used an alternating order of uptake calculation each day to ensure that different crops within a simulation did not benefit from precedence in daily orders of calculations.  Soil water simulations using the SWIM3 model
Huth et al., 2012
arbitrate individual crop uptakes as part of the simulataneous solutions of various soil water fluxes as part of its solution of the Richards' equation
Richards, 1931
.
The soil arbitrator operates via a simple integration of daily fluxes into crop root systems via a
Runge Kutta
calculation.
If Y is any soil resource, such as water or N, and U is the uptake of that resource by one or more plant root systems,
then
Y
t+1
= Y
t
U
Because U will change through the time period in complex manners depending on the number and nature of demands for that resource, we use Runge Kutta to integrate through that time period using
Y
t+1
= Y
t
+ 1/6 x (U
1
+ 2xU
2
+ 2xU
3
+ U
4
)
Where U
1
,U
2
,U
3
and U
4
are 4 estimates of the Uptake rates calculated by the crop models given a range of soil resource conditions, as follows:
U
1
= f(Y
t
),
U
2
= f(Y
t
0.5xU
1
),
U
3
= f(Y
t
0.5xU
2
),
U
4
= f(Y
t
U
3
).
So U
1
is the estimate based on the uptake rates at the beginning of the time interval, similar to a simple Euler method.
U
2
and U
3
are estimates based on the rates somewhere near the midpoint of the time interval.  U
4
is the estimate based on the rates toward the end of the time interval.
The iterative procedure allows crops to influence the uptake of other crops via various feedback mechanisms.  For example,  crops rapidly extracting water from near the surface will dry the soil in those layers, which will force deeper rooted crops to potentially extract water from lower layers. Uptakes can notionally be of either sign, and so trees providing hydraulic lift of water from water tables could potentially make this water available for uptake by mutplie understory species within the timestep.  Crops are responsible for meeting resource demand by whatever means they prefer.  And so, leguminous crops may start by taking up mineral N at the start of the day but rely on fixation later in a time period if N becomes limiting.  This will reduce competition from others and change the balance dynamically throughout the integration period.
The design has been chosen to provide the following benefits:
The approach is numerically simple and pure.
The approach does not require the use of any particular uptake equation. The uptake equation is embodied within the crop model as designed by the crop model developer and tester.
The approach will allow any number of plant species to interact.
The approach will allow for arbitration between species in any zone, but also competition between species that may demand resources from multiple zones within the simulation.
The approach will automatically arbitrate supply of N between zones, layers, and types (nitrate vs ammonium) with the preferences of all derived by the plant model code.
2 Simple Test Using Split Populations
This test compares the water and nitrogen competition for simulations where a static crop canopy with LAI of 3, rooting depth of 1m and a daily N demand of 0.15 kgN/ha is a single crop, two half-populations or three one third-populations.  In each case the total canopy size and water demand of each of the single or split populations will be the same.  If the soil arbitrator is operating correctly the overall water and nitrogen balance of each simulation should be the same.  In the case of the split populations, the water and nitrogen supplies provided by the soil arbitrator to the sub-populations should sum to the same value calculated in the simulation for the full population.
This test simulation also tests that the uptakes provided by the arbitrator are passed to the correct instances of each crop model within a multiple zone simulation.  In such a simulation there may be multiple instances of a wheat model for example.  The arbitrator must communicate such that the correct information is supplied to each instance of each particular crop model.
The simulation includes 3 fields with up to three crops in each.
Field 1
Wheat: LAI = 3 (m
2
/m
2
), N demand = 0.15 (kg/ha)
Field 2
Wheat: LAI = 1.5 (m
2
/m
2
), N demand = 0.075 (kg/ha)
Barley: LAI = 1.5 (m
2
/m
2
), N demand = 0.075 (kg/ha)
Field 3
Wheat: LAI = 1.0 (m
2
/m
2
), N demand = 0.05 (kg/ha)
Barley: LAI = 1.0 (m
2
/m
2
), N demand = 0.05 (kg/ha)
Oats:   LAI = 1.0 (m
2
/m
2
), N demand = 0.05 (kg/ha)
The crops are simple instances of a static plant model and so the crop type is only of use in referencing the various instances of the models.
3 Conceptual Tests of Multiple Crops in a Single Zone
The series of test and simulations in this section test the performance of SoilArbitrator when there are multiple plants competing for resources in a single zone.  These tests take place with either a deep or a shallow soil and in the Lincoln (NZ) environment.  The plants vary between one or more Slurps, one or more ryegrasses and then finally a ryegrass â€“ white clover sward.
The first test compares simulations with one Slurp against equivalent simulations with two Slurps (each with half the leaf area etc. as the single Slurp).  The point is to demonstrate that the SoilArbitrator can effectively aggregate the multiple Slurps to match the single.
AgPasture Deep Soil Test simulations have either two deep-rooted ryegrasses or one deep and one shallow rooted.  The simulated plant growth and sward composition are compared when under conditions with and without irrigation and fertiliser.  The exercise is repeated in AgPasture Shallow Soil Test but using a soil with substantially reduced soil water storage.
AgPasture Test Root Depth Differential repeats AgPasture Deep Soil Test but only under conditions of no irrigation or fertiliser but with a range of rooting depths of the second ryegrass.
The final test A Ryegrass and White Clover Example is more realistic in that it contains a white clover competing with a ryegrass and examines the changing sward composition as water and/or fertiliser are added to the simulation.
3.1 01 Basic Conceptual Test
This 'experiment' does a basic test of the SoilArbitrator comparing a single Slurp to two equal Slurps, both with and without irrigation and fertiliser.  Slurp is a static plant model in which LAI, water and nitrogen demand are user-set constants but in which the uptake of water and N are determined by availability and competition.
Success in this test is that there should be minimal differences in leaf weight, soil water and soil nitrogen uptake when comparing one Slurp (OneSlurp) to two equal Slurps (TwoSlurp).  The simulations are done under conditions of unlimited irrigation and fertiliser (Plus) and with no irrigation or fertiliser (Minus).  In the graphs below, the relevant outputs from the individual Slurps in TwoSlurp are added together in the Output component.
OneSlurp models are set up with photosynthesis at 6.0 g C /m2, LAI at 3.5 and N uptake at 2.5 kg N /ha /day.  TwoSlurp simulations are set up with two equal Slurps using values of 3.0 g C /m2, LAI at 1.75 and N uptake at 1.25 kg N /ha /day.  All simulations are run using the Lincoln weather data.
The first four graphs below show soil water storage, soil nitrate N storage, cumulative water uptake and cumulative N uptake for the four contrasting simulations.  As expected, there are minimal differences caused by the number of Slurp models in the simulation but there are substantial differences caused by the presence or absence of irrigation and fertiliser.
There are small differences in the soil water storage for the water-limited simulations (first graph, SoilWater) under conditions of severe water shortage.  These are likely to be due to the approximations in the Runge-Kutta method.  While it might be improved with a higher-order solution, the third plot (WaterUptake) shows that there are no persistent differences in water uptake and that the Runge-Kutta solution is likely to be sufficient for the types of simulation for which APSIM is used.
The second and fourth graphs minimal differences in N storage and N uptake.
The final two plots show the water and N uptake from the TwoSlurp simulations only giving the uptake from the individual crops.  As expected, there are no discernible differences between the crops.
The simulations in this section are very simple conceptual tests using the static Slurp model.  They show that the Runge-Kutta method in SoilArbitrator is behaving sensibly in the presence of two equal crops when they are compared to a single equivalent crop.  These were intentionally simple tests.  Slurp is a static model and will not reduce its water or N demand in response to stress (i.e. no feedback to crop performance).  The next series of tests will use a dynamic model that will include that feedback.
Experiment Name
Design (Number of Treatments)
Experiment01
Factor (4)
3.2 02 AgPasture Deep Soil Test
This next series of tests uses a dynamic (full) crop model so that any discrepancies in water or N supply to the plant will produce feedbacks into demand and subsequent uptake.  In this series of tests each simulation contains two ryegrass models.  In the Equal simulations both ryegrasses have a rooting depth of 750 mm while in the Unequal simulations Rye1 has a 750 mm root depth but Rye2 has a 150 mm root depth.  In all other respects the crops are equal.  Simulations are run with unlimited irrigation and fertiliser (IrrigFert) and without either input supplied (DryNofert).  The simulations use a soil with 132 mm of PAW to 750 mm deep in the Lincoln environment.
The first two graphs show that there is no difference in standing herbage between the ryegrasses if they have the same rooting depth.  Graphs 3 and 4 show the significant advantage that the deeper-rooting Rye1 has over Rye2 in the Unequal simulations.
The fifth graph Yield shows that there is a slight yield advantage, persistent despite inputs of irrigation or fertiliser, when the rooting depths are equal and deep.  This shows that the yield from the deeper-rooting Rye1 cannot completely compensate for the loss of yield from the shallow-rooting Rye2.  The sixth graph shows the proportion of the total yield originating from Rye1.  In the Equal simulations this sits, of course, at 0.5.  However, in the Unequal simulations the proportion of yield from Rye1 rises to 0.65 with earlier increases in this proportion when no irrigation or fertiliser is applied.
Experiment Name
Design (Number of Treatments)
Experiment02
Factor (4)
3.3 03 AgPasture Shallow Soil Test
This series of simulations is as for AgPasture Deep Soil Test but substituting a soil with a lower soil water storage - 60 mm to 750 mm deep.  The graphs of the simulations with equal ryegrasses are not repeated but the standing herbage from the equal ryegrass simulations are shown as a reference on the first two graphs.
Under conditions of no irrigation or fertiliser the differences between Rye1 and Rye2 are intensified under the shallow soil compared to the earlier simulations with the deep soil.  The proportion of the total yield from Rye1 is greater in the shallow than the deep soil but still reaches a maximum of 0.67.  The dominance of Rye1 seems limited by the couple of months (in winter) at the beginning of the simulation in which the shallow-rooted Rye2 is not much constrained by its shallow root depth.
Experiment Name
Design (Number of Treatments)
Experiment03
Factor (4)
3.4 04 AgPasture Test Root Depth Differential
This series of simulations is as for AgPasture Deep Soil Test but applying a range of rooting depths to Rye2 while leaving Rye1 at a rooting depth of 750 mm.  The simulations use a soil with 132 mm of PAW to 750 mm deep in the Lincoln environment and are not fertilised or irrigated.
Total yield (first graph) again shows the small yield penalty as the root systems become more unequal.  Against this trend the total yield (Rye1 + Rye2) when Rye2 has a 100 mm rooting depth is slightly higher (~ 50 kg DM /ha or < 1 % of the total yield) than when Rye2 has a rooting depth of 225 mm.  The most likely explanation is that of less competition effect from the shallowest-rooting Rye2.  The proportion of the total yield arising from the deeper-rooted Rye1 (second graph) ranges from 0.5 for the equal root depth scenario to 0.7 for the scenario with the shallowest Rye2.  Graphs 3 and 4 show the cumulative harvested yield from each of the grasses â€“ the patterns are sensible given the changes in rooting depth.
The final two graphs show the degree of water stress (1 is no stress) in December and January for each of the grasses.  As the root depth of Rye2 decreases, water stress in Rye1 shows a slower onset and a reduction in magnitude.  Again, these patterns meet expectations.
Experiment Name
Design (Number of Treatments)
Experiment04
Factor (5)
3.5 05 A Ryegrass and White Clover Example
The final test in the single zone set is of a typical ryegrass and white clover pasture.  The white clover has a rooting depth of 150 mm and the ryegrass has roots down to 750 mm deep.  The simulations use a soil with 132 mm of PAW to 750 mm deep in the Lincoln environment which the pasture harvested by cutting down to 1000 kg DM /ha every 21 days with all herbage removed from the paddock. The simulation is run for ten years and in three combinations: no irrigation or fertiliser, irrigation but no fertiliser, and both irrigation and fertiliser.
The first graph shows the proportion of the total harvested dry matter that was ryegrass and the next three graphs show the standing dry matter of the white clover (pink) and the total (clover plus ryegrass) sward.  In this deep soil with no irrigation or fertiliser and under a cutting regime, the grass and clover stabilise to about 35% grass and 65% clover.  The soil here allows for reasonable growth in the shallow-rooted clover and the poor nutrient status of the soil (no nutrient returns at all) gives the clover a competitive advantage.  When fertiliser is added to the dry simulation the sward becomes 90% grass (not shown).  With irrigation but no fertiliser, the sward becomes clover dominant (about 70%) but when fertilised the sward swaps to grass-dominant (about 20% clover).  These patterns make sense given the physiological characteristics of the individual plants and suggests that the Runge-Kutta method is a reasonable representation of the competitive processes in the soil.
Experiment Name
Design (Number of Treatments)
Experiment05
Factor (3)
4 Conceptual Tests of Crops with Roots Across Zones
The series of test and simulations in this section test the performance of SoilArbitrator when there are plants with roots spread across zones.  All of these tests are in a uniform soil with lots of soil water storage but no source of organic N.  The climate is a uniform or controlled environment.  The plant is a ryegrass.
The first three series demonstrate two-zone simulations with the ryegrass in Zone0 and a bare soil in Zone1.  The Zone0 ryegrass can spread a variable proportion of its roots into Zone1.
The last two simulations are each three zones and a ryegrass in each zone.  The ryegrass in the central zone can spread roots into its neighbours but the ryegrasses in the outer zones can access only their â€˜homeâ€™ zones.  The simulations start with either uniform mineral N across the zones or with increased N in Zone0.
4.1 06 Test With No Stress
The conditions for this series of simulations are:
â€¢	a controlled environment with 25 MJ /m2 solar radiation, 15 C minimum and 25 C maximum air temperature, no rainfall;
â€¢	a uniform soil with 210 mm of PAW to 600 mm deep;
â€¢	the soil has no organic matter and there is no surface organic matter;
â€¢	Zone0 has a ryegrass with roots to 600 mm deep;
â€¢	Zone 1 has no plant in the zone;
â€¢	the ryegrass is trimmed every day to maintain 2250 kg DM /ha with all herbage removed from the simulation; and
â€¢	water and fertiliser are applied every day sufficient to maintain conditions of no water or N stress.
The series of simulations add some roots from the ryegrass in Zone0 into Zone1 such that Zone1 contains 0, 0.25, 0.5, 0.75 or 1.0 of the roots in Zone0 so that the ryegrass can meet its water and N demand from some combination of the resources in the two zones.  Note that there is no canopy cover in Zone1 and therefore evaporation from the soil surface will be greater than that in Zone0.  The expectation is that as a greater proportion of the roots are in Zone1, that:
â€¢	the irrigation need in Zone0 will decrease and that in Zone 1 will increase and possibly exceed that in Zone0 when there is an even spread of roots (because of the expected additional evaporation of soil water without canopy cover in Zone1)
â€¢	the amount of fertiliser added should decrease in Zone0 and increase in Zone1 as more rots are spread into Zone1
The graphs show that these expectations are met.
Experiment Name
Design (Number of Treatments)
Experiment06
NumOfZones (5)
4.2 07 Test With Water Stress
This series of simulations are as for the â€˜No Stressâ€™ series except there is no irrigation (fertiliser remains turned on).  There is also no rainfall so this is a run-down experiment.  The expectation is that as more roots are spread into Zone1 that:
â€¢	The plant can access more water so more dry matter will be harvested and, later on when there is insufficient stored soil water, senescence will be slower;
â€¢	Water demand and water uptake will be maintained for longer;
â€¢	Water stress will be later and less severe for a while;
â€¢	There will be no N stress because fertiliser is applied as needed;
â€¢	The soil water in Zone0 will be depleted more slowly while the soil water in Zone1 will be depleted more quickly;
â€¢	Soil nitrogen will be unaffected; and
â€¢	Less fertiliser will be applied in Zone0 and more in Zone1.
These expectations are met.  It seems likely that the fluctuations in soil N storage are due to the size of each fertiliser addition (50 kg N /ha).
Experiment Name
Design (Number of Treatments)
Experiment07
NumOfZones (5)
4.3 08 Test With Nitrogen Stress
This series of simulations are as for the â€˜No Stressâ€™ series except there is no fertiliser (irrigation remains turned on).  There is also no soil organic matter or initial mineral N so this is a run-down experiment.  The expectation is that as more roots are spread into Zone1 that:
â€¢	The plant can access more N so more dry matter will be harvested and, later on when there is insufficient stored N, senescence will be slower;
â€¢	Water demand and water uptake will be maintained for longer;
â€¢	There will be no water stress;
â€¢	N stress will be delayed and less severe;
â€¢	Soil water storage will be unaffected;
â€¢	The soil N storage in Zone0 will be depleted more slowly while the soil N in Zone1 will be depleted more quickly; and
â€¢	There is likely to be a reduction in irrigation applied to Zone0 and an increase in Zone1 as more water is extracted from Zone1 but this will be moderated by the reduction in growth as N becomes severely limited.
These expectations are largely met.  There are some strange dynamics when 0.25 of the roots are in Zone1 for which there is no obvious explanation but otherwise the patterns are as expected.
Experiment Name
Design (Number of Treatments)
Experiment08
NumOfZones (5)
4.4 Conceptual Three Zone Simulation with Initial Uniform Soil N
In this simulation, which is largely based on the one above, there are three zones with a ryegrass, lots of irrigation but no fertiliser and about 35 kg mineral N in the soil at initialisation.  The roots from the ryegrass in Zone1 has roots on Zone0 and Zone2 but the ryegrasses in Zone0 and Zone2 only have roots in their own zone.  As expected:
â€¢	the ryegrass in Zone1 grows more (note that Zone0 and Zone2 are mostly plotting on top of each other),
â€¢	water demand and uptake is higher,
â€¢	N stress is lower for longer because the plant has access to some of the N in Zone 0 and 2,
â€¢	soil N storage declines more slowly, and
â€¢	more irrigation is required in Zones 0 and 2 because of the additional water uptake from the Sone1 ryegrass and its spreading roots.
4.5 Conceptual Three Zone Simulation with Initial Nonuniform Soil N
This simulation differs from the previous only in that Zone0 initially starts with 100 kg of mineral N in the soil. This results in the expected changes in plant performance and depletion of soil N and water.
5 Interface
5.1 SoilArbitrator
Properties (Outputs)
Name
Description
Units
Type
Settable?
Structure
IStructure
True
6 References
Huth, N.I., Bristow, K.L., Verburg, K., 2012. SWIM3: Model use, calibration, and validation. Transactions of the ASABE 55 (4), 1303-1313.
Keating, B. A., Carberry, P. S., 1993. Resource capture and use in inter cropping - solar radiation. Field Crops Research 34 (3-4), 273-301.
Richards, Lorenzo Adolph, 1931. Capillary conduction of liquids through porous mediums. Journal of Applied Physics 1 (5), 318-333.
Snow, V. O., Huth, N. I., 2004. The APSIM MICROMET Module. HortResearch Internal Report, HortResearch, Auckland.
