# SoilTemperature documentation

Source URL: https://docs.apsim.info/validation/SoilTemperature
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:52:48.035027+00:00

1 The APSIM SoilTemperature Model
Val Snow (AgResearch, New Zealand) and Dean Holzworth (CSIRO, Australia)
The soil temperature model simulates soil temperature given minimal input information using a numerical scheme.
It includes functionality for simulating the heat flux and temperatures over the soil profile, including temperature
on the soil surface.
This implementation is largely based on the method described by
Campbell, 1985
but has some modifications
to make it compatible with APSIM.
Acknowledgements
SoilTemperature was completed using funding from AgResearch’s Strategic Science Investment Fund and CSIRO’s
internal funding (SIP).
High level description
See
Campbell, 1985
for details on the numerical scheme   the mathematics is not replicated here. The soil thermal
properties that are needed for the numerical solution are the specific heat capacity (the quantity of energy needed to raise
the soil temperature by 1 C) and the thermal conductivity (the ability of the soil to conduct heat). These properties are
estimated from standard APSIM soil properties using methods from
Campbell, 1985
,
Tian et al., 2016
and
de Vries, 1963
taking into account the possibility that the soil has rocks, ice and high organic matter contents. If the particle size
information for the soil is not supplied then they are estimated as 30% clay, 65% silt and 5% sand with these values
displayed in red in the user interface so that better values may be supplied if available. Initial values of soil temperature
may be supplied by the user and if not they are estimated from a standard simple analytical equation. SoilTemperature
runs 48 timesteps within each day. The upper boundary condition during the day is interpolated using a sine function from
the minimum and maximum air temperature for the day. The lower boundary condition is set at 20 m deep as the annual
average air temperature. To allow this deep lower boundary condition SoilTemperature includes a number of ‘phantom’
layers below the user specified soil profile. The properties of these layers are set to equal those in the deepest simulated
layer and their only purpose is to facilitate the implementation of the lower boundary condition. These nodes are invisible
to the user.
Since temperature changes rapidly near the soil surface and very little at depth, the best simulation
will be obtained with short elements (shallow layers) near the soil surface and longer ones deeper in
the soil. Ideally, the element lengths should follow a geometric progression...
Ten to twelve nodes are probably sufficient for short term simulations (daily or weekly). Fifteen nodes
would probably be sufficient for annual cycle simulation where a deeper grid is needed.
Node structure
Due to FORTRAN's 'flexibility' with arrays that are not present in C#, few modifications have been done
to array sizes in this version of SoilTemp. Here, all arrays are forcibly 0 based, so to deal with the
fact that the original module had both 0  and 1 based arrays, all arrays have been increased in size by
one. With this approach, the indexing does not need to change in those processes that in the FORTRAN code
that processed 1 based arrays, here the first (0th) element would simply not be used...
This is actually rather convenient. In these arrays, the element 0 now refers to the air (airNode),
the element 1 refers to the soil surface (surfaceNode), and from elements 2 (topsoilNode) to numNodes+1
all nodes refer the middle of layers within the soil.
2 ValidationTests
Tested against data and simulaitons supplied by:
Neil Huth (CSIRO, Australia)
Sotirios Archontoulis and Isaiah Huber (Iowa State University, USA)
Hamish Brown (Plant and Food Research, New Zealand)
2.1 Map
2.2 ForestHill
2.2.1 ForestHillWeatherStation
This simulation reproduces the soil conditions within the weather station at CSIRO Forest Hill.
2.2.2 ForestHillWheat
Experiment Name
Design (Number of Treatments)
ForestHillWheat
(2)
2.3 Wagga
This simulation is a duplicate of the Eucalyptus validation Wagga simulation. The only change is the addition of the soil temperature model.
2.4 Norwin
Note that there is limited weatehr data available for this simulaiton. It is likely that the poor performance at 0.5 m is at least partially due to the short run in to the simulation.
2.5 USA BareSoil
Experiment Name
Design (Number of Treatments)
ST
Year (9)
2.6 NewZealand
Simulating crop rotation under controlled  conditions
Rob Zyskowski, Edmar I. Teixeira, Hamish Brown, Edith Khaembah, Rogerio Cichota
The New Zealand Institute for Plant & Food Research Limited, Private Bag 4704, Christchurch, New Zealand
Introduction
This is a simulation of a crop rotation consisting of a forage species, fodder beet (
Beta vulgaris
L.), grown over spring/summer and harvested in autumn, followed by a catch crop, oats (
Avena sativa
L.). The actual experiment was divided in two phases and was established at the rain shelter facility of Plant and Food Research, Lincoln, NZ.  The experiment was designed to help develop the models for the respective plants and here they are used to further demonstrate that ApsimX can simulate water and nitrogen cycling in the field and then to examine whether catch crops are a good option for mitigating N leaching from forages.
Phase 1
: Co-limitation of water and nitrogen on fodder beet physiology
Irrigation: 2 treatments, nil or full irrigation (to match PET)
Nitrogen: 3 treatments, 0 kg N/ha, 50 kg N/ha & 300 kg N/ha applied as dissolved urea with fertigator
25 or 100 kg N/ha applied after emergence
25 or 100 kg N/ha applied when canopy nearly reached full cover
100 kg N/ha applied a month after full cover
Phase 2
: Ability of oats to act as catch-crops for N over winter/spring
Irrigation: 2 treatments, low (enough to keep plants growing) or full irrigation (to match PET)
Nitrogen: 3 treatments, 80 kg N/ha, 125 kg N/ha & 320 kg N/ha at sowing (residual plus urea applied at sowing)
Acknowledgments
This work was a collaborative effort funded by the Sustainable Agro-Ecosystems (SAE) and the Forages for Reduced Nitrogen Leaching (FRNL) programmes.  The experiment was setup and conducted by several people, including Brendon Malcolm, Emmanuel Chakwizira, Shane Maley, Mike George, Steve Dilon and Alexandre Michel, to whom we would like to express our gratitude.
Experiment Name
Design (Number of Treatments)
LincolnRainshelter
Nit x Irr (2)
Management
Pre-experiment actions:
23/08/2016 - Oats harvested
02/09/2016 - Site irrigated with 25mm
05/09/2016 - Site ploughed
06/09/2016 - Site cambridge rolled
17/10/2016 - Site power harrowed
17/10/2016 - Rainshelter was rolled in
18/10/2016 - Fertiliser application (200kg/ha of KCl, 250kg/ha of Triple Super, 200kg/ha of NaCl and 30kg/ha of Boronate 15%)
18/10/2016 - Site cambridge rolled and harrowed
Experiment:
Fodder beet:
Cultivar: Rivage
Sowing density: 11 plants/m2
row spacing: 45 cm
depth: 15 mm
General actions:
18/10/2016 - Sowing
25/10/2016 - Irrigation of 5mm to all plots
27/10/2016 - Plants started to emerge
28/10/2016 - Irrigation of 5mm to all plots
28/10/2016 - First fertiliser application, 25kgN/ha of urea_N to the 50kg/ha plots and 100kgN/ha o the 300kg/ha plots
23/11/2016 - Irrigation treatments started
18/01/2017 - Second fertiliser application, 25Nkg/ha of liquid urea_N to the 50kg/ha plots and 100kgN/ha to the 300kg/ha plots
15/02/2017 - Final fertiliser application, 100Nkg/ha of liquid urea_N to the 300kg/ha plots
17/05/2017 - Plots harvested
Intercrop actions:
19/05/2017 - All crop residues removed
25/05/2017 - Dryland plots irrigated with 100mm, over two days
29/05/2017 - Dryland 300N plot irrigated with 64.0mm
30/05/2017 - Dryland 0N and 50N plots irrigated with 30.3 and 51.0mm
22/06/2017 - Site topped with maxitell
30/06/2017 - Site cambridge rolled
30/06/2017 - Rainshelter was rolled off (but it was put on in a few occasions)
Oats:
Cultivar: Milton
Sowing density: 300 plants/m2
row spacing: 15 cm
depth: 45 mm
General actions:
05/07/2017 - Sowing
05/07/2017 - Fertiliser application (400kg/ha Potash, Urea: treat4=40kgN/ha, treat5=85kgN/ha, treat6=270kgN/ha)
24/07/2017 - Plants started to emerge
11/09/2017 - Irrigation started in treatements 4, 5, and 6
06/11/2017 - Irrigation extended to all plots
04-09/01/2018 - Plots harvested
3 Effect of Soil Layering
SoilTemperature uses a numerical solution to the heat flow equation to find soil temperature at any depth. In principle, the numerical solution is sensitive to the layering but the extent of has not previously been tested in APSIM. Because the model will be used with both SWIM (thin, usually <5mm, surface layers) and SoilWater (thicker, usually >100 mm, surface layers) it is important to understand any sensitivity.
A simple simulation was set up using a ‘dummy’ model to hold soil water contents constant as the purpose here is to test the soil temperature simulation not the soil water simulation. Both simulations had soil extending to 1100 mm deep. The thin-layering simulation had 11 layers with the top layer 2 mm deep and with seven layers in the top 100 mm. The thick-layering simulation had six layers with only one layer in the top 100 mm – a value typical for simulations using SoilWater.
The simulation was further simplified by using synthetic weather that was a simple annual cycle with a superimposed fortnightly oscillation.
All temperatures plotted are the average temperature unless otherwise stated.
The first four plots below compare the simulated soil temperature across four depths with the thin layering in black and the thick layering in orange. Plots 5 to 8 show the 1:1 relationship between the two layering options. Note that the depths of 1, 8 and 15 mm all fall in the top layer of the thick-layering simulation but are in layers 1, 3, 4, and 8 of the thin-layering simulation. Plots 9 and 10 show the maximum simulated temperature at 1 mm deep.
Overall, the simulations showed low sensitivity to the differences in layering (Plots 1-4). There was a greater difference in simulated maximum temperature than average temperature (plots 9, 10). For example, on 22 October the average soil temperature at 1 mm for the thin layering was 29 C v’s 28 for the thick layering (Plots 5 and 6). The differences in maximum temperature are greater than for average temperature. This is particularly evident in Plot 10 where the deviation at higher temperatures is about 3 C. It is possible that these differences may be important in future simulation purposes but these are minor differences when viewed from the perspective of must current uses.
It must be emphasised that this test is on the soil temperature model only. Layering will have a substantial effect on soil water contents when a soil water simulation model is included in the simulation – and the differences in soil water content will feed through to greater differences in the simulated soil temperature. Users should be aware of this when deciding which soil water model to use.
4 Initialisation Choices
SoilTemperature’s numerical solution requires initial values of soil temperature by layer. These are not usually available from measurements so a scheme, based on a simplified analytical solution to the heat flow equation, has been implemented. Those estimated values are used by default unless the user adds in their own values.
Numerical solutions can take some time to ‘forget’ the initial conditions and the time to forget increases with depth. Simulations were set up to demonstrate this feature. The soil is a silt loam in a pasture system based at Lincoln, New Zealand. The simulation is set up with default initial conditions as well as two, deliberately bad, sets of initial temperatures – uniformly at -20.0 and 30.0 C.
The first four plots concentrate on the first two weeks of the simulation and the surface layers. The second four plots show simulated outputs deeper in the soil and over several years.
The deeper in the soil that the output is examined the longer it takes for the simulation to forget the initial temperatures. While we are not suggesting that initial conditions are routinely guessed as badly as these, it is important to note this effect when comparing against data. As a rough guide, if the depth of interest is in the top 300 mm then the ‘run in’ should be at least two weeks. At about 1 m deep the run in should be greater than six months and deeper than that it may be a year.
Note that these are worst-case scenarios and that other models, particularly SoilWater or SWIM and Nutrient, also require substantial run in times.
Experiment Name
Design (Number of Treatments)
InitialisationChoices
(3)
5 Initialisation Time of Year
The initialisation algorithm does a reasonable job of providing initial values but there is a lag once the simulation starts for the model to ‘forget’ the impact of the initial values. The deeper in the soil the longer that memory of the initial conditions is.
Experiment Name
Design (Number of Treatments)
InitialisationTimeOfYear
Factor (6)
6 Resetting SoilTemperature
SoilTemperature allows the user to reset the simulated temperature at any time during the simulation with options of resetting to the values from the start of the simulation or to values specified by depth. Resetting is achieved using an Operations component.
The plots below have the temperature not reset at all (black) compared against a reset to the initial values on 1 September and a further reset to 6 C in every layer on 1 November (ochre). The commands to do the resetting are:
1980-09-01 [SoilTemperature].Reset()
1980-11-01 [SoilTemperature].Reset(6 6 6 6 6 6 6)
Note that if specific values are wanted (as opposed to the initial values), then the user must specify a value for each layer in a space-delimited format. If a LayerStructure is used in the soil then the layers in the reset command align to those in LayerStructure.
Further note that the values are reset at the start of the simulation day.
Experiment Name
Design (Number of Treatments)
Resetting
(2)
7 SensibilityTests - Clay
Experiment Name
Design (Number of Treatments)
ExpClay
ClayContent (2)
It is expected that soil temperature will show a sensitivity to soil properties, such as clay content. From theory, as the clay content increases then the ability to store heat increases. This effect should be evident when examining plots of soil temperature, comparing the effects of day-to-day air temperature oscillations and the annual oscillation cycle. A soil with higher clay content compared to one with a lower clay content, all other things including water content being held constant, should show a more muted effect of the oscillations and a greater delay between the peak air temperature and the peak in soil temperature. Shorter-term oscillations should also be dampened down more quickly with depth.
The plots below show these effects for a bare soil with a clay content of 10 and 90% held at a volumetric water content of 0.15.
8 SensibilityTests - Depth of Lower Boundary
Experiment Name
Design (Number of Treatments)
ExpLowerBC
LowerBC (3)
The assumed lower boundary condition is that of the location’s annual average temperature. That value is asserted as the temperature of the deepest layer in the soil temperature simulation (see the implementation description for more information – this is substantially deeper than the soil profile usually used in APSIM). By default that depth of constant temperature is set to 20 m but users can override that value.
Here a test was created to ensure that the lower boundary depth was sufficiently deep. A successful test is achieved when deepening the lower boundary has no substantive effect on the simulated temperatures. The plots below demonstrate this.
9 Comparison of Temperature Models
The series of plots below compare the old method for simulating soil temperature (sometimes labelled as CERES but actually from EPIC) against the new numerical simulation. The simulations cover biomass, harvested material and soil temperature at 100 and 1250 mm for wheat grown in Dalby (Australia), pasture in Lincoln (New Zealand) and a maize-soybean rotation in Iowa (USA).
The outputs show relatively minor effects on crop/pasture growth and somewhat more realistic simulations of soil temperature at depth which may be important for simulating freezing in soils.
10 Interface
10.1 SoilTemperature
Properties (Outputs)
Name
Description
Units
Type
Settable?
Depth
Models.Core.SummaryAttributeUse for display only, values taken from thickness
mm
String
True
DepthToConstantTemperature
mm
double
True
FinalSoilTemperature
oC
double
False
FinalSoilSurfaceTemperature
oC
double
False
Value
Mandatory for ISoilTemperature interface. For now, just return average daily values - CHECK
oC
double
False
AverageSoilTemperature
If called during init1, this will return an array of length 100 with all elements as 0.0
oC
double
False
AverageSoilSurfaceTemperature
oC
double
False
MinimumSoilTemperature
oC
double
False
MinimumSoilSurfaceTemperature
oC
double
False
MaximumSoilTemperature
oC
double
False
MaximumSoilSurfaceTemperature
oC
double
False
BoundaryLayerConductance
W/K
double
False
ThermalConductivity
W.m/K
double
False
HeatCapacity
J/K/m3
double
False
HeatStore
J/K
double
False
Thr_profile
oC
double
False
Links (Dependencies)
Name
Type
IsOptional?
weather
IWeather
False
clock
IClock
False
microClimate
MicroClimate
True
physical
Physical
False
organic
Organic
False
waterBalance
ISoilWater
False
Events published
Name
Type
SoilTemperatureChanged
Void SoilTemperatureChanged (Object sender, EventArgs e)
Methods (callable from manager)
Name
Description
Reset
void Reset(double values)
Performs the tasks to reset the model
11 References
Campbell, G. S., 1985. Soil Physics With Basic. Soil Physics With Basic, 1-150.
de Vries, D.A., 1963. Physics of Plant Environment. Physics of Plant Environment, 210–235.
Tian, Z., Lu, Y., Horton, R., Ren, T., 2016. A simplified de Vries-based model to estimate thermal conductivity of unfrozen and frozen soil. European Journal of Soil Science 67 (5), 564-572.
