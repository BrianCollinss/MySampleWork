# APSIM Next Generation

Source URL: https://apsimnextgeneration.netlify.app/user_tutorials/module3/moduleThreeTutorial
Source domain: apsimnextgeneration.netlify.app
Fetched at: 2026-04-19T09:54:39.106494+00:00

The next generation of APSIM
>
APSIM Next Gen User Tutorials
> Module 3: The Nitrogen Cycle
Module 3: The Nitrogen Cycle
Created 01/03/2023 - Last updated 23/05/2024
IMPORTANT NOTE: It is highly recommended that you upgrade your APSIM Next Gen version to at least version 2023.2.7164.0 or later.
The Nitrogen Cycle
In this exercise you will observe the cycle of fertiliser nitrogen in a fallow situation; urea to ammonium, ammonium to nitrate and the loss of soil nitrate via denitrification.
This simulation will introduce editing a simple Manager rule and demonstrate more advanced features of graphing simulation results.
Start a new simulation based on the
Wheat
example.
Rename this simulation to Nitrogen Cycle.
Save this file as
Module3
.
The simulation will use a different weather file. To do this:
click the
weather
node
click the
browse
button
double-click
AU_Dalby
(C:\Program Files\APSIM[Version]\Examples\WeatherFiles)
In the
Clock
node, change the starting date to
1/1/1989
and the end date to
31/12/1989
Add the
Heavy Clay
soil from the
Training toolbox
Delete
Soil
node
Set
Percent full
to 50 in the
Heavy Clay's
Water
node.
Set starting nitrogen to 19kg/ha NO3 and 0 NH4, evenly distributed. Don’t forget to change units to kg/ha (right-click the column header
ppm
).
Make the depth equal to the entire soil profile (check Water node for the profile depth).
Delete all manager scripts (all have a farmer icon: SowingFertiliser, Harvest, SowingRule1)
Copy a
Fertilise on fixed dates
management node to the field node. You can locate this by going to:
Home
Management toolbox
Fertilise folder
You can either drag this to your
Field
node or copy and paste it to the
Field
node.
Change
Type of fertiliser to apply?
to
UreaN
Change fertilisation date to
10-Jan
Change
Amount of fertiliser to be applied (kg/ha)
to
100
In the
Report
node let set up the output variables:
[Clock].Today
[Weather].Rain
[Soil].SoilWater.Drainage
sum([Soil].SoilWater.ESW)
sum([Soil].NO3.kgha) as NO3Total
sum([Soil].NH4.kgha) as NH4Total
sum([Soil].Nutrient.DenitrifiedN) as Denitrification
In the
Report events
section remove existing event variables. Then add:
[Clock].EndOfDay
Run the simulation
Delete the current graph named
Wheat Yield Time Series
Add another
Graph
node to the
Nitrogen Cycle
simulation node
Rename
Graph
to
Nitrogen
Add a
Series
to
Nitrogen
graph
Rename
Series
to
NO3 total
Variables for this
Series
Add another
Series
node to
Nitrogen
Rename this
Series
to
NH4 total
Variables for this
Series
Create a new graph with a series each for
rain
,
sum([Soil].SoilWater.ESW)
,
NO3Total
,
Denitrification
.
each series should have
[Clock].Today
as the
X
axis variable.
From this chart you can see that significant nitrogen is lost via denitrification when large amounts of nitrate are available in saturated soil conditions.
Congratulations on finishing the 3rd module!
Note
: If you found any incorrect/outdated information in this tutorial, please let us know on GitHub by
submitting an issue.
