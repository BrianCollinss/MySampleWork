# APSIM Next Generation

Source URL: https://apsimnextgeneration.netlify.app/user_tutorials/module5/moduleFiveTutorial
Source domain: apsimnextgeneration.netlify.app
Fetched at: 2026-04-19T09:54:39.620831+00:00

The next generation of APSIM
>
APSIM Next Gen User Tutorials
> Module 5: Long-term simulations
Module 5: Long-term simulations
Created 07/03/2023 - Last updated 05/03/2023
IMPORTANT NOTE: It is highly recommended that you upgrade your APSIM Next Gen version to at least version 2023.2.7164.0 or later.
Long-term Simulations
Chickpea sowing rates - 40 year runs
In this exercise you will use sowing rules to plant Chickpea crops and observe yield probabilities for a 40 year period given a half full soil moisture profile at sowing.
We will compare two sowing rate strategies for these conditions with the goal of maximising yield.
The weather will be different each year but the soil starting conditions will be the same.
By default, in long term simulations (i.e. longer than one year), the end of one years’ simulation becomes the starting point of the next year.
This is useful if you are interested in seeing the degradation or improvement of the soil over a long time period.
But what if you wanted to work out what the best strategy would be for the current year using weather scenarios from the past 40 years?
To do this we could create 40 different simulations all with the same starting conditions but a different weather file (which would be a lot of work),
or we could run the same simulation over 40 years and just reset the starting conditions each year (much simpler).
We can then try different management strategies to see which one would have worked best under the past 10 years weather scenarios.
Start a new simulations using
Wheat
example.
Rename the
simulation
as
Chickpea
and save the simulation as
Module5
.
Change the
Clock
nodes’ start and end date values to:
1/01/1940
and
31/12/1980
Copy the
Heavy clay
soil from the
Training toolbox
and delete the exisiting
Soil
node.
Set the starting
Water
to
50%
full - filled from top.
Set NO3
Depth
to
0-1800
and
initial value
to
20kg/ha
(right-click heading to change units).
Set NH4
Depth
to
0-1800
and
initial value
to
0kg/ha
.
Change the
SurfaceOrganicMatter
node’s type to sorghum (don’t forget to rename the pool name to
sorghum
as well),
initial surface residue:
550 kg/ha
, Carbon:Nitrogen ratio of
76
, leave the Fraction of residue standing as is.
Right-click
Field
and add a
Chickpea
plant node. All plants are found under the
PMF
folder.
Delete
Wheat
.
In
Sow using a variable
change the values to match the following:
Run the simulation.
You should get an error that at it’s end says:
.Chickpea.Field.Report can not find the component: [Wheat]
. This means we need to update the report variables and report events in
Report
. Let’s change any that have
[Wheat]
to
[Chickpea]
.
Remove
[Chickpea].Grain.Protein
.
Change the report variable
[Wheat].Phenology.Zadok.Stage
to
[Chickpea].Phenology.Zadok
.
Also, remember to change the report event down the bottom
[Wheat].Harvesting
.
Run the simulation again.
You should get an error with the message: Cannot find a soil crop parameterisation called ChickpeaSoil
This is caused by using a soil that is missing a
SoilCrop
node (located under
Physical
) with the same name as the plant node you are using. For now, create a copy of
WheatSoil
under
Physical
and rename it
ChickpeaSoil
to fix this exception.
Before we run again, let’s update our
Harvest
management node to look for the correct crop, now that
Wheat
has been removed and
Chickpea
has ben added.
Your simulation should now run successfully and you should have data in your
DataStore
node.
Add a management script called
Reset on date
to the
Chickpea
simulation (found in
Management Toolbox
under
Other
)
Leave all parameters as yes and change the
date to reset on
to
1-may
.
note:
make sure this management script sits above all other management scripts in the tree as they are ran in the order they are arranged from top to bottom.
Remove the
Fertilise at sowing
manager node.
Rename the
Chickpea
simulation as
Chickpea 10 plants
.
Copy
Chickpea 10 plants
and rename it
Chickpea 15 plants
.
Change the
Plant population(/m2)
to
15
in the
Sowing using a variable
management script.
Right-click the
DataStore
and click
Empty the datastore
. This makes sure that we have the most up to date data once we run, then run the simulations.
Create a graph under
Simulations
. Rename it
Total chickpea yield time series
with a series that plots
[Clock].Today
and
Yield
.
Now you have a graph showing yield when comparing differing plant density over 40 years.
If you’ve copied the graph settings from the image below and you can’t see both simulations data displayed. Make sure
Chickpea 15 plants
sits above
Chickpea 10 plants
in the tree.
Note:
If you found any incorrect/outdated information in this tutorial. Please let us know on GitHub by
submitting an issue.
