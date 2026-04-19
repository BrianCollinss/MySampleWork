# APSIM Next Generation

Source URL: https://apsimnextgeneration.netlify.app/user_tutorials/module2/moduletwotutorial
Source domain: apsimnextgeneration.netlify.app
Fetched at: 2026-04-19T09:51:39.689779+00:00

The next generation of APSIM
>
APSIM Next Gen User Tutorials
> Module 2: Surface Organic Matter Simulation
Module 2: Surface Organic Matter Simulation
Created 23/02/2023 - Last updated 05/03/2023
IMPORTANT NOTE: It is highly recommended that you upgrade your APSIM Next Gen version to at least version 2023.2.7164.0 or later.
The Effect of Residue Cover on Soil Water Storage during Fallow
Tracking the Decline of Cover as Residues Decompose.
APSIM simulates the influence of crop residues on the efficiency with which water is captured and retained during fallows.
Residue cover declines as residues decompose.
Residue decomposition is simulated in APSIM in response to weather, as well as the chemical composition of the residues.
By doing this simulation you will reinforce skills learned in previous exercises and learn to do some basic editing of default values to “customise” your simulations.
This module assumes you have completed the previous module: “Module One: Fallow Simulation”. It will introduce you to the Surface Organic Matter module and demonstrate how surface residue decomposes over time.
For this module we will use the simulation we created in Exercise 1 (Module1) as a base.
There is also a completed example in the training toolbox if you would prefer to use that. Open the file Module1.apsim.
Save the file as Module2.apsim in your
Apsim Training
folder.
Remember to use
Save As
not
Save
or you will overwrite the old file.
Delete the
Sand Fallow
simulation. We’ll use
Clay Fallow
as the starting point. Also delete the graph.
Make a copy of
Clay Fallow
by dragging it to the simulations node in the tree and rename this new simulation to
Clay Residue
.
Your node tree should look like this:
Expand the new simulation then expand the paddock node. Click the SurfaceOrganicMatter module and change the initial surface residue to 3000 kg/ha.
Run both of the simulations, either by clicking on the
simulations
node at the top and then clicking Run, or selecting them individually and running them.
Create a graph of Date vs surface organic matter cover (SurfaceOrganicMatter.Cover) and Weather.Rain (right hand axis) for the
Clay Residue
simulation.
To do this:
right-click
Clay Residue
node.
change graph name to
Organic Matter Cover
right-click
Organic Matter Cover
node,
double-click
Add model...
,
double-click
Series
click
Series
node
change
X
to
Clock.Today
change
Y
to
SurfaceOrganicMatter.Cover
select
Report
as data source
change the
type
to
Scatter
change
line type
to
Solid
change
marker type
to
None
change
colour
to yellow
rename this
Series
node as
SOM
.
Your node tree should look like this:
Create another series for rain under the
Graph
node.
To do this:
right-click
Organic Matter Cover
node
double-click
Add model...
double-click
Series
rename this series to
Rain
click
Rain
node
select
Report
for
Data Source
change
X
to
Clock.Today
change
Y
to
Weather.Rain
check
on right?
checkbox
change
Type
to
Scatter
change
Line type
to
Solid
change
Marker type
to
None
change
Colour
to blue.
Your node tree should look like this:
The effect of cover decline on runoff and evaporation
We will compare the effect that ground cover has on runoff.
Graph both simulations, with Date vs runoff (cumulative) and Rain (right axis).
To do this:
right-click the
Simulations
node
click
add model...
double-click
Graph
Rename the graph to Runoff.
Let’s build the graph
To do this:
right-click
Runoff
graph
click
Add model...
double-click
Series
rename series to
Cumulative Runoff
Set your series variables to below:
NOTE: If you only get one graph it means that one simulation has not been run yet.
Let’s add another Series for
Rain
To do this:
right-click
Runoff
click
Add model
double-click
Series
rename this series to
Rain
set the series variables to below:
The effect of residue type on speed of decomposition
The APSIM residue model will decompose residues at differing rates according to the C:N ratio of the material.
To demonstrate this we will reproduce the previous simulation but apply legume residues in the place of the wheat residues.
Create another copy of the
Clay Residue
simulation. Rename it to
Clay Chickpea Residue
. Remove the graph component.
Remove the [Weather].Rain Report Variable from the Report node in the ‘Clay Chickpea Residue’ simulation.
Change the SurfaceOrganicMatter residue parameters to 3000 kg/ha of chickpea residue.
Also change the initial residue pool to ‘chickpea’.
Change the C:N ratio to 25.
Run the simulation.
Create a graph with all three residue simulations with residue as a function of time. Call the graph
Cover
.
To do this:
right-click the
simulations
node
click
Add model...
double-click
Graph
rename
Graph
to
Cover
right-click
Cover
graph
click
Add model...
double-click
Series
Change the variables to match the image below:
If you’d like the lengend to appear on the right of the graph like the image:
Click one of the legend items
a menu will appear at the bottom of the graph and you can change the drop down menu’s value to top right or any other position to your liking.
Congratulations on completing module two!
Note
: If you found any incorrect/outdated information in this tutorial, please let us know on GitHub by
submitting an issue.
