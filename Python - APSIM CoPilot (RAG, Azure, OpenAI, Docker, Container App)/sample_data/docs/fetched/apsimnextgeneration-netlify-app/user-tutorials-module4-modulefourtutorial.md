# APSIM Next Generation

Source URL: https://apsimnextgeneration.netlify.app/user_tutorials/module4/moduleFourTutorial
Source domain: apsimnextgeneration.netlify.app
Fetched at: 2026-04-19T09:54:39.363676+00:00

The next generation of APSIM
>
APSIM Next Gen User Tutorials
> Module 4: Sowing A Crop
Module 4: Sowing A Crop
Created 23/02/2023 - Last updated 05/03/2023
IMPORTANT NOTE: It is highly recommended that you upgrade your APSIM Next Gen version to at least version 2023.2.7164.0 or later.
Sowing A Crop
In this exercise you will observe the growth of a crop over a single season.
You will learn a bit more about how to use APSIM to do a ‘what-if’ experiment with fertiliser rates.
These skills can not only be used to experiment with sowing fertiliser rates but also variables such as:
Time of planting.
Rate of sowing.
Crop comparisons and different starting soil moisture conditions.
Start a new simulation using the
Wheat.apsimx
example.
you can find this in the
open an example
menu item
Rename the simulation as
Wheat
.
Save this simulation as
Module4
.
Make sure that
AU_Dalby.met
is the selected met file under the weather node.
Set the start and end dates of the simulation as
1/01/1989 - 31/12/1989
under the
Clock
node.
Set the starting water to 25% full - filled from top. This is under
Soil
then
Water
.
Set the
initial values
column of both
NO3
and
NH4
to
kg/ha
.
Now let’s make some changes to the
Fertilise at sowing
management node.
Change the
Fertilise at sowing
parameter
Amount of fertiliser to be applied (kg/ha)
to
0
:
Let’s run the simulation and then inspect the
Graph
graph.
Make sure the x and y axes are set to
Clock.Today
and
Yield
respectively.
Rename the graph to
Wheat Yield Time Series
and its’
Series
to
Wheat Yield
.
We can see with 0 sowing fertiliser we achieved a yield of almost 900 kg/ha.
Next we will create an experiment where we alter the sowing fertiliser amount for this year.
We will see how this affects the yield.
Creating an experiment
First add a
Experiment
node to the
Simulations
node at the top of the simulations tree.
The
Experiment
node will be added to the bottom of the tree, hold
ctrl
key and press
up arrow
several times until it is directly below the
Simulations
node.
Add a
Factors
node to this experiment.
Add a
Factor
node to the
Factors
node.
To be able to change factors in our experiment, we will have to add our
Wheat
simulation to the experiment as a child node.
Drag and drop the
Wheat
simulation node onto the
Experiment
node.
Delete the
Wheat
simulation that is not a child of the experiment node.
Next, we will create several versions of our experiment with varying sowing fertiliser amounts.
To do this, add this line to the
Factor
node:
[Fertilise at sowing].Script.Amount = 0 to 60 step 30
If we click back on the
Experiment
node, you can see 3 differing amounts of sowing fertiliser.
To see the results lets drag a copy of the
Wheat Yield Time Series
graph onto the
Experiment
node.
Change the variables to reflect the image below:
Run the simulation. You’ll see that the sowing fertiliser amounts increase the yield of the wheat crop by varying degrees.
Congratulations on completing the 4th module
Note
: If you found any incorrect/outdated information in this tutorial, please let us know on GitHub by
submitting an issue.
