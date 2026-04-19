# InitialisingSoilCarbonPools documentation

Source URL: https://docs.apsim.info/tutorial/InitialisingSoilCarbonPools
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:54:30.448091+00:00

1 InitialisingSoilCarbonPools Tutorial
Initialising soil carbon pools
Val Snow (AgResearch, New Zealand) and Dean Holzworth (CSIRO, Australia)
(Last updated 21 May 2024)
The intention is that this documentation is read alongside the simulation that produced it to see the detail of the examples. The simulation is to be found under the "Examples" button, then select the “Tutorials” folder and open “InitialisingSoilCarbonPools.apsimx”.
APSIM, like many other models, uses conceptual soil carbon pools in the simulation of carbon and nitrogen dynamics. See Probert et al. (1998) and Cichota et al (2021) for a description of these pools. Because the pools are conceptual, they cannot be measured and so a method to initialise them is needed and that is the purpose of this tutorial.
The general approach is to set up a ‘spin up’ simulation. This is a long-duration spin-up simulation that includes the weather, soil and management (cropping, fertiliser, irrigation) conditions appropriate for the intended simulation. This simulation is run for an appropriate time and the partitioning between the pools at the end of the spin up are captured to initialise the new simulation.
The duration of the spin up should be set depending on the known general history of the site being simulated. For example, if the site has been in undisturbed grassland for hundreds of years then a 100+ year simulation under generally similar conditions should be made. Usually there is not daily weather available for long spin-ups so the WeatherSampler component is used to artificially lengthen what ever data is available through random sampling with replacement of a shorter weather file. See the notes in WeatherSampler for more information on this.
A second complication is that APSIM’s outputs are structured in the same way as the data needed to set initial values for the carbon pools. This is remedied using a model applied to the DataStore that will reformat the outputs such that they can be pasted back into the Organic UI. To use this, ensure that the “JustFinals” Report component is in the simulation (copy it from this tutorial simulation), add the “CreateProfileTable” model to the DataStore and select “JustFinals”.
Now when you click on the DataStore and select “CreateProfileTable” in the “Table” dropdown, a properly formatted set of data will be available to copy and paste into Organic.
For this method to work, ensure that before you run the spin-up simulation that the depth layering in Organic is the same as in the Physical model.
References
Cichota, R., I. Vogeler, J. Sharp, K. Verburg, N. Huth, D. Holzworth, N. Dalgliesh and V. Snow (2021). "A protocol to build soil descriptions for APSIM simulations." MethodsX 8: 101566.
Probert, M. E., J. P. Dimes, B. A. Keating, R. C. Dalal and W. M. Strong (1998). "APSIM's water and nitrogen modules and simulation of the dynamics of water and nitrogen in fallow systems." Agricultural Systems 56: 1–28.
