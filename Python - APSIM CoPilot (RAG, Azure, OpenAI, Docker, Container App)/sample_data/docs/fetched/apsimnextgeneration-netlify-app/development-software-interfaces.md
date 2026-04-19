# APSIM Next Generation

Source URL: https://apsimnextgeneration.netlify.app/development/software/interfaces
Source domain: apsimnextgeneration.netlify.app
Fetched at: 2026-04-19T09:51:35.619997+00:00

The next generation of APSIM
>
Development
>
Software
> Software interfaces
Software interfaces
A model needs to be loosely coupled to other models to allow it to be replaced by an alternate implementation. To enable this, it is important that models implement the required interfaces.
Model type
Description
Plant
To allow users to call methods for Sow, Harvest, EndCrop implement
IPlant
To allow MicroClimate to calculate and arbitrate canopy light interception implement
ICanopy
To allow SoilArbitrator to calculate and arbitrate water and nutrient uptake implement
IUptake
To allow plant consumers (pest / diseases / stock) to damage a plant implement
IPlantDamage
and
IOrganDamage
Soil water
To allow models to get water variables implement
ISoilWater
Nutrient
To allow models to get nutrient variables implement
ISolute
and
INutrient
Surface organic matter
To allow models (e.g. Plant, Stock, SimpleGrazing) to transfer biomass to the surface residue pools and to get surface residue variables implement
ISurfaceOrganicMatter
To allow plant consumers (pest / diseases / stock) to damage surface residues implement
IPlantDamage
and
IOrganDamage
Weather
To allow models to get weather data implement
IWeather
Soil temperature models
To allow models to get soil temperature variables implement
ISoilTemperature
The converse of the above table also holds true. For example, a canopy arbitration model must
look for and use
models that implement
ICanopy
.
