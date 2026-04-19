# Clock documentation

Source URL: https://docs.apsim.info/model/Clock
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:54:36.012108+00:00

1 The APSIM Clock Model
###Clock model
The clock model is responsible for controlling the daily timestep in APSIM. It
keeps track of the simulation date and loops from the start date to the end
date, publishing events that other models can subscribe to.
1.1 Clock
Function OnDoCommence(object _, CommenceArgs e) of Model Clock contains the following Events in the given order.
Event Handle
Summary
StartOfSimulation
Occurs once at the start of the simulation.
FinalInitialise
Final Initialise event. Occurs once at start of simulation.
StartOfFirstDay
Occurs once at the start of the first day of the simulation.
DoCatchYesterday
Occurs first each day to allow yesterdays values to be caught
DoWeather
Occurs each day to calculuate weather
DoDailyInitialisation
Occurs each day to do daily updates to models
StartOfDay
Occurs at start of each day.
StartOfMonth
Occurs at start of each month.
StartOfYear
Occurs at start of each year.
StartOfWeek
Occurs at start of each week.
DoManagement
Occurs each day to do management actions and changes
DoFertiliserApplications
Invoked to perform all fertiliser applications.
DoPestDiseaseDamage
Occurs to do Pest/Disease actions
DoEnergyArbitration
Occurs when the canopy energy balance needs to be calculated with MicroCLimate
DoSoilErosion
Occurs to tell soil erosion to perform its calculations.
DoSoilWaterMovement
Occurs each day to do water calculations such as irrigation, swim, water balance etc
DoSoilTemperature
Occurs to perform soil temperature calculations to do solute processes.
DoSolute
Occurs each day
DoSurfaceOrganicMatterPotentialDecomposition
Occurs each day to perform daily calculations of organic soil matter
DoSoilOrganicMatter
Occurs each day to perform daily calculations of organic soil matter
DoSurfaceOrganicMatterDecomposition
Occurs each day to do the daily residue decomposition
DoUpdateWaterDemand
Occurs each day to do daily growth increment of total plant biomass
DoDCAPST
Occurs each day when when dcaps performs its calculations. This must happenbetween DoPotentialPlantGrowth and DoPotentialPlantPartitioning.
DoWaterArbitration
Occurs each day to do water arbitration
PrePhenology
Occurs between DoWaterArbitration and DoPhenology. Performs sorghum final leaf no calcs.
DoPhenology
Occurs each day to perform phenology
DoPotentialPlantGrowth
Occurs each day to do potential growth
DoPotentialPlantPartioning
Occurs each day to do the water limited dm allocations.  Water constaints to growth are accounted for in the calculation of DM supplyand does initial N calculations to work out how much N uptake is required to pass to SoilArbitrator
DoPastureWater
Initiates water calculations for the Pasture model
DoNutrientArbitration
Occurs each day to do nutrient arbitration
DoActualPlantPartioning
Occurs each day to do nutrient allocations
DoActualPlantGrowth
Occurs each day to do nutrient allocations. Pasture growth
PartitioningComplete
Occurs each day to finish partitioning
DoStock
Occurs each day to process stock methods in GrazPlan Stock
DoLifecycle
Occurs each day to process a Pest and Disease lifecycle object
DoUpdate
Occurs near end of each day to do checks and finalising
DoManagementCalculations
Occurs each day after the simulation is done. Does managment calculations
DoEndPasture
Occurs after pasture growth and sends material to SOM
DoReportCalculations
Occurs when [do report calculations].
EndOfWeek
Occurs at end of each week.
EndOfYear
Occurs at end of each year.
EndOfMonth
Occurs at end of each month.
EndOfDay
Occurs at end of each day
DoReport
Occurs at end of each day
EndOfSimulation
Occurs at end of simulation.
2 Interface
2.1 Clock
Parameters (Inputs)
| Name  | Description                  | Units | Type       | Value                 |
| ----- | ---------------------------- | ----- | ---------- | --------------------- |
| Start | Models.Core.SummaryAttribute |       | nullable
1 | 7/1/1980 12:00:00 AM  | | End   | Models.Core.SummaryAttribute |       | nullable
1 | 6/30/1985 12:00:00 AM |
Properties (Outputs)
Name
Description
Units
Type
Settable?
Structure
IStructure
True
StartDate
If the user did notnot provide a start date, attempt to locate a weather fileand use its start date. If no weather file can be found,throw an exception.
datetime
True
EndDate
If the user did notnot provide a end date, attempt to locate a weather fileand use its end date. If no weather file can be found,throw an exception.
datetime
True
Today
datetime
True
FractionComplete
double
False
IsStartMonth
boolean
False
IsStartYear
boolean
False
IsEndMonth
boolean
False
IsEndYear
boolean
False
Links (Dependencies)
Name
Type
IsOptional?
Summary
ISummary
False
Events published
Name
Type
StartOfSimulation
Void StartOfSimulation (Object sender, EventArgs e)
StartOfFirstDay
Void StartOfFirstDay (Object sender, EventArgs e)
StartOfDay
Void StartOfDay (Object sender, EventArgs e)
StartOfMonth
Void StartOfMonth (Object sender, EventArgs e)
StartOfYear
Void StartOfYear (Object sender, EventArgs e)
StartOfWeek
Void StartOfWeek (Object sender, EventArgs e)
EndOfDay
Void EndOfDay (Object sender, EventArgs e)
EndOfMonth
Void EndOfMonth (Object sender, EventArgs e)
EndOfYear
Void EndOfYear (Object sender, EventArgs e)
EndOfWeek
Void EndOfWeek (Object sender, EventArgs e)
EndOfSimulation
Void EndOfSimulation (Object sender, EventArgs e)
FinalInitialise
Void FinalInitialise (Object sender, EventArgs e)
DoCatchYesterday
Void DoCatchYesterday (Object sender, EventArgs e)
DoWeather
Void DoWeather (Object sender, EventArgs e)
DoDailyInitialisation
Void DoDailyInitialisation (Object sender, EventArgs e)
DoInitialSummary
Void DoInitialSummary (Object sender, EventArgs e)
DoManagement
Void DoManagement (Object sender, EventArgs e)
DoFertiliserApplications
Void DoFertiliserApplications (Object sender, EventArgs e)
DoPestDiseaseDamage
Void DoPestDiseaseDamage (Object sender, EventArgs e)
DoEnergyArbitration
Void DoEnergyArbitration (Object sender, EventArgs e)
DoSoilWaterMovement
Void DoSoilWaterMovement (Object sender, EventArgs e)
DoSoilErosion
Void DoSoilErosion (Object sender, EventArgs e)
DoSoilTemperature
Void DoSoilTemperature (Object sender, EventArgs e)
DoSolute
Void DoSolute (Object sender, EventArgs e)
DoSurfaceOrganicMatterPotentialDecomposition
Void DoSurfaceOrganicMatterPotentialDecomposition (Object sender, EventArgs e)
DoSoilOrganicMatter
Void DoSoilOrganicMatter (Object sender, EventArgs e)
DoSurfaceOrganicMatterDecomposition
Void DoSurfaceOrganicMatterDecomposition (Object sender, EventArgs e)
DoUpdateWaterDemand
Void DoUpdateWaterDemand (Object sender, EventArgs e)
DoWaterArbitration
Void DoWaterArbitration (Object sender, EventArgs e)
DoPastureWater
Void DoPastureWater (Object sender, EventArgs e)
PrePhenology
Void PrePhenology (Object sender, EventArgs e)
DoPhenology
Void DoPhenology (Object sender, EventArgs e)
DoPotentialPlantGrowth
Void DoPotentialPlantGrowth (Object sender, EventArgs e)
DoPotentialPlantPartioning
Void DoPotentialPlantPartioning (Object sender, EventArgs e)
DoNutrientArbitration
Void DoNutrientArbitration (Object sender, EventArgs e)
DoActualPlantPartioning
Void DoActualPlantPartioning (Object sender, EventArgs e)
DoActualPlantGrowth
Void DoActualPlantGrowth (Object sender, EventArgs e)
PartitioningComplete
Void PartitioningComplete (Object sender, EventArgs e)
DoUpdate
Void DoUpdate (Object sender, EventArgs e)
DoStock
Void DoStock (Object sender, EventArgs e)
DoLifecycle
Void DoLifecycle (Object sender, EventArgs e)
DoManagementCalculations
Void DoManagementCalculations (Object sender, EventArgs e)
DoEndPasture
Void DoEndPasture (Object sender, EventArgs e)
DoReportCalculations
Void DoReportCalculations (Object sender, EventArgs e)
DoReport
Void DoReport (Object sender, EventArgs e)
DoDCAPST
Void DoDCAPST (Object sender, EventArgs e)
CLEMInitialiseResource
Void CLEMInitialiseResource (Object sender, EventArgs e)
CLEMInitialiseActivity
Void CLEMInitialiseActivity (Object sender, EventArgs e)
CLEMValidate
Void CLEMValidate (Object sender, EventArgs e)
CLEMStartOfTimeStep
Void CLEMStartOfTimeStep (Object sender, EventArgs e)
CLEMUpdateLabourAvailability
Void CLEMUpdateLabourAvailability (Object sender, EventArgs e)
CLEMUpdatePasture
Void CLEMUpdatePasture (Object sender, EventArgs e)
CLEMDetachPasture
Void CLEMDetachPasture (Object sender, EventArgs e)
CLEMPastureReady
Void CLEMPastureReady (Object sender, EventArgs e)
CLEMDoCutAndCarry
Void CLEMDoCutAndCarry (Object sender, EventArgs e)
CLEMAnimalBreeding
Void CLEMAnimalBreeding (Object sender, EventArgs e)
CLEMPotentialIntake
Void CLEMPotentialIntake (Object sender, EventArgs e)
CLEMCalculateManure
Void CLEMCalculateManure (Object sender, EventArgs e)
CLEMCollectManure
Void CLEMCollectManure (Object sender, EventArgs e)
CLEMGetResourcesRequired
Void CLEMGetResourcesRequired (Object sender, EventArgs e)
CLEMAnimalMilkProduction
Void CLEMAnimalMilkProduction (Object sender, EventArgs e)
CLEMAnimalWeightGain
Void CLEMAnimalWeightGain (Object sender, EventArgs e)
CLEMAnimalDeath
Void CLEMAnimalDeath (Object sender, EventArgs e)
CLEMAnimalMilking
Void CLEMAnimalMilking (Object sender, EventArgs e)
CLEMCalculateEcologicalState
Void CLEMCalculateEcologicalState (Object sender, EventArgs e)
CLEMAnimalMark
Void CLEMAnimalMark (Object sender, EventArgs e)
CLEMAnimalManage
Void CLEMAnimalManage (Object sender, EventArgs e)
CLEMAnimalStock
Void CLEMAnimalStock (Object sender, EventArgs e)
CLEMAnimalSell
Void CLEMAnimalSell (Object sender, EventArgs e)
CLEMAnimalBuy
Void CLEMAnimalBuy (Object sender, EventArgs e)
CLEMAgeResources
Void CLEMAgeResources (Object sender, EventArgs e)
CLEMHerdSummary
Void CLEMHerdSummary (Object sender, EventArgs e)
CLEMFinalizeTimeStep
Void CLEMFinalizeTimeStep (Object sender, EventArgs e)
CLEMEndOfTimeStep
Void CLEMEndOfTimeStep (Object sender, EventArgs e)
