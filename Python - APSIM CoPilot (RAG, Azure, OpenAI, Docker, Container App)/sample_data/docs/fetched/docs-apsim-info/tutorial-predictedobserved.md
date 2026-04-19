# PredictedObserved documentation

Source URL: https://docs.apsim.info/tutorial/PredictedObserved
Source domain: docs.apsim.info
Fetched at: 2026-04-19T09:54:05.783967+00:00

1 PredictedObserved Tutorial
2 Importing data from spreadsheet
APSIM is capable of importing observed data from an Excel spreadsheet and adding the data to graphs.
To import data from a spreadsheet, add an ExcelInput model (under AddModel, PostSimulationTools) to the DataStore and rename it 'Observed'. You can then specify the name of the xlsx file and the worksheet name to import. When the simulation is run (or when the DataStore is refreshed - right click on DataStore), the data will be imported into a data table inside the .db file. Graphs can then be created with series that point to the data table and you can choose x and y field names to plot. The time series graphs below show the biomass and LAI columns plotted. Each graph has a predicted series and an observed series.
NOTE:
For this to work, it is important to ensure the simulation names in the spreadsheet (column A)
exactly
match the simulation names produced by the experiment. To see the expected names, click on the experiment and the simulation names will be shown on the right. If the names don't match, no observed data will be shown on the graphs.
3 Matching predicted and observed data
Once observed data has been imported, you can add a 'PredictedObserved' model to the DataStore.
You can then specify the name of the predicted table (usually the name of your report model e.g. DailyReport in our example) and observed table. You then need to specify one or more 'match fields' (column names) from the observed table to match on. Generally, this is SimulationID and Clock.Today. When the simulation is run (or when the DataStore is refreshed - right click on DataStore), the 'PredictedObserved' model will iterate through each value in the observed table and try and find a matching predicted value using the values of the 'match fields' (SimulationID and Clock.Today in our example).
NOTE:
For this to work though, it is important to ensure the column names in the spreadsheet
exactly
match the column names in the predicted (report) table. The simulation names also need to
exactly
match.
In the figure above, you can see the value of Barley.Leaf.LAI from the observed table was matched to the predicted value where the simulation name and Clock.Today values matched.
You might also notice that the Predicted Observed table doesn't include above ground biomass. This is because the column name in the observed table (Barley.AboveGround.Wt) doesn't match the column name in the predicted table ((Barley.AboveGroundLive.Wt).
The graph below was created with a series that points to the 'PredictedObserved' table and plots Predicted.Barley.Leaf.LAI against Observed.Barley.Leaf.LAI. This would not have been possible without the PredictedObserved model matching fields between 2 tables. The graph series also has a child 'Regression' model under it that displays the regression line.
