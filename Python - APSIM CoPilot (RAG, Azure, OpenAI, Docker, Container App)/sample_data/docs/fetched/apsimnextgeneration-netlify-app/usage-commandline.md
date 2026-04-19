# APSIM Next Generation

Source URL: https://apsimnextgeneration.netlify.app/usage/commandline
Source domain: apsimnextgeneration.netlify.app
Fetched at: 2026-04-19T09:51:23.459996+00:00

The next generation of APSIM
>
Usage
> Command Line
Command Line
To run APSIM from the command line you need to locate the Models.exe binary file. On Windows this is located in
C:\Program Files\APSIM<Version-number>\bin\Models.exe
on LINUX it is located in
/usr/local/bin/Models.exe
Command line usage.
To run simulation specific files from the command line:
Models.exe file.apsimx file2.apsimx
–recursive
- Recursively search through subdirectories for files matching the file specification.
Models --recursive dir/*.apsimx
–upgrade
-  Upgrade a file to the latest version of the .apsimx file format without running the file.
–run-tests
- After running a file, run all tests inside the file.
–verbose
- Write detailed messages to stdout when a simulation starts/finishes.
–csv
- After running all files, export all reports to .csv files
–merge-db-files
- Merge multiple .db files into a single .db file.
Models.exe --merge-db-files site1.db site2.db
–list-simulations
- write the names of all simulations in a .apsimx file to stdout. The files are not run.
–list-enabled-simulations
- write the names of all
enabled
simulations in a .apsimx file to stdout. The files are not run.
–list-referenced-filenames
-   write the names of all files that are referenced by an .apsimx file(s) to stdout e.g. weather files, xlsx files.
–single-threaded
- Run all simulations sequentially on a single thread.
–simulation-names
-  Only run simulations if their names match a regular expression.
Models.exe file1.apsimx --simulation-name *Australia*
–apply
-  Apply commands from a .txt file. Can be used to create new simulations and modify existing ones.
Click here for more info
Models.exe --apply commands.txt
Models.exe file1.apsimx --apply commands.txt
–playlist
-  Allows a group of simulations to be selectively run. Requires a playlist node to be present in the APSIM file.
Click here for more info
Models.exe file1.apsimx --playlist playlist1
–log
- Change the log (summary file) verbosity.
Models.exe example.apsimx --log error
Models.exe example.apsimx --log warning
Models.exe example.apsimx --log information
Models.exe example.apsimx --log diagnostic
Models.exe example.apsimx --log all
–in-memory-db
- Use an in memory database rather than writing simulation output to a .db file.
Models.exe example.apsimx --in-memory-db
–batch
-  Allows the use of a .csv file to specify values of variables than can be substituted into a command file (–apply).
Click here for more info
Models.exe --apply command.txt --batch values.csv
–file-version-number
- Write the file version number of an apsimx file to stdout.
Models.exe File1.apsimx --file-version-number
–help
- Write all command line switches to stdout.
–version
- Write the APSIM version number to stdout.
