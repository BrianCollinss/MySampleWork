# APSIM Next Generation

Source URL: https://apsimnextgeneration.netlify.app/usage/debugmanagerscript
Source domain: apsimnextgeneration.netlify.app
Fetched at: 2026-04-19T09:51:27.307984+00:00

The next generation of APSIM
>
Usage
> Debug Manager Script
Debug Manager Script
To debug a manager script you need to insert
using System.Diagnostics;
at the top of your manager script. Then to trigger a breakpoint, insert
Debugger.Break();
into a method or property to have the debugger stop. Apsim Next Generation needs to be run from Visual Studio and be in debug mode.When a simulation is run from APSIM Next Generation, Visual Studio will stop on the above line and you will be able to inspect values of variables and step into/over lines of code.
