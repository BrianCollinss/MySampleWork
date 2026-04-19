# APSIM Next Generation

Source URL: https://apsimnextgeneration.netlify.app/usage/commandline/commandlanguage-old-to-new
Source domain: apsimnextgeneration.netlify.app
Fetched at: 2026-04-19T09:51:24.780708+00:00

The next generation of APSIM
>
Usage
>
Command Line
> APSIM Command Language (old to new)
APSIM Command Language (old to new)
The APSIM command language has changed to make it more intuitive.
The new language is described here
. Most of the changes are to the
add
command. The other commands remain unchanged.
Examples that show how to convert new old syntax to the new syntax.
Old -> New
add
- add a new or existing model to another model.
add [Zone] Report
->
add new Report to [Zone]
add [Zone] Report MyReport
->
add new Report to [Zone] name MyReport
add [Zone] soils.apsimx;[Soil1] Soil
->
add [Soil1] from soils.apsimx to [Zone] name Soil
duplicate
- duplicate a model.
duplicate [Zone].Report NewReport
->
duplicate [Zone].Report name NewReport
comment lines
- only
#
is supported as a comment character.
