# APSIM Next Generation

Source URL: https://apsimnextgeneration.netlify.app/development/software/pmfdesign
Source domain: apsimnextgeneration.netlify.app
Fetched at: 2026-04-19T09:51:35.360145+00:00

The next generation of APSIM
>
Development
>
Software
> PMF code design
PMF code design
In addition to the
Root models must allow for multi-point root systems. This is provided when
IUptake
is implemented and the standard
Root
class is used. If an alternate
Root
model is used, it needs to allow for multi-point root systems.
Plant models must provide CO2 impacts. If the user changes CO2 in the weather component the plant model should respond.
Plant models must use the transpiration value provided by MicroClimate. Without this, intercropping will not be possible.
