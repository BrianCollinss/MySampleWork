# APSIM Next Generation

Source URL: https://apsimnextgeneration.netlify.app/usage/tests
Source domain: apsimnextgeneration.netlify.app
Fetched at: 2026-04-19T09:51:30.147285+00:00

The next generation of APSIM
>
Usage
> Test Results
Test Results
To test your data, you can add something like this to a manager script:
using System;
using Models.Core;
namespace Models
{
[Serializable]
public class Script : Model, ITest
{
public void Run()
{
bool testHasFailed = true;
if (testHasFailed)
throw new Exception("oh dear");
}
}
}
Things to note:
Any failures should be handled by throwing an exception.
Whenever Apsim finishes running simulations, it will run all
IModel
s which implement the
ITest
interface. Therefore, the script must inherit from
Model
ITest
is defined in the
Models.Core
namespace
By default, Apsim will not run these tests when run from the command line, unless the
--run-tests
command line switch is provided. If you don’t know what the command line is, this will not affect you.
Jenkins
will run these tests whenever a pull request is run. Therefore, all files under the Examples, Prototypes, and Tests directories are guaranteed to be tested.
