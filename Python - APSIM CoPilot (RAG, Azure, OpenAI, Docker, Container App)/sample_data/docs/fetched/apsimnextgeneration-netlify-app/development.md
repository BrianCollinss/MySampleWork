# APSIM Next Generation

Source URL: https://apsimnextgeneration.netlify.app/development
Source domain: apsimnextgeneration.netlify.app
Fetched at: 2026-04-19T09:51:32.308060+00:00

The next generation of APSIM
> Development
Development
For changes to be accepted by the APSIM Initiaive Reference Panel submissions must adhere to software and science guidelines.
All submissions that contain source code changes will be subject to a software peer review that ensures all software guildlines have been met.
All submissions that contain major changes to model science e.g. new models or new processes (labeled as ‘Major’ in GitHub) will undergo peer-review by at least one independent reviewer. The Reference Panel will manage this process. The science reviewer(s) will be ensure all science guildlines have been met.
Reasons need to be given by the model author should any of the guidelines not be followed.
Submission Guidelines
Submissions will be via a
GitHub Pull Request
Where possible, submissions should be small. For example, rather than adding a new model validation dataset and changing the parameterisation of a model, separate these into 2 submissions. This will allow the impacts of the new dataset and the changed parameterisation to be independently assessed.
For science submissions (new models or processes), the submission pull request will have all files (.apsimx, .met, .xlsx) in a directory formatted as Tests\UnderReview\MODELNAME. The directory can contain:
weather files (*.met)
observed files (*.xlsx)
MODELNAME.apsimx (validation simulations)
MODELNAME Example.apsimx (example simulations)
If a new model has been submitted, it will be under a
Replacements
node in the MODELNAME.apsimx and MODELNAME Example.apsimx files.
Software Guidelines
Coding style
Model design
User interface design
PMF model design
Software interfaces
Unit tests
Science guidelines
Testing
Documentation
Examples
