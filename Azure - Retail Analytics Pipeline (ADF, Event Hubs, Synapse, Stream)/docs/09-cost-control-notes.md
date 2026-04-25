# Cost Control Notes

## Principles

- Use serverless SQL only
- Run streaming components only while testing or processing events
- Keep sample data small
- Prefer short-lived resources

## Common Cost Mistakes

- Leaving Stream Analytics running overnight
- Over-provisioning Event Hubs throughput
- Replaying event data continuously without a stop condition
- Keeping unused Azure resources after a run

## Low-Cost Recommendations

- Use one storage account
- Use one Event Hub
- Use minimal retention and throughput settings
- Use a short replay file instead of infinite generation when possible

## What Is Done Locally

- Use finite replay settings
- Keep event volume moderate

## What Is Done In Azure Portal

- Review pricing tiers before creation
- Delete the resource group after validation if appropriate

## What Is Done In Service UI

- Stop Stream Analytics immediately after validation

## What Is Provided In Repo

- Small sample datasets
- Finite replay tooling
- Build order guidance
