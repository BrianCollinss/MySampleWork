# AWS And Snowflake Snowpipe Setup

This guide explains the cloud-side work needed for Step 04 through Step 07. The
goal is to make the AWS and Snowflake handoff explicit, because those steps are
where most first-run issues occur.

## What gets created where

Snowflake creates:
- a storage integration
- an external S3 stage
- a Snowpipe
- a Bronze stream
- a Silver task

AWS provides:
- an S3 bucket and prefixes
- an IAM role that Snowflake can assume
- an SNS topic for auto-ingest notifications
- an S3 event notification that publishes file-arrival events to SNS
- local AWS credentials for the upload helper script

## Before you start

Confirm these project values:

- Snowflake database: `TRAINING_0001`
- S3 bucket: `bc-snowflake-training-0001`
- S3 prefixes: `sql/` and `py/`
- AWS region: `ap-southeast-2`
- IAM role name: `bc-snowflake-training-0001`
- SNS topic ARN: `arn:aws:sns:ap-southeast-2:472506472624:bc-snowflake-training-0001`
- Storage integration name: `resume_s3_int`

## 1. Create the AWS bucket structure

Create or reuse the bucket:

- `s3://bc-snowflake-training-0001`

Use these prefixes:

- `sql/`
- `py/`

These prefixes are important because the SQL and Snowpark twins intentionally
watch different S3 paths.

## 2. Create the IAM role Snowflake will assume

Create an IAM role named:

- `bc-snowflake-training-0001`

Attach an S3 read policy to that role. This policy is for Snowflake, not for
your local uploader user.

```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Sid": "AllowBucketList",
      "Effect": "Allow",
      "Action": [
        "s3:ListBucket"
      ],
      "Resource": "arn:aws:s3:::bc-snowflake-training-0001",
      "Condition": {
        "StringLike": {
          "s3:prefix": [
            "sql/*",
            "py/*"
          ]
        }
      }
    },
    {
      "Sid": "AllowObjectRead",
      "Effect": "Allow",
      "Action": [
        "s3:GetObject",
        "s3:GetObjectVersion"
      ],
      "Resource": [
        "arn:aws:s3:::bc-snowflake-training-0001/sql/*",
        "arn:aws:s3:::bc-snowflake-training-0001/py/*"
      ]
    }
  ]
}
```

Do not fill in the role trust relationship yet. Snowflake generates the exact
principal and external ID later, after the storage integration exists.

## 3. Create the SNS topic

Create an SNS topic named:

- `bc-snowflake-training-0001`

Initial topic properties:
- Type: `Standard`
- Region: `ap-southeast-2`

You do not create the Snowflake subscription manually. Snowflake creates that
subscription when the pipe is created successfully with `AUTO_INGEST = TRUE`.

## 4. Run Snowflake Step 04 to create the storage integration

Run:

- `sql/sql_04_create_storage_integration.sql`

Use a role with integration privileges, usually `ACCOUNTADMIN`.

Why this step matters:
- it creates the Snowflake storage integration
- it tells Snowflake which IAM role to assume
- it generates the Snowflake-side IAM user ARN and external ID that AWS must trust

Important:
- avoid rerunning Step 04 unless you really need to
- recreating the integration can rotate the external ID and force another AWS trust update

## 5. Run Snowflake Step 05 and capture the trust values

Run:

- `sql/sql_05_storage_integration_check.sql`

It runs:

```sql
DESC INTEGRATION IDENTIFIER($aws_storage_integration);
```

From the output, capture:

- `STORAGE_AWS_IAM_USER_ARN`
- `STORAGE_AWS_EXTERNAL_ID`
- `STORAGE_AWS_ROLE_ARN`

These values must match the AWS trust relationship exactly.

## 6. Update the IAM role trust relationship

Update the trust relationship on the IAM role `bc-snowflake-training-0001`.

Pattern:

```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Principal": {
        "AWS": "STORAGE_AWS_IAM_USER_ARN"
      },
      "Action": "sts:AssumeRole",
      "Condition": {
        "StringEquals": {
          "sts:ExternalId": "STORAGE_AWS_EXTERNAL_ID"
        }
      }
    }
  ]
}
```

Replace the placeholders with the exact values from `DESC INTEGRATION`.

Success looks like:
- Snowflake can assume the AWS role without `sts:AssumeRole` errors

## 7. Update the SNS topic policy

The topic must allow:
- Snowflake to subscribe
- S3 to publish object-created notifications

Use the latest Snowflake principal from the current integration when you set the
`AllowSnowflakeSubscribe` statement.

Pattern:

```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Sid": "AllowTopicOwnerAdmin",
      "Effect": "Allow",
      "Principal": {
        "AWS": "arn:aws:iam::472506472624:root"
      },
      "Action": [
        "SNS:Publish",
        "SNS:RemovePermission",
        "SNS:SetTopicAttributes",
        "SNS:DeleteTopic",
        "SNS:ListSubscriptionsByTopic",
        "SNS:GetTopicAttributes",
        "SNS:AddPermission",
        "SNS:Subscribe"
      ],
      "Resource": "arn:aws:sns:ap-southeast-2:472506472624:bc-snowflake-training-0001"
    },
    {
      "Sid": "AllowSnowflakeSubscribe",
      "Effect": "Allow",
      "Principal": {
        "AWS": "CURRENT_STORAGE_AWS_IAM_USER_ARN"
      },
      "Action": "SNS:Subscribe",
      "Resource": "arn:aws:sns:ap-southeast-2:472506472624:bc-snowflake-training-0001"
    },
    {
      "Sid": "AllowS3Publish",
      "Effect": "Allow",
      "Principal": {
        "Service": "s3.amazonaws.com"
      },
      "Action": "SNS:Publish",
      "Resource": "arn:aws:sns:ap-southeast-2:472506472624:bc-snowflake-training-0001",
      "Condition": {
        "ArnLike": {
          "aws:SourceArn": "arn:aws:s3:::bc-snowflake-training-0001"
        }
      }
    }
  ]
}
```

Success looks like:
- Step 06 no longer fails with `SNS:Subscribe` authorization errors

## 8. Configure the S3 event notification

Add an S3 event notification on bucket `bc-snowflake-training-0001`:

- Event type: `ObjectCreated`
- Destination: the SNS topic `bc-snowflake-training-0001`

Recommended filters:
- Prefix: `sql/` for the SQL test path
- Prefix: `py/` for the Python test path
- Suffix: `.csv.gz`

Why the suffix matters:
- the helper uploader sends compressed files ending in `.csv.gz`
- this matches the file pattern expected by the Snowpipe demo

Success looks like:
- new uploads produce SNS notifications that Snowpipe can consume automatically

## 9. Create the Snowpipe objects

Run:

- `sql/sql_06_snowpipe_external.sql`
or
- `python/py_06_snowpipe_external.py`

This creates:
- the external stage
- the pipe
- the Bronze stream
- the Silver task

Success looks like:
- the file runs without AWS authorization errors
- Snowflake auto-creates a subscription on the SNS topic

## 10. Configure local AWS credentials for the uploader

The shared upload helper is:

- `scripts/push_mock_batch_to_s3.py`

It uses `boto3`, so your local machine must have AWS credentials.

Two common options:

### Option A: named AWS profile

Set in `.env`:

```env
AWS_PROFILE=bc-snowflake-training-0001
AWS_REGION=ap-southeast-2
```

If you use IAM access keys:

```bash
aws configure --profile bc-snowflake-training-0001
```

If you use SSO and actually have an SSO start URL from an organization:

```bash
aws configure sso --profile bc-snowflake-training-0001
aws sso login --profile bc-snowflake-training-0001
```

### Option B: raw environment variables

Set in `.env`:

```env
AWS_ACCESS_KEY_ID=...
AWS_SECRET_ACCESS_KEY=...
AWS_SESSION_TOKEN=...
AWS_REGION=ap-southeast-2
```

Important:
- `AWS_ACCESS_KEY_ID` is not your AWS account ID
- it usually looks like `AKIA...` or `ASIA...`

## 11. Give your local uploader identity write access

This is separate from the Snowflake-assumed IAM role.

The local uploader identity needs:
- `s3:PutObject`
- usually `s3:GetObject`
- usually `s3:ListBucket`

For example, if your local AWS profile resolves to user `BrianCollins1`, that
user or its role must have permission to upload into:

- `arn:aws:s3:::bc-snowflake-training-0001/sql/*`
- `arn:aws:s3:::bc-snowflake-training-0001/py/*`

Without that, the uploader will fail even if Snowflake itself is configured correctly.

## 12. Upload a test file

Use the shared helper:

```bash
python scripts/push_mock_batch_to_s3.py --target-prefix sql
python scripts/push_mock_batch_to_s3.py --target-prefix py
```

This compresses `data/mock_orders_seed.csv` and uploads it as a `.csv.gz` file.

Success looks like:
- the script prints the final `s3://...` path
- the object appears under the expected prefix in S3

## 13. Validate the Snowpipe flow

Run:

- `sql/sql_07_validate_snowpipe_objects.sql`

What success looks like:
- `LIST @...` shows the uploaded file
- `SYSTEM$PIPE_STATUS(...)` shows `RUNNING`
- `lastIngestedFilePath` is populated after a load
- Bronze row count is greater than `0`
- `COPY_HISTORY` shows `STATUS = Loaded`
- Silver rows appear with `load_method = 'SNOWPIPE'`

## Common failure patterns

`Error assuming AWS_ROLE`
- The IAM trust relationship does not match the current `DESC INTEGRATION` output.

`SNS:Subscribe` denied
- The SNS topic policy does not allow the current Snowflake AWS principal.

File uploads to S3 but Bronze stays empty
- The file went to the wrong prefix.
- The S3 event notification is missing.
- The SNS subscription was not created.
- The file suffix does not match `.csv.gz`.

`AccessDenied` from the uploader script
- Your local AWS identity lacks `s3:PutObject`.
- This is separate from the Snowflake-assumed IAM role.
