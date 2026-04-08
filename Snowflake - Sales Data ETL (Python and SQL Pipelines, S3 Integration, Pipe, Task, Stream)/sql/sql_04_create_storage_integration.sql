-- Step 04: create or update the Snowflake storage integration only.
-- Process:
-- 1. Point Snowflake at the AWS IAM role that can read the S3 bucket.
-- 2. Restrict the integration to the project bucket root.
-- 3. Stop here so AWS trust can be updated before any stage or pipe is created.
--
-- Important:
-- Re-running this step can change STORAGE_AWS_EXTERNAL_ID.
-- If that happens, rerun sql_05_storage_integration_check.sql and update the
-- AWS IAM role trust relationship before moving on to Step 06.

EXECUTE IMMEDIATE
$$
DECLARE
  allowed_location STRING := RTRIM($aws_s3_bucket_url, '/') || '/';
  create_integration_sql STRING;
BEGIN
  create_integration_sql := 'CREATE OR REPLACE STORAGE INTEGRATION ' || $aws_storage_integration || ' '
    || 'TYPE = EXTERNAL_STAGE STORAGE_PROVIDER = S3 ENABLED = TRUE '
    || 'STORAGE_AWS_ROLE_ARN = ''' || $aws_storage_aws_role_arn || ''' '
    || 'STORAGE_ALLOWED_LOCATIONS = (''' || allowed_location || ''')';

  EXECUTE IMMEDIATE create_integration_sql;
END;
$$;
