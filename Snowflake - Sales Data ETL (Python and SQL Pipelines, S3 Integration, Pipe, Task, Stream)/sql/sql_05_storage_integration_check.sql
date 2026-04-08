-- Step 05 helper: inspect the Snowflake storage integration after Step 04.
-- Purpose:
-- 1. Describe the storage integration created for S3 access.
-- 2. Surface the Snowflake-managed IAM user ARN and external ID.
-- 3. Use those values when configuring or validating the AWS IAM role trust policy.
--
-- Run this after sql_04_create_storage_integration.sql.
-- This file is intentionally separate from the main pipeline numbering because
-- it is an operational verification step, not a data pipeline transformation.

DESC INTEGRATION IDENTIFIER($aws_storage_integration);

-- After running DESC INTEGRATION, capture these two output rows:
-- 1. STORAGE_AWS_IAM_USER_ARN
-- 2. STORAGE_AWS_EXTERNAL_ID
--
-- Use them in the AWS IAM role trust policy. The Snowflake side creates the
-- integration object, but AWS still needs to trust Snowflake explicitly.
