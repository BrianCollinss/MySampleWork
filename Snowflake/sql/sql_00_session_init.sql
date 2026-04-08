-- Session init: run this once at the top of a Snowflake worksheet/session when
-- you want to execute the SQL twin steps manually without repeating the same
-- SET and USE statements before every step body.
--
-- The later sql_01 through sql_07 files assume this setup has already been run
-- in the current session. This file intentionally does not `USE DATABASE
-- IDENTIFIER($project_database)` because Step 01 is responsible for creating
-- that database on a clean run.

SET project_role = 'ACCOUNTADMIN';
SET project_warehouse = 'COMPUTE_WH';
SET project_database = 'TRAINING_0001';
SET project_schema = 'PIPELINE';
SET output_prefix = 'sql';
SET local_data_dir = './data';
SET aws_s3_bucket_url = 's3://bc-snowflake-training-0001';
SET aws_storage_integration = 'resume_s3_int';
SET aws_storage_aws_role_arn = 'arn:aws:iam::472506472624:role/bc-snowflake-training-0001';
SET aws_sns_topic_arn = 'arn:aws:sns:ap-southeast-2:472506472624:bc-snowflake-training-0001';
SET aws_region = 'ap-southeast-2';

USE ROLE IDENTIFIER($project_role);
USE WAREHOUSE IDENTIFIER($project_warehouse);
