-- Helper reset script: remove the Snowflake objects created by this demo so the
-- project can be rerun from a clean starting point.
--
-- Run `sql_00_session_init.sql` first, then review the values below before
-- executing this file. This script is intentionally separate from the numbered
-- build path because it is an operational cleanup action, not a pipeline step.
--
-- What this removes:
-- 1. The project database, which also removes the Bronze, Silver, and Gold
--    schemas plus all `sql_` and `py_` objects inside them.
-- 2. The storage integration created for the Snowpipe demo, which is account-
--    level and therefore must be dropped separately.
--
-- What this does not remove:
-- - AWS resources such as the S3 bucket, SNS topic, IAM role, or IAM policies.
-- - Local files, Python environment files, or anything outside Snowflake.
--
-- Success:
-- - `SHOW DATABASES LIKE 'TRAINING_0001'` returns no rows after the drop.
-- - `SHOW INTEGRATIONS LIKE 'RESUME_S3_INT'` returns no rows after the drop.

SET reset_database = $project_database;
SET reset_storage_integration = $aws_storage_integration;
SET drop_database_sql = 'DROP DATABASE IF EXISTS ' || $reset_database;
SET drop_integration_sql = 'DROP INTEGRATION IF EXISTS ' || $reset_storage_integration;

-- Use an account-capable role because storage integrations are account-level objects.
USE ROLE ACCOUNTADMIN;
USE DATABASE SNOWFLAKE;

-- Dropping the database removes all schemas, tables, stages, streams, tasks, pipes, and views created in it.
EXECUTE IMMEDIATE $drop_database_sql;
-- Success: the project database no longer appears in `SHOW DATABASES`.

-- Drop the storage integration separately because it lives outside the database.
EXECUTE IMMEDIATE $drop_integration_sql;
-- Success: the storage integration no longer appears in `SHOW INTEGRATIONS`.

SHOW DATABASES LIKE 'TRAINING_0001';
SHOW INTEGRATIONS LIKE 'RESUME_S3_INT';
