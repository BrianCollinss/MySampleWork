@echo off
REM Upload a freshly generated gzip batch into the SQL Snowpipe prefix.
CALL "%UserProfile%\miniconda3\condabin\conda.bat" activate snowflake-project
python scripts\push_mock_batch_to_s3.py --target-prefix sql
