@echo off
REM Upload a freshly generated gzip batch into the SQL Snowpipe prefix.
python scripts\push_mock_batch_to_s3.py --target-prefix sql
