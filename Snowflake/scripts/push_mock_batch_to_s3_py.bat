@echo off
REM Upload a freshly generated gzip batch into the PY Snowpipe prefix.
python scripts\push_mock_batch_to_s3.py --target-prefix py
