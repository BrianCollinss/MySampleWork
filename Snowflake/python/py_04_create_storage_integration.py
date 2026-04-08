"""Snowpark storage integration setup for the Python pipeline.

Step 04 purpose:
1. Point Snowflake at the AWS IAM role that can read the S3 bucket.
2. Restrict the integration to the project bucket root.
3. Stop here so AWS trust can be updated before any stage or pipe is created.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from snowflake_project.connection import get_snowpark_session
from snowflake_project.project import load_project_settings, run_sql, set_session_context


def main() -> None:
    settings = load_project_settings(output_prefix="py")

    with get_snowpark_session() as session:
        set_session_context(session, settings)

        # Match the SQL pipeline by creating only the storage integration in this step.
        run_sql(
            session,
            f"""
            CREATE OR REPLACE STORAGE INTEGRATION {settings.aws_storage_integration}
            TYPE = EXTERNAL_STAGE
            STORAGE_PROVIDER = S3
            ENABLED = TRUE
            STORAGE_AWS_ROLE_ARN = '{settings.aws_storage_aws_role_arn}'
            STORAGE_ALLOWED_LOCATIONS = ('{settings.aws_s3_bucket_url.rstrip('/')}/')
            """,
            "py create storage integration",
        )


if __name__ == "__main__":
    main()
