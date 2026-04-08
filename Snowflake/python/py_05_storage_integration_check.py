"""Snowpark storage integration inspection for the Python pipeline.

Step 05 purpose:
1. Describe the storage integration created for S3 access.
2. Surface the Snowflake-managed IAM user ARN and external ID.
3. Use those values when configuring or validating the AWS IAM role trust policy.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from snowflake_project.connection import get_snowpark_session
from snowflake_project.project import load_project_settings, set_session_context


def main() -> None:
    settings = load_project_settings(output_prefix="py")

    with get_snowpark_session() as session:
        set_session_context(session, settings)

        rows = session.sql(f"DESC INTEGRATION {settings.aws_storage_integration}").collect()
        for row in rows:
            print(row)


if __name__ == "__main__":
    main()
