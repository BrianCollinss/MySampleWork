"""Snowpark reset helper for the Python project.

Step purpose:
1. Switch to a safe database so the project database can be dropped cleanly.
2. Drop the project database, which removes the Bronze, Silver, and Gold schemas
   plus all `sql_` and `py_` objects inside them.
3. Drop the storage integration separately because it is an account-level object.

This is an operational cleanup helper, not part of the numbered build pipeline.
AWS resources such as the S3 bucket, SNS topic, IAM role, and IAM policies are
not removed by this script.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from snowflake_project.connection import get_snowpark_session
from snowflake_project.project import load_project_settings, run_sql


def main() -> None:
    settings = load_project_settings(output_prefix="py")

    with get_snowpark_session() as session:
        # Use the configured role first, then move into a stable database so
        # Snowsight or other clients do not stay attached to the database being dropped.
        run_sql(session, f"USE ROLE {settings.project_role}", "py reset use role")
        run_sql(session, "USE DATABASE SNOWFLAKE", "py reset use safe database")

        # Dropping the project database removes all demo schemas, tables, stages,
        # streams, tasks, pipes, and views created inside that database.
        run_sql(
            session,
            f"DROP DATABASE IF EXISTS {settings.project_database}",
            "py reset drop project database",
        )

        # The storage integration lives outside the database, so it must be
        # cleaned up separately when you want a full Snowpipe reset.
        run_sql(
            session,
            f"DROP INTEGRATION IF EXISTS {settings.aws_storage_integration}",
            "py reset drop storage integration",
        )

        # These checks confirm the reset worked without requiring manual queries.
        print(session.sql(f"SHOW DATABASES LIKE '{settings.project_database}'").collect())
        print(session.sql(f"SHOW INTEGRATIONS LIKE '{settings.aws_storage_integration.upper()}'").collect())


if __name__ == "__main__":
    main()
