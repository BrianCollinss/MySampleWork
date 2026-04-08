"""Shared project settings and helper functions for the SQL/Snowpark twin pipeline."""

from dataclasses import dataclass
from pathlib import Path

from snowflake_project.connection import get_env, load_environment


PROJECT_ROOT = Path(__file__).resolve().parents[1]


@dataclass(frozen=True)
class ProjectSettings:
    project_role: str
    project_warehouse: str
    project_database: str
    project_schema: str
    output_prefix: str
    local_data_dir: str
    aws_s3_bucket_url: str
    aws_storage_integration: str
    aws_storage_aws_role_arn: str
    aws_sns_topic_arn: str
    aws_region: str

    @property
    def bronze_schema(self) -> str:
        return f"{self.project_database}.BRONZE"

    @property
    def silver_schema(self) -> str:
        return f"{self.project_database}.SILVER"

    @property
    def gold_schema(self) -> str:
        return f"{self.project_database}.GOLD"

    def object_name(self, schema: str, suffix: str) -> str:
        return f"{schema}.{self.output_prefix}_{suffix}"

    def bronze_object(self, suffix: str) -> str:
        return self.object_name(self.bronze_schema, suffix)

    def silver_object(self, suffix: str) -> str:
        return self.object_name(self.silver_schema, suffix)

    def gold_object(self, suffix: str) -> str:
        return self.object_name(self.gold_schema, suffix)

    @property
    def local_seed_file(self) -> str:
        return str((PROJECT_ROOT / self.local_data_dir / "mock_orders_seed.csv").resolve()).replace("\\", "/")

    @property
    def s3_stage_url(self) -> str:
        return f"{self.aws_s3_bucket_url.rstrip('/')}/{self.output_prefix}/"


def load_project_settings(output_prefix: str) -> ProjectSettings:
    """Load the pipeline settings for a specific twin path."""
    load_environment()
    return ProjectSettings(
        project_role=get_env("SNOWFLAKE_ROLE", "SYSADMIN") or "SYSADMIN",
        project_warehouse=get_env("PROJECT_WAREHOUSE", get_env("SNOWFLAKE_WAREHOUSE", "COMPUTE_WH")) or "COMPUTE_WH",
        project_database=get_env("PROJECT_DATABASE", "TRAINING_0001") or "TRAINING_0001",
        project_schema=get_env("PROJECT_SCHEMA", "PIPELINE") or "PIPELINE",
        output_prefix=output_prefix,
        local_data_dir=(get_env("LOCAL_DATA_DIR", "./data") or "./data").replace("\\", "/").strip(),
        aws_s3_bucket_url=get_env("AWS_S3_BUCKET_URL", "s3://bc-snowflake-training-0001") or "s3://bc-snowflake-training-0001",
        aws_storage_integration=get_env("AWS_STORAGE_INTEGRATION", "resume_s3_int") or "resume_s3_int",
        aws_storage_aws_role_arn=get_env(
            "AWS_STORAGE_AWS_ROLE_ARN",
            "arn:aws:iam::472506472624:role/bc-snowflake-training-0001",
        ) or "arn:aws:iam::472506472624:role/bc-snowflake-training-0001",
        aws_sns_topic_arn=get_env(
            "AWS_SNS_TOPIC_ARN",
            "arn:aws:sns:ap-southeast-2:472506472624:bc-snowflake-training-0001",
        ) or "arn:aws:sns:ap-southeast-2:472506472624:bc-snowflake-training-0001",
        aws_region=get_env("AWS_REGION", "ap-southeast-2") or "ap-southeast-2",
    )


def set_session_context(session, settings: ProjectSettings) -> None:
    """Apply role, warehouse, and database context for Snowpark steps."""
    session.sql(f"USE ROLE {settings.project_role}").collect()
    session.sql(f"USE WAREHOUSE {settings.project_warehouse}").collect()
    session.sql(f"USE DATABASE {settings.project_database}").collect()


def run_sql(session, statement: str, label: str) -> None:
    """Execute a SQL statement with a short progress label."""
    print(f"Running [{label}] {' '.join(statement.split())[:120]}")
    session.sql(statement).collect()
