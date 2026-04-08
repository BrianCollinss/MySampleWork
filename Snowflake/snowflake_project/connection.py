"""Connection helpers shared by every Python twin step in the project.

The goal of this module is to make the authentication path explicit:
1. Load the project-local .env file.
2. Validate the minimum Snowflake settings required to open a session.
3. Choose either password auth or browser-based SSO.
4. Return a connector session that the step runner can reuse.
"""

import os
from pathlib import Path
from typing import cast

from dotenv import load_dotenv
import snowflake.connector as snowflake
from snowflake.connector.errors import Error
from snowflake.snowpark import Session


PROJECT_ROOT = Path(__file__).resolve().parents[1]
ENV_PATH = PROJECT_ROOT / ".env"
REQUIRED_VARS = ("SNOWFLAKE_USER", "SNOWFLAKE_ACCOUNT")


def load_environment() -> None:
    # Every pipeline step reads the same project-local environment file so the
    # SQL and Python twins stay pointed at the same Snowflake account.
    load_dotenv(dotenv_path=ENV_PATH)


def get_env(name: str, default: str | None = None) -> str | None:
    value = os.getenv(name, default)
    return value.strip() if isinstance(value, str) else value


def get_connection():
    # Fail before opening a session if the core account identity is incomplete.
    missing = [name for name in REQUIRED_VARS if not get_env(name)]
    if missing:
        raise ValueError(f"Missing required environment variables: {', '.join(missing)}")

    # Start with the connection settings shared by both auth modes.
    user = get_env("SNOWFLAKE_USER")
    account = get_env("SNOWFLAKE_ACCOUNT")
    if user is None or account is None:
        raise ValueError(f"Missing required environment variables: {', '.join(REQUIRED_VARS)}")

    connection_kwargs: dict[str, str] = {
        "user": user,
        "account": account,
    }

    # Respect the configured role so object creation happens in the intended security context.
    role = get_env("SNOWFLAKE_ROLE")
    if role:
        connection_kwargs["role"] = role

    # Pass the warehouse through up front because several pipeline steps create tasks and run CTAS-style loads.
    warehouse = get_env("SNOWFLAKE_WAREHOUSE")
    if warehouse:
        connection_kwargs["warehouse"] = warehouse

    authenticator = get_env("SNOWFLAKE_AUTHENTICATOR")
    if authenticator == "externalbrowser":
        # This path is for SSO users who sign in through their browser instead of storing a Snowflake password locally.
        connection_kwargs["authenticator"] = authenticator
        return snowflake.connect(**connection_kwargs)

    # Password auth is the default path when no explicit browser-based SSO authenticator is configured.
    password = get_env("SNOWFLAKE_PASSWORD")
    if not password:
        raise ValueError(
            "Missing required environment variable: SNOWFLAKE_PASSWORD "
            "(or set SNOWFLAKE_AUTHENTICATOR=externalbrowser)."
        )

    connection_kwargs["password"] = password
    return snowflake.connect(**connection_kwargs)


def get_connection_parameters() -> dict[str, str]:
    """Return the Snowflake connection settings shared by connector and Snowpark."""
    load_environment()

    missing = [name for name in REQUIRED_VARS if not get_env(name)]
    if missing:
        raise ValueError(f"Missing required environment variables: {', '.join(missing)}")

    user = get_env("SNOWFLAKE_USER")
    account = get_env("SNOWFLAKE_ACCOUNT")
    if user is None or account is None:
        raise ValueError(f"Missing required environment variables: {', '.join(REQUIRED_VARS)}")

    connection_parameters: dict[str, str] = {
        "user": user,
        "account": account,
    }

    role = get_env("SNOWFLAKE_ROLE")
    if role:
        connection_parameters["role"] = role

    warehouse = get_env("SNOWFLAKE_WAREHOUSE")
    if warehouse:
        connection_parameters["warehouse"] = warehouse

    authenticator = get_env("SNOWFLAKE_AUTHENTICATOR")
    if authenticator == "externalbrowser":
        connection_parameters["authenticator"] = authenticator
        return connection_parameters

    password = get_env("SNOWFLAKE_PASSWORD")
    if not password:
        raise ValueError(
            "Missing required environment variable: SNOWFLAKE_PASSWORD "
            "(or set SNOWFLAKE_AUTHENTICATOR=externalbrowser)."
        )

    connection_parameters["password"] = password
    return connection_parameters


def get_snowpark_session() -> Session:
    """Create a Snowpark session for the Python twin pipeline."""
    # Snowpark types `configs` as accepting `dict[str, int | str]`. Our project
    # only supplies string values, which is a valid runtime subset, so we cast
    # here to satisfy the static type checker without weakening the helper API.
    return Session.builder.configs(cast(dict[str, int | str], get_connection_parameters())).create()


def main() -> None:
    try:
        # Load the environment before printing settings so the console output reflects the actual runtime config.
        load_environment()
        auth_mode = get_env("SNOWFLAKE_AUTHENTICATOR", "password")
        print(
            "Attempting Snowflake connection with "
            f"account={get_env('SNOWFLAKE_ACCOUNT')}, "
            f"user={get_env('SNOWFLAKE_USER')}, "
            f"role={get_env('SNOWFLAKE_ROLE') or 'default'}, "
            f"warehouse={get_env('SNOWFLAKE_WAREHOUSE') or 'default'}, "
            f"authenticator={auth_mode}"
        )

        with get_connection() as conn:
            with conn.cursor() as cur:
                # Use a lightweight metadata query so connectivity can be verified without modifying any objects.
                cur.execute("SELECT CURRENT_VERSION()")
                row = cur.fetchone()
                if row is None:
                    raise ValueError("The Snowflake version query returned no results.")
                version = row[0]

        print(f"Connected to Snowflake successfully. Version: {version}")
    except (ValueError, Error) as exc:
        # Exit non-zero so setup failures are obvious in terminals, scripts, and CI logs.
        print(f"Snowflake connection failed: {exc}")
        raise SystemExit(1) from exc
