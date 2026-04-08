"""Entry point for the standalone Snowflake connectivity check.

This file stays intentionally small so anyone opening the project can run a
single script to confirm credentials, role, and warehouse access before moving
on to the medallion pipeline steps.
"""

from snowflake_project.connection import main


if __name__ == "__main__":
    # Run the shared connection test that validates the current .env settings.
    main()
