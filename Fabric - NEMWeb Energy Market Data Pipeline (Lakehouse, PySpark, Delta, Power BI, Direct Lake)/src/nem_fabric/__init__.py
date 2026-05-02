"""Project package for AEMO NEMWeb ingestion and Fabric Lakehouse modelling.

The modules under this package are reusable from both local smoke tests and
Microsoft Fabric notebooks. Local code handles lightweight HTTP, parsing, and
pandas checks; Fabric notebooks are expected to use Spark for Lakehouse-scale
Delta writes.
"""

__all__ = ["__version__"]

# Keep the package version simple so notebooks and smoke tests can report the
# code revision they imported without needing packaging metadata.
__version__ = "0.1.0"
