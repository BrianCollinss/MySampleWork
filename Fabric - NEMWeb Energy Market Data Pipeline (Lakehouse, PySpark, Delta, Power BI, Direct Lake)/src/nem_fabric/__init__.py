"""Project package for AEMO NEMWeb ingestion and Fabric Lakehouse modelling.

Module prefixes describe runtime dependencies:

- `common_`: shared code that can run locally or in Microsoft Fabric.
- `fabric_`: Fabric/Spark/Lakehouse-specific implementations.
- `local_`: local filesystem or developer-machine implementations.
"""

__all__ = ["__version__"]

# Keep the package version simple so notebooks and smoke tests can report the
# code revision they imported without needing packaging metadata.
__version__ = "0.1.0"
