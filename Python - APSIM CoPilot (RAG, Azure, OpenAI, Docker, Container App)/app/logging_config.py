"""Logging and telemetry setup for APSIM Copilot.

The app uses standard Python logging and optionally enables Azure Monitor when
an Application Insights connection string is available. Verbose Azure SDK
loggers are pushed down to WARNING so local development stays readable.
"""

from __future__ import annotations

import logging

from app.config import AppConfig

try:
    from azure.monitor.opentelemetry import configure_azure_monitor
except ImportError:  # pragma: no cover
    configure_azure_monitor = None

_AZURE_MONITOR_CONFIGURED = False


def configure_logging(config: AppConfig) -> logging.Logger:
    """Configure application logging and optional telemetry integration."""
    global _AZURE_MONITOR_CONFIGURED

    logging.basicConfig(
        level=getattr(logging, config.log_level.upper(), logging.INFO),
        format="%(asctime)s %(levelname)s [%(name)s] %(message)s",
    )
    # Keep framework noise under control while still surfacing warnings/errors.
    logging.getLogger("azure.core.pipeline.policies.http_logging_policy").setLevel(logging.WARNING)
    logging.getLogger("azure.monitor.opentelemetry").setLevel(logging.WARNING)
    logging.getLogger("opentelemetry").setLevel(logging.WARNING)
    logger = logging.getLogger(config.app_name)
    if config.has_app_insights() and configure_azure_monitor is not None and not _AZURE_MONITOR_CONFIGURED:
        try:
            configure_azure_monitor(connection_string=config.applicationinsights_connection_string)
            _AZURE_MONITOR_CONFIGURED = True
            logger.info("Azure Monitor instrumentation enabled.")
        except Exception as exc:  # pragma: no cover
            logger.warning("Failed to enable Azure Monitor instrumentation: %s", exc)
    return logger


def get_logger(name: str | None = None) -> logging.Logger:
    """Return a namespaced logger with a sensible application fallback."""
    return logging.getLogger(name or "apsim_copilot")
