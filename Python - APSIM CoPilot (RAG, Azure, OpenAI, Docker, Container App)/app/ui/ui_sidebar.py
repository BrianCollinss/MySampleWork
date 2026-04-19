"""Sidebar rendering helpers for the Streamlit UI.

The sidebar is intentionally simple and operational: it shows whether the app
can currently reach each external dependency and which runtime settings are in
effect for the current session.
"""

from __future__ import annotations

import streamlit as st

from app.config import AppConfig


def render_sidebar(config: AppConfig) -> None:
    """Render the configuration and runtime status sidebar."""
    st.sidebar.title("APSIM Copilot")
    st.sidebar.caption("OpenAI + Azure APSIM assistant demo")
    st.sidebar.subheader("Configuration")
    st.sidebar.write(f"OpenAI API: {'Ready' if config.has_openai() else 'Missing'}")
    st.sidebar.write(f"Azure AI Search: {'Ready' if config.has_search() else 'Missing'}")
    st.sidebar.write(f"Azure Blob Storage: {'Ready' if config.has_blob() else 'Missing'}")
    st.sidebar.write(f"Application Insights: {'Ready' if config.has_app_insights() else 'Optional / Missing'}")
    st.sidebar.subheader("Runtime")
    st.sidebar.write(f"Mode: `{config.deployment_mode}`")
    st.sidebar.write(f"Search index: `{config.azure_search_index_name}`")
    st.sidebar.write(f"Blob container: `{config.azure_storage_container_name}`")
