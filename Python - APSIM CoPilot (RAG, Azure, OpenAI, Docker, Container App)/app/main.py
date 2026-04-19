"""Streamlit entrypoint for APSIM Copilot.

This module wires together configuration, external services, and the three UI
workflows: APSIM question answering, APSIMX explanation, and CSV summarisation.
"""

from __future__ import annotations

import json
import sys
from pathlib import Path

import pandas as pd
import streamlit as st
from dotenv import load_dotenv

PROJECT_ROOT = Path(__file__).resolve().parents[1]
if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))

# Load the project-root `.env` file for local development, but never let it
# override real environment variables already provided by the hosting platform.
# In Azure Container Apps, those injected env vars must win.
load_dotenv(dotenv_path=PROJECT_ROOT / ".env", override=False)

from app.config import get_config
from app.logging_config import configure_logging, get_logger
from app.services.az_ai_search_service import AISearchService
from app.models.mo_apsimx_parser import parse_apsimx_content
from app.services.az_blob_service import BlobStorageService
from app.models.mo_csv_summary_service import analyse_csv_bytes
from app.services.oa_openai_service import OpenAIService
from app.services.oa_prompt_builder import (
    APSIMX_SYSTEM_PROMPT,
    CSV_SYSTEM_PROMPT,
    RAG_SYSTEM_PROMPT,
    build_apsimx_user_prompt,
    build_csv_user_prompt,
    build_rag_user_prompt,
    strip_trailing_sources_section,
)
from app.ui.ui_sidebar import render_sidebar

config = get_config()
logger = configure_logging(config)

st.set_page_config(page_title=config.app_name, page_icon=":seedling:", layout="wide")


@st.cache_resource
def get_openai_service() -> OpenAIService | None:
    """Return one cached OpenAI service instance for the Streamlit session."""
    if not config.has_openai():
        return None
    return OpenAIService(config)


@st.cache_resource
def get_search_service() -> AISearchService | None:
    """Return one cached Azure AI Search service instance for the session."""
    openai_service = get_openai_service()
    if not config.has_search() or openai_service is None:
        return None
    return AISearchService(config, openai_service)


@st.cache_resource
def get_blob_service() -> BlobStorageService | None:
    """Return one cached Blob Storage helper when storage is configured."""
    if not config.has_blob():
        return None
    service = BlobStorageService(config)
    service.ensure_container()
    return service


def render_ask_apsim_tab() -> None:
    """Render the retrieval-augmented APSIM question-answering workflow."""
    question = st.text_area(
        "Ask a question about APSIM",
        placeholder="Example: How does APSIM represent soil water balance outputs?",
        height=120,
    )
    if st.button("Answer APSIM question", type="primary"):
        if not question.strip():
            st.error("Enter a question first.")
            return
        search_service = get_search_service()
        openai_service = get_openai_service()
        if search_service is None or openai_service is None:
            st.error("OpenAI API and Azure AI Search are required for APSIM Q&A.")
            return
        with st.spinner("Retrieving APSIM context and drafting answer..."):
            try:
                # Retrieval stays explicit so users can inspect the exact source
                # chunks that informed the generated answer.
                documents = search_service.search(question)
                prompt = build_rag_user_prompt(question, documents, config.max_context_characters)
                answer = strip_trailing_sources_section(openai_service.chat(RAG_SYSTEM_PROMPT, prompt))
                st.markdown(f"{answer}<br><br>", unsafe_allow_html=True)
                with st.expander("Retrieved sources", expanded=False):
                    for index, doc in enumerate(documents, start=1):
                        display_path = Path(doc.source_path.replace("\\", "/")).name if doc.source_path else ""
                        st.markdown(f"<br>**{index}. {doc.title} [{display_path}]**", unsafe_allow_html=True)
                        st.write(doc.content[:1200] + ("..." if len(doc.content) > 1200 else ""))
                        st.markdown(f"<hr>", unsafe_allow_html=True)
            except Exception as exc:
                logger.exception("APSIM Q&A failed.")
                st.error(f"Could not answer the APSIM question: {exc}")


def render_explain_apsimx_tab() -> None:
    """Render the APSIMX upload and explanation workflow."""
    uploaded_file = st.file_uploader("Upload a .apsimx file", type=["apsimx"], key="apsimx")
    if uploaded_file is None:
        st.info("Upload an APSIMX file to generate a plain-English explanation.")
        return
    if st.button("Explain APSIMX file", type="primary", key="explain-apsimx"):
        openai_service = get_openai_service()
        blob_service = get_blob_service()
        try:
            content = uploaded_file.getvalue()
            structured_summary = parse_apsimx_content(content)
            if config.save_uploads_to_blob and blob_service is not None:
                # Optional upload persistence is useful for demos where you want
                # to inspect later what files users tried in the app.
                blob_service.upload_bytes(uploaded_file.name, content, folder="uploads/apsimx")
            if openai_service is None:
                st.warning("OpenAI API is not configured. Showing the structured summary only.")
                st.json(structured_summary, expanded=True)
                return
            prompt = build_apsimx_user_prompt(structured_summary)
            explanation = openai_service.chat(APSIMX_SYSTEM_PROMPT, prompt)
            st.markdown(explanation)
            with st.expander("Structured summary (JSON)"):
                st.json(structured_summary, expanded=True)
        except json.JSONDecodeError:
            st.error("The uploaded file is not valid JSON, so it could not be parsed as an APSIMX file.")
        except Exception as exc:
            logger.exception("APSIMX explanation failed.")
            st.error(f"Could not explain the APSIMX file: {exc}")


def render_basic_charts(dataframe: pd.DataFrame) -> None:
    """Render a minimal chart for the first few numeric columns."""
    numeric_columns = dataframe.select_dtypes(include="number").columns.tolist()
    if not numeric_columns:
        st.info("No numeric columns were detected, so no charts were generated.")
        return
    st.line_chart(dataframe[numeric_columns[:3]].head(100))


def render_summarise_csv_tab() -> None:
    """Render the CSV upload, analysis, and summary workflow."""
    uploaded_files = st.file_uploader("Upload APSIM output CSV files", type=["csv"], accept_multiple_files=True, key="csv")
    if not uploaded_files:
        st.info("Upload one or more CSV files to generate a plain-English summary.")
        return
    if st.button("Summarise CSV files", type="primary", key="summarise-csv"):
        openai_service = get_openai_service()
        blob_service = get_blob_service()
        for uploaded_file in uploaded_files:
            try:
                content = uploaded_file.getvalue()
                dataframe, analysis = analyse_csv_bytes(uploaded_file.name, content, preview_rows=config.csv_preview_rows)
                if config.save_uploads_to_blob and blob_service is not None:
                    blob_service.upload_bytes(uploaded_file.name, content, folder="uploads/csv")
                st.subheader(uploaded_file.name)
                st.write(f"Rows: {analysis.row_count} | Columns: {analysis.column_count}")
                if openai_service is not None:
                    prompt = build_csv_user_prompt(analysis.to_prompt_payload())
                    summary = openai_service.chat(CSV_SYSTEM_PROMPT, prompt)
                    st.markdown(summary)
                else:
                    st.warning("OpenAI API is not configured. Showing the analysis payload only.")
                    st.json(analysis.to_prompt_payload(), expanded=False)
                st.dataframe(dataframe.head(config.csv_preview_rows), use_container_width=True)
                render_basic_charts(dataframe)
                with st.expander("Descriptive statistics"):
                    st.json(analysis.descriptive_stats, expanded=False)
            except Exception as exc:
                logger.exception("CSV summarisation failed.")
                st.error(f"Could not summarise {uploaded_file.name}: {exc}")


def main() -> None:
    """Render the full Streamlit page and route users to each workflow tab."""
    render_sidebar(config)
    st.title(config.app_name)
    st.caption("Ask APSIM questions, explain `.apsimx` files, and summarise APSIM output CSVs.")
    tab_ask, tab_apsimx, tab_csv = st.tabs(["Ask APSIM", "Explain .apsimx", "Summarise CSV"])
    with tab_ask:
        render_ask_apsim_tab()
    with tab_apsimx:
        render_explain_apsimx_tab()
    with tab_csv:
        render_summarise_csv_tab()
    st.divider()
    st.caption(f"{config.app_name} | Mode: {config.deployment_mode}")


if __name__ == "__main__":
    main()
