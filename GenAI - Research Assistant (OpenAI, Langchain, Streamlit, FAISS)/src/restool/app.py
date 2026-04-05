"""Streamlit app logic for ResTool."""

import os
import time
import streamlit as st
from langchain_openai import OpenAIEmbeddings
from .config import FAISS_FILE_PATH
from .fetcher import clean_urls, load_documents_from_urls
from .processor import build_vectorstore, save_vectorstore, load_vectorstore
from .query import answer_query


def initialise_session_state() -> None:
    """Ensure the app remembers whether documents have already been processed."""
    if "documents_processed" not in st.session_state:
        st.session_state.documents_processed = False
    if "vectorstore_ready" not in st.session_state:
        st.session_state.vectorstore_ready = False


def _patch_tiktoken_encoding_for_model() -> None:
    """Patch tiktoken model lookup so unsupported names use cl100k_base silently."""
    try:
        import tiktoken
    except ImportError:
        # If tiktoken is not installed, let LangChain handle tokenizer selection.
        return

    original = getattr(tiktoken, "encoding_for_model", None)
    if original is None:
        return

    def encoding_for_model_with_fallback(model_name: str):
        try:
            return original(model_name)
        except KeyError:
            return tiktoken.get_encoding("cl100k_base")

    tiktoken.encoding_for_model = encoding_for_model_with_fallback


def get_embeddings():
    """Create the OpenAI embeddings client for the current session."""
    from .config import EMBEDDING_MODEL

    # Patch model-to-encoding lookup to suppress warnings for newer OpenAI models.
    _patch_tiktoken_encoding_for_model()

    return OpenAIEmbeddings(
        model=EMBEDDING_MODEL,
        tiktoken_model_name=EMBEDDING_MODEL
    )


def process_urls_and_build_store(urls: list[str], file_path: str, embeddings) -> None:
    """Fetch URLs, extract content, create embeddings, and persist the FAISS store."""
    filtered_urls = clean_urls(urls)
    if not filtered_urls:
        st.error("Please provide at least one URL.")
        return

    # Fetch article text for each URL.
    documents = load_documents_from_urls(filtered_urls)
    st.write(f"Documents loaded: {len(documents)}")
    if not documents:
        st.error("No content extracted.")
        return

    try:
        # Convert fetched text into vector embeddings and build the search index.
        vectorstore, chunks = build_vectorstore(documents, embeddings)
        st.write(f"Chunks created: {len(chunks)}")

        # Save the vector store so later queries can be answered without reprocessing.
        save_vectorstore(vectorstore, file_path)
        st.success("Vector store saved.")
        return vectorstore
    except Exception as e:
        st.error(f"Vector store creation failed: {e}")


def main() -> None:
    """Run the Streamlit app and wire the UI actions to processing logic."""
    initialise_session_state()
    st.title("ResTool 📈")

    # Sidebar UI: URL input, article count, and process button.
    st.sidebar.title("News Article URLs")
    default_urls = [
        "https://www.moneycontrol.com/news/business/tata-motors-mahindra-gain-certificates-for-production-linked-payouts-11281691.html",
        "https://www.moneycontrol.com/news/business/tata-motors-launches-punch-icng-price-starts-at-rs-7-1-lakh-11098751.html",
        "https://www.moneycontrol.com/news/business/stocks/buy-tata-motors-target-of-rs-743-kr-choksey-11080811.html"
    ]

    # If an existing FAISS store is already saved, enable the query path.
    if os.path.exists(FAISS_FILE_PATH):
        st.session_state.vectorstore_ready = True
        st.session_state.documents_processed = True

    article_count = st.sidebar.slider("Number of articles", min_value=1, max_value=10, value=3)
    urls = []
    for i in range(article_count):
        default_value = default_urls[i] if i < len(default_urls) else ""
        url = st.sidebar.text_input(f"URL {i + 1}", value=default_value, key=f"url_{i}")
        urls.append(url)

    process_url_clicked = st.sidebar.button("Process URLs")

    # Query input area. Disabled until documents are processed.
    question_container = st.container()
    with question_container:
        query = st.text_input(
            "Ask a question about the articles:",
            disabled=not st.session_state.documents_processed
        )
        if not st.session_state.documents_processed:
            st.info("Process the URLs first to enable questions.")

    if process_url_clicked:
        st.session_state.documents_processed = False
        st.session_state.vectorstore_ready = False
        try:
            embeddings = get_embeddings()
            vectorstore = process_urls_and_build_store(urls, FAISS_FILE_PATH, embeddings)
            if vectorstore is not None:
                st.session_state.documents_processed = True
                st.session_state.vectorstore_ready = True
                time.sleep(2)
                st.rerun()
        except Exception as e:
            st.error(f"Processing failed: {e}")

    if query:
        try:
            embeddings = get_embeddings()
            vectorstore = load_vectorstore(FAISS_FILE_PATH, embeddings)
            if vectorstore is None:
                st.error("No processed data found. Please process URLs first.")
            else:
                answer_query(query, vectorstore)
        except Exception as e:
            st.error(f"Could not load vector store: {e}")
