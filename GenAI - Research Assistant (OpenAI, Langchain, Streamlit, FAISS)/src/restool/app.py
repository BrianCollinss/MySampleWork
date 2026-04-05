"""
Streamlit app logic for ResTool.

This module contains the main Streamlit application for ResTool, a research assistant
that processes news articles from URLs, creates vector embeddings, and enables
question-answering using OpenAI's language models and FAISS vector storage.

Key components:
- Session state management for UI persistence
- URL processing and document loading
- Vector store creation and persistence
- Interactive Q&A interface
"""

import os
import time
import streamlit as st
from langchain_openai import OpenAIEmbeddings
from .config import FAISS_FILE_PATH
from .fetcher import clean_urls, load_documents_from_urls
from .processor import build_vectorstore, save_vectorstore, load_vectorstore
from .query import answer_query


def initialise_session_state() -> None:
    """
    Initialize Streamlit session state variables.

    Session state persists across app reruns, allowing the UI to remember
    whether documents have been processed and if the vector store is ready.
    This prevents reprocessing URLs unnecessarily and controls UI element states.
    """
    if "documents_processed" not in st.session_state:
        st.session_state.documents_processed = False
    if "vectorstore_ready" not in st.session_state:
        st.session_state.vectorstore_ready = False


def _patch_tiktoken_encoding_for_model() -> None:
    """
    Apply a patch to tiktoken's encoding lookup to handle unsupported model names.

    Some newer OpenAI models may not be recognized by tiktoken, causing warnings
    or errors. This function replaces the original encoding_for_model function
    with a fallback version that uses 'cl100k_base' encoding for unknown models,
    preventing tokenizer-related issues during embedding creation.

    Steps:
    1. Check if tiktoken is available; if not, skip patching.
    2. Store reference to the original encoding_for_model function.
    3. Define a new function that tries the original lookup first.
    4. If KeyError occurs (model not found), fall back to cl100k_base encoding.
    5. Replace tiktoken's function with the patched version.
    """
    try:
        import tiktoken
    except ImportError:
        # If tiktoken is not installed, let LangChain handle tokenizer selection.
        return

    original = getattr(tiktoken, "encoding_for_model", None)
    if original is None:
        return

    def encoding_for_model_with_fallback(model_name: str):
        """
        Get encoding for a model name, with fallback for unknown models.

        Args:
            model_name: The name of the OpenAI model to get encoding for.

        Returns:
            tiktoken.Encoding: The appropriate encoding for the model.
        """
        try:
            return original(model_name)
        except KeyError:
            return tiktoken.get_encoding("cl100k_base")

    tiktoken.encoding_for_model = encoding_for_model_with_fallback


def get_embeddings():
    """
    Create and return an OpenAI embeddings client.

    This function sets up the embeddings model used for converting text
    into vector representations. It applies the tiktoken patch to prevent
    warnings and ensures compatibility with various OpenAI models.

    Returns:
        OpenAIEmbeddings: Configured embeddings client for text vectorization.
    """
    from .config import EMBEDDING_MODEL

    # Apply patch to handle potential tokenizer issues with newer models.
    _patch_tiktoken_encoding_for_model()

    return OpenAIEmbeddings(
        model=EMBEDDING_MODEL,
        tiktoken_model_name=EMBEDDING_MODEL
    )


def process_urls_and_build_store(urls: list[str], file_path: str, embeddings) -> None:
    """
    Process a list of URLs to create and save a FAISS vector store.

    This function orchestrates the complete pipeline from URL input to
    persisted vector store:
    1. Validate and filter input URLs
    2. Fetch and extract text content from URLs
    3. Split documents into chunks and create embeddings
    4. Build and save the FAISS vector store for later retrieval

    Args:
        urls: List of URLs to process
        file_path: Path where to save the FAISS index
        embeddings: OpenAI embeddings client

    Returns:
        The created vector store, or None if processing failed
    """
    # Step 1: Clean and validate the provided URLs
    filtered_urls = clean_urls(urls)
    if not filtered_urls:
        st.error("Please provide at least one URL.")
        return

    # Step 2: Load documents from the filtered URLs
    documents = load_documents_from_urls(filtered_urls)
    st.write(f"Documents loaded: {len(documents)}")
    if not documents:
        st.error("No content extracted.")
        return

    try:
        # Step 3: Build vector store from documents using embeddings
        vectorstore, chunks = build_vectorstore(documents, embeddings)
        st.write(f"Chunks created: {len(chunks)}")

        # Step 4: Persist the vector store to disk for future queries
        save_vectorstore(vectorstore, file_path)
        st.success("Vector store saved.")
        return vectorstore
    except Exception as e:
        st.error(f"Vector store creation failed: {e}")


def main() -> None:
    """
    Main entry point for the ResTool Streamlit application.

    This function sets up the UI and handles user interactions:
    1. Initialize session state for persistence
    2. Display the main title and sidebar for URL input
    3. Check for existing vector store and enable query mode if available
    4. Handle URL processing when the button is clicked
    5. Enable and process user queries against the processed documents
    """
    # Step 1: Set up session state to track processing status
    initialise_session_state()
    st.title("ResTool 📈")

    # Step 2: Configure sidebar UI for URL input
    st.sidebar.title("News Article URLs")
    default_urls = [
        "https://www.moneycontrol.com/news/business/tata-motors-mahindra-gain-certificates-for-production-linked-payouts-11281691.html",
        "https://www.moneycontrol.com/news/business/tata-motors-launches-punch-icng-price-starts-at-rs-7-1-lakh-11098751.html",
        "https://www.moneycontrol.com/news/business/stocks/buy-tata-motors-target-of-rs-743-kr-choksey-11080811.html"
    ]

    # Step 3: Check if vector store already exists from previous runs
    if os.path.exists(FAISS_FILE_PATH):
        st.session_state.vectorstore_ready = True
        st.session_state.documents_processed = True

    # Step 4: Create UI elements for URL input
    article_count = st.sidebar.slider("Number of articles", min_value=1, max_value=10, value=3)
    urls = []
    for i in range(article_count):
        default_value = default_urls[i] if i < len(default_urls) else ""
        url = st.sidebar.text_input(f"URL {i + 1}", value=default_value, key=f"url_{i}")
        urls.append(url)

    process_url_clicked = st.sidebar.button("Process URLs")

    # Step 5: Set up the query input area (disabled until documents are processed)
    question_container = st.container()
    with question_container:
        query = st.text_input(
            "Ask a question about the articles:",
            disabled=not st.session_state.documents_processed
        )
        if not st.session_state.documents_processed:
            st.info("Process the URLs first to enable questions.")

    # Step 6: Handle URL processing when button is clicked
    if process_url_clicked:
        st.session_state.documents_processed = False
        st.session_state.vectorstore_ready = False
        try:
            embeddings = get_embeddings()
            vectorstore = process_urls_and_build_store(urls, FAISS_FILE_PATH, embeddings)
            if vectorstore is not None:
                st.session_state.documents_processed = True
                st.session_state.vectorstore_ready = True
                time.sleep(2)  # Brief pause for user feedback
                st.rerun()  # Refresh UI to enable query input
        except Exception as e:
            st.error(f"Processing failed: {e}")

    # Step 7: Handle user queries if documents have been processed
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
