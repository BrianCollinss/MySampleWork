"""Functions for fetching and loading documents from URLs."""

import requests
from bs4 import BeautifulSoup
from uuid import uuid4
from typing import List
from langchain_core.documents import Document
import streamlit as st
from .config import REQUEST_TIMEOUT, USER_AGENT


def clean_urls(urls: List[str]) -> List[str]:
    """Remove empty URLs after trimming spaces."""
    return [url.strip() for url in urls if url and url.strip()]


def fetch_url_as_document(url: str) -> Document:
    """Download a web page and convert it to a LangChain Document."""
    headers = {"User-Agent": USER_AGENT}
    
    try:
        response = requests.get(url, headers=headers, timeout=REQUEST_TIMEOUT)
        response.raise_for_status()
    except requests.exceptions.RequestException as e:
        raise ValueError(f"Failed to fetch URL '{url}': {e}")
    
    soup = BeautifulSoup(response.text, "html.parser")
    for tag in soup(["script", "style", "noscript", "iframe", "svg"]):
        tag.decompose()
    
    text = " ".join(soup.stripped_strings)
    if not text.strip():
        raise ValueError("No readable text found.")
    
    doc_id = str(uuid4())
    return Document(
        page_content=text,
        metadata={"source": url, "doc_id": doc_id},
        id=doc_id
    )


def load_documents_from_urls(urls: List[str]) -> List[Document]:
    """Load documents from a list of URLs, handling errors gracefully."""
    documents = []
    for url in urls:
        try:
            doc = fetch_url_as_document(url)
            documents.append(doc)
        except Exception as e:
            st.warning(f"Could not load {url}: {e}")
    return documents