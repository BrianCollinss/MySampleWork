"""
RockyBot: News Research Tool

What this app does
------------------
1. Lets the user enter up to 3 news article URLs.
2. Downloads the text from those pages.
3. Converts the text into LangChain Document objects.
4. Splits long text into smaller chunks.
5. Creates embeddings for those chunks using OpenAIEmbeddings.
6. Stores the embeddings in a FAISS vector database.
7. Saves that vector store to disk.
8. Lets the user ask questions about the articles.
9. Uses RetrievalQAWithSourcesChain to retrieve relevant chunks and answer.

What this version has
---------------------
This version also improves the Streamlit behaviour:
- the question box is always shown in the same place
- the question box stays disabled until processing succeeds
- session_state is used so the app remembers whether URLs were processed
- the question box does not jump to the bottom of the page after processing

Required packages
-----------------
pip install streamlit requests beautifulsoup4 python-dotenv
pip install langchain==0.0.284 openai==0.28.0 faiss-cpu==1.7.4 unstructured==0.9.2

Helpful commands on Windows/Conda
--------------------------------------------------------
conda env create -f environment.yml
conda activate rockybot
conda env update -f environment.yml --prune
conda update --all
conda env export > environment.yml
streamlit run main.py
"""


import os
import requests
import streamlit as st

import time
from uuid import uuid4
from bs4 import BeautifulSoup
from dotenv import load_dotenv

from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_core.documents import Document
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_community.vectorstores import FAISS


# Load environment variables from the .env file.
load_dotenv()


def clean_urls(urls):
    """Remove empty URLs."""

    # Keep only non-empty URLs after trimming spaces.
    return [url.strip() for url in urls if url and url.strip()]


def fetch_url_as_document(url):
    """Download one web page and turn it into a Document."""

    # Send a browser-like header to reduce blocking by websites.
    headers = {
        "User-Agent": (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/124.0 Safari/537.36"
        )
    }

    # Download the page.
    try:
        response = requests.get(url, headers=headers, timeout=20)
        response.raise_for_status()
    except requests.exceptions.RequestException as e:
        raise ValueError(f"Failed to fetch URL '{url}': {e}")

    # Parse HTML.
    soup = BeautifulSoup(response.text, "html.parser")

    # Remove elements that do not help with article text.
    for tag in soup(["script", "style", "noscript", "iframe", "svg"]):
        tag.decompose()

    # Join visible text into one string.
    text = " ".join(soup.stripped_strings)

    # Stop if no readable text was found.
    if not text.strip():
        raise ValueError("No readable text found.")

    # Create a stable id for this document.
    doc_id = str(uuid4())

    # Return a LangChain document.
    return Document(
        page_content=text,
        metadata={"source": url, "doc_id": doc_id},
        id=doc_id
    )


def ensure_document_ids(documents):
    """Make sure every document has an id."""

    fixed_documents = []

    for doc in documents:
        # Reuse an existing id if possible.
        doc_id = (
            getattr(doc, "id", None)
            or doc.metadata.get("doc_id")
            or str(uuid4())
        )

        # Copy metadata so we can safely update it.
        metadata = dict(doc.metadata)
        metadata["doc_id"] = doc_id

        # Create a clean document with a guaranteed id.
        fixed_doc = Document(
            page_content=doc.page_content,
            metadata=metadata,
            id=doc_id
        )

        fixed_documents.append(fixed_doc)

    return fixed_documents


def load_documents_from_urls(urls):
    """Load documents from all URLs."""

    documents = []

    for url in urls:
        try:
            # Download and convert each URL into a document.
            doc = fetch_url_as_document(url)
            documents.append(doc)

        except Exception as e:
            # Show a warning for URLs that fail.
            st.warning(f"Could not load {url}: {e}")

    return documents


def split_documents(documents):
    """Split documents into chunks."""

    splitter = RecursiveCharacterTextSplitter(
        separators=["\n\n", "\n", ".", ","],
        chunk_size=1000,
        chunk_overlap=200
    )

    # Split the documents into smaller chunks.
    chunks = splitter.split_documents(documents)

    fixed_chunks = []

    for chunk in chunks:
        # Keep metadata, especially the parent doc_id and source.
        metadata = dict(chunk.metadata)

        # Rebuild the chunk document cleanly.
        fixed_chunk = Document(
            page_content=chunk.page_content,
            metadata=metadata
        )

        fixed_chunks.append(fixed_chunk)

    return fixed_chunks


def build_vectorstore(documents, embeddings):
    """Create FAISS vector store."""

    # First make sure all source documents have ids.
    documents = ensure_document_ids(documents)

    # Then split them into chunk documents.
    chunks = split_documents(documents)

    # Build a unique id for every chunk explicitly.
    # This is safer than relying on Document.id alone.
    ids = []

    for i, chunk in enumerate(chunks):
        # Use the original document id if available.
        parent_doc_id = chunk.metadata.get("doc_id", "doc")

        # Build a guaranteed unique chunk id.
        chunk_id = f"{parent_doc_id}_chunk_{i}"

        # Save it in metadata as well for debugging.
        chunk.metadata["chunk_id"] = chunk_id

        ids.append(chunk_id)

    # Optional safety check before creating the vector store.
    if len(ids) != len(set(ids)):
        raise ValueError("Duplicate chunk ids were generated before FAISS.")

    # Build the FAISS index using explicit ids.
    vectorstore = FAISS.from_texts(
        texts=[chunk.page_content for chunk in chunks],
        embedding=embeddings,
        metadatas=[chunk.metadata for chunk in chunks],
        ids=ids
    )

    return vectorstore, chunks


def save_vectorstore(vectorstore, file_path):
    """Save FAISS store to disk."""

    # Save index and metadata to a local folder.
    vectorstore.save_local(file_path)


def load_vectorstore(file_path, embeddings):
    """Load FAISS store from disk."""

    # Stop if no saved store exists yet.
    if not os.path.exists(file_path):
        return None

    # Load the FAISS store from disk.
    vectorstore = FAISS.load_local(
        file_path,
        embeddings,
        allow_dangerous_deserialization=True
    )

    return vectorstore


def process_urls_and_build_store(urls, file_path, embeddings):
    """Run the full ingestion pipeline."""

    # Remove blank URL inputs.
    filtered_urls = clean_urls(urls)

    # Stop early if no valid URLs were provided.
    if not filtered_urls:
        st.error("Please provide at least one URL.")
        return None

    # Download article text from the URLs.
    documents = load_documents_from_urls(filtered_urls)

    # Show how many articles were loaded.
    st.write(f"Documents loaded: {len(documents)}")

    # Stop if no content was extracted.
    if not documents:
        st.error("No content extracted.")
        return None

    try:
        # Build embeddings and FAISS index.
        vectorstore, chunks = build_vectorstore(documents, embeddings)

        # Show how many chunks were created.
        st.write(f"Chunks created: {len(chunks)}")

        # Save the FAISS store for later reuse.
        save_vectorstore(vectorstore, file_path)

        st.success("Vector store saved.")
        return vectorstore

    except Exception as e:
        # Show any vector or embedding errors.
        st.error(f"Vector store creation failed: {e}")
        return None


def answer_query(query, vectorstore):
    """Answer a question using retrieved article chunks."""

    # Stop if the vector store is missing.
    if vectorstore is None:
        st.error("Vector store missing. Process URLs first.")
        return

    # Create the chat model.
    llm = ChatOpenAI(
        model="gpt-4o-mini",
        temperature=0.3,
        max_tokens=500
    )

    # Create a retriever from the FAISS store.
    retriever = vectorstore.as_retriever(search_kwargs={"k": 4})

    try:
        # Retrieve relevant chunks.
        if hasattr(retriever, "invoke"):
            docs = retriever.invoke(query)
        else:
            docs = retriever.get_relevant_documents(query)

    except Exception as e:
        st.error(f"Retrieval failed: {e}")
        return

    # Stop if nothing relevant was found.
    if not docs:
        st.warning("No relevant content was found for this question.")
        return

    # Build context from the retrieved chunks.
    context_parts = []

    for i, doc in enumerate(docs, start=1):
        source = doc.metadata.get("source", "Unknown source")
        context_parts.append(
            f"Source {i}: {source}\n{doc.page_content}"
        )

    context = "\n\n".join(context_parts)

    # Build a grounded prompt.
    prompt = f"""
You are answering questions based only on the provided article excerpts.
Use only the context below.
If the answer is not available in the context, say you do not know.

Question:
{query}

Context:
{context}

Answer:
""".strip()

    try:
        # Ask the model to answer the question.
        response = llm.invoke(prompt)

    except Exception as e:
        st.error(f"Query failed: {e}")
        return

    # Show the answer on the page.
    st.header("Answer")
    st.write(response.content)


def initialise_session_state():
    """Create default session values."""

    # Track whether documents were processed.
    if "documents_processed" not in st.session_state:
        st.session_state.documents_processed = False

    # Track whether a saved vector store is ready.
    if "vectorstore_ready" not in st.session_state:
        st.session_state.vectorstore_ready = False


def get_embeddings():
    """Create the embedding model only when needed."""

    return OpenAIEmbeddings(model="text-embedding-3-small")


def main():
    """Run the Streamlit app."""

    # Create session values the first time the app runs.
    initialise_session_state()

    # Show the page title first so the app renders immediately.
    st.title("RockyBot: News Research Tool 📈")

    # Build the sidebar.
    st.sidebar.title("News Article URLs")

    # Keep the three original default articles in the code.
    default_urls = [
        (
            "https://www.moneycontrol.com/news/business/"
            "tata-motors-mahindra-gain-certificates-for-"
            "production-linked-payouts-11281691.html"
        ),
        (
            "https://www.moneycontrol.com/news/business/"
            "tata-motors-launches-punch-icng-price-starts-at-"
            "rs-7-1-lakh-11098751.html"
        ),
        (
            "https://www.moneycontrol.com/news/business/stocks/"
            "buy-tata-motors-target-of-rs-743-kr-choksey-"
            "11080811.html"
        )
    ]

    # Define where the FAISS store is saved.
    file_path = "faiss_store_openai"

    # If a saved store already exists, enable questioning.
    if os.path.exists(file_path):
        st.session_state.vectorstore_ready = True
        st.session_state.documents_processed = True

    # Let the user choose how many URL boxes to show.
    article_count = st.sidebar.slider("Number of articles", min_value=1, max_value=10, value=3, step=1)

    # Collect URLs from the visible input boxes.
    urls = []

    for i in range(article_count):
        # Use the built-in defaults for the first three boxes.
        if i < len(default_urls):
            default_value = default_urls[i]
        else:
            default_value = ""

        # Create one text box per article.
        url = st.sidebar.text_input(
            f"URL {i + 1}",
            value=default_value,
            key=f"url_{i}"
        )

        urls.append(url)

    # Create the processing button.
    process_url_clicked = st.sidebar.button("Process URLs")

    # Keep the question area in a fixed place on the page.
    question_container = st.container()

    with question_container:
        # Show the question box.
        query = st.text_input(
            "Ask a question about the articles:",
            disabled=not st.session_state.documents_processed
        )

        # Show a message until processing is complete.
        if not st.session_state.documents_processed:
            st.info("Process the URLs first to enable questions.")

    # Process articles when the user clicks the button.
    if process_url_clicked:

        # If FAISS already exists, do not rebuild it
        # if os.path.exists(file_path):
        #     st.info("Vector store already exists. Reusing saved index.")
        #     st.session_state.documents_processed = True
        #     st.session_state.vectorstore_ready = True
        #     return
    
        # Disable questions during processing.
        st.session_state.documents_processed = False
        st.session_state.vectorstore_ready = False

        try:
            # Create embeddings only at processing time.
            embeddings = get_embeddings()

            # Build and save the vector store.
            vectorstore = process_urls_and_build_store(urls, file_path, embeddings)

            # Re-enable questions after successful processing.
            if vectorstore is not None:
                st.session_state.documents_processed = True
                st.session_state.vectorstore_ready = True

                time.sleep(2)   # few seconds wait so user can see the texts
                st.rerun()

        except Exception as e:
            st.error(f"Processing failed: {e}")

    # Answer the question if the user entered one.
    if query:
        try:
            # Create embeddings only when loading the saved store.
            embeddings = get_embeddings()

            # Load the saved vector store.
            vectorstore = load_vectorstore(file_path, embeddings)

            # Stop if no saved store exists.
            if vectorstore is None:
                st.error("No processed data found. Please process URLs first.")
            else:
                # Answer the question from the loaded store.
                answer_query(query, vectorstore)

        except Exception as e:
            st.error(f"Could not load vector store: {e}")


if __name__ == "__main__":
    main()