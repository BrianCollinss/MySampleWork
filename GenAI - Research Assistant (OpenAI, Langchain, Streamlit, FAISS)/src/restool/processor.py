"""Functions for processing documents and managing the vector store."""

from typing import List, Tuple
from langchain_core.documents import Document
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_community.vectorstores import FAISS
from uuid import uuid4
import os
from .config import CHUNK_SIZE, CHUNK_OVERLAP, SEPARATORS, FAISS_FILE_PATH


def ensure_document_ids(documents: List[Document]) -> List[Document]:
    """Ensure every document has a unique ID before embedding."""
    fixed_documents = []
    for doc in documents:
        doc_id = getattr(doc, "id", None) or doc.metadata.get("doc_id") or str(uuid4())
        metadata = dict(doc.metadata)
        metadata["doc_id"] = doc_id
        fixed_doc = Document(
            page_content=doc.page_content,
            metadata=metadata,
            id=doc_id
        )
        fixed_documents.append(fixed_doc)
    return fixed_documents


def split_documents(documents: List[Document]) -> List[Document]:
    """Split documents into smaller chunks for embedding."""
    splitter = RecursiveCharacterTextSplitter(
        separators=SEPARATORS,
        chunk_size=CHUNK_SIZE,
        chunk_overlap=CHUNK_OVERLAP
    )
    chunks = splitter.split_documents(documents)
    fixed_chunks = []
    for chunk in chunks:
        metadata = dict(chunk.metadata)
        fixed_chunk = Document(
            page_content=chunk.page_content,
            metadata=metadata
        )
        fixed_chunks.append(fixed_chunk)
    return fixed_chunks


def build_vectorstore(documents: List[Document], embeddings) -> Tuple[FAISS, List[Document]]:
    """Create a FAISS vector store from documents."""
    # Step 1: give every document a stable identifier.
    documents = ensure_document_ids(documents)

    # Step 2: split documents into chunks to improve retrieval quality.
    chunks = split_documents(documents)
    
    ids = []
    for i, chunk in enumerate(chunks):
        parent_doc_id = chunk.metadata.get("doc_id", "doc")
        chunk_id = f"{parent_doc_id}_chunk_{i}"
        chunk.metadata["chunk_id"] = chunk_id
        ids.append(chunk_id)
    
    if len(ids) != len(set(ids)):
        raise ValueError("Duplicate chunk IDs generated.")
    
    # Step 3: build the FAISS store from chunk text and embeddings.
    vectorstore = FAISS.from_texts(
        texts=[chunk.page_content for chunk in chunks],
        embedding=embeddings,
        metadatas=[chunk.metadata for chunk in chunks],
        ids=ids
    )
    return vectorstore, chunks


def save_vectorstore(vectorstore: FAISS, file_path: str = FAISS_FILE_PATH) -> None:
    """Save the FAISS vector store to disk so it can be reused."""
    vectorstore.save_local(file_path)


def load_vectorstore(file_path: str, embeddings) -> FAISS | None:
    """Load an existing FAISS vector store from disk."""
    if not os.path.exists(file_path):
        return None
    return FAISS.load_local(
        file_path,
        embeddings
    )
