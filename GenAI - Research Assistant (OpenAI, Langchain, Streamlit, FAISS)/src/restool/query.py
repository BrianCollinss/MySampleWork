"""Functions for querying the vector store and answering questions."""

from langchain_openai import ChatOpenAI
import streamlit as st
from .config import LLM_MODEL, LLM_TEMPERATURE, LLM_MAX_TOKENS, RETRIEVER_K


def answer_query(query: str, vectorstore) -> None:
    """Answer a question using the retrieved documents from the vector store."""
    if vectorstore is None:
        st.error("Vector store missing. Process URLs first.")
        return

    # Create the LLM client for generating the answer.
    llm = ChatOpenAI(
        model=LLM_MODEL,
        temperature=LLM_TEMPERATURE,
        max_tokens=LLM_MAX_TOKENS
    )

    # Use the vector store retriever to get the top-K relevant chunks.
    retriever = vectorstore.as_retriever(search_kwargs={"k": RETRIEVER_K})

    try:
        docs = retriever.invoke(query) if hasattr(retriever, "invoke") else retriever.get_relevant_documents(query)
    except Exception as e:
        st.error(f"Retrieval failed: {e}")
        return

    if not docs:
        st.warning("No relevant content found for this question.")
        return

    # Build a context payload for the LLM from retrieved documents.
    context_parts = []
    for i, doc in enumerate(docs, start=1):
        source = doc.metadata.get("source", "Unknown source")
        context_parts.append(f"Source {i}: {source}\n{doc.page_content}")
    context = "\n\n".join(context_parts)

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
        response = llm.invoke(prompt)
        st.header("Answer")
        st.write(response.content)
    except Exception as e:
        st.error(f"Query failed: {e}")
