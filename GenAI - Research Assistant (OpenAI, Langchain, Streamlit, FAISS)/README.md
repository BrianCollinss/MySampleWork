
# ResTool

A Streamlit-based GenAI application for researching news articles using OpenAI, LangChain, and FAISS.

## Features
- Fetch and process text from news URLs
- Create embeddings and store in FAISS vector database
- Ask questions and get AI-powered answers based on article content
- Modular architecture for easy extension

## Installation
1. Clone the repository.
2. Create the Conda environment: `conda env create -f environment.yml`
3. Activate the environment: `conda activate restool`
4. Install Python packages: `pip install -r requirements.txt`
5. Copy `.env.example` to `.env` and add your OpenAI API key.
6. Run: `streamlit run main.py`

## Usage
- Enter up to 10 article URLs in the sidebar.
- Click "Process URLs" to build the vector store.
- Ask questions about the articles.

## Architecture
- `src/restool/`: Core modules (fetching, processing, querying, UI).
- Uses RAG (Retrieval-Augmented Generation) for Q&A.

## Testing
Run `pytest` in the root directory.

## License
- All rights reserved.
- This project is proprietary to Dr Brian Collins.
- See the `LICENSE` file for details.