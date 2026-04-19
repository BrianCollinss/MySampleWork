# Base image for the APSIM Copilot demo container.
FROM python:3.11-slim

# Runtime flags keep Python output predictable inside Container Apps and
# configure Streamlit to listen on the container network interface.
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1 \
    STREAMLIT_SERVER_PORT=8501 \
    STREAMLIT_SERVER_ADDRESS=0.0.0.0

# Install Python dependencies before copying the rest of the source so Docker
# can reuse this layer when only application code changes.
WORKDIR /app

COPY requirements.txt .
RUN pip install --upgrade pip && pip install -r requirements.txt

# Copy the app source and expose the Streamlit port expected by the docs and
# deployment manifests.
COPY . .

EXPOSE 8501

# Start the Streamlit UI when the container launches.
CMD ["streamlit", "run", "app/main.py", "--server.port=8501", "--server.address=0.0.0.0"]
