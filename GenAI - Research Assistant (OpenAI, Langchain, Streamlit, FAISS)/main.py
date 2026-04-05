"""Entry point for ResTool Streamlit app."""

import os
import sys

# Ensure the local src package is visible when Streamlit runs this script.
ROOT_DIR = os.path.dirname(os.path.abspath(__file__))
SRC_DIR = os.path.join(ROOT_DIR, "src")
if SRC_DIR not in sys.path:
    sys.path.insert(0, SRC_DIR)

from restool.app import main

if __name__ == "__main__":
    main()