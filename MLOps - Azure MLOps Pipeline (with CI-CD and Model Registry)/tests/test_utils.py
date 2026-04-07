import os
import sys
import tempfile
from unittest.mock import patch

import pytest

# Add scripts to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'scripts'))

# Mock azure imports to avoid authentication issues during testing
with patch("utilities.utils.MLClient"), patch("utilities.utils.DefaultAzureCredential"):
    from utilities.utils import (
        get_stored_run_id,
        read_metric_file,
        save_metric_outputs,
        save_run_id,
    )


class TestUtils:
    def test_save_and_get_run_id(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            run_id = "test_run_123"
            save_run_id(temp_dir, run_id)
            retrieved_id = get_stored_run_id(temp_dir)
            assert retrieved_id == run_id

    def test_get_stored_run_id_file_not_found(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            with pytest.raises(FileNotFoundError):
                get_stored_run_id(temp_dir)

    def test_save_and_read_metric_file(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            metric_name = "accuracy"
            metric_value = "0.95"
            save_metric_outputs(temp_dir, {metric_name: metric_value})
            retrieved_value = read_metric_file(temp_dir, metric_name)
            assert retrieved_value == metric_value

    def test_read_metric_file_not_found(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            with pytest.raises(FileNotFoundError):
                read_metric_file(temp_dir, "nonexistent_metric")
