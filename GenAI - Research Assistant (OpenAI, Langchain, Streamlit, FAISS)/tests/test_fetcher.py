"""Basic tests for fetcher module."""

import pytest
from restool.fetcher import clean_urls


def test_clean_urls():
    """Test URL cleaning."""
    urls = ["  http://example.com  ", "", "https://test.com"]
    result = clean_urls(urls)
    assert result == ["http://example.com", "https://test.com"]