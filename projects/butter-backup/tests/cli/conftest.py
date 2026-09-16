import pytest
from typer.testing import CliRunner


@pytest.fixture
def runner(monkeypatch) -> CliRunner:
    # Set the COLUMNS environment variable to a large value to avoid line wrapping in
    # the output during tests. This ensures that the output is displayed in a single
    # line, making it easier to assert error messages.
    monkeypatch.setenv("COLUMNS", "1000")
    return CliRunner()
