import shutil
from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest


@pytest.fixture(scope="session")
def _big_file_persistent():
    """
    Prepare a file of minimum size for BtrFS and return its path
    """

    min_size = 128 * 1024**2  # ~109MiB is the minimum size for BtrFS
    with NamedTemporaryFile() as ntf:
        file = Path(ntf.name)
        file.write_bytes(bytes(min_size))
        yield file


@pytest.fixture
def big_file(_big_file_persistent):
    """
    Prepare a file of minimum size for BtrFS and return its path
    """
    with NamedTemporaryFile() as ntf:
        big_file = Path(ntf.name)
        shutil.copy(_big_file_persistent, big_file)
        yield big_file
