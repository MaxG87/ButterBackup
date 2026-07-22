from collections import abc
from pathlib import Path

import pytest
import shell_interface as sh


@pytest.fixture
def root_owned_tmp_path(tmp_path: Path) -> abc.Iterable[Path]:
    """
    Create a temporary directory owned by root for testing
    """
    root_owned_path = tmp_path / "root_owned"
    root_owned_path.mkdir()
    current_user = sh.get_user()
    current_group = sh.get_group(current_user)
    sh.chown(root_owned_path, user="root", group="root", recursive=False)
    try:
        yield root_owned_path
    finally:
        sh.chown(
            root_owned_path, user=current_user, group=current_group, recursive=False
        )
