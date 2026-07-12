import typing as t
from pathlib import Path

import pytest
import shell_interface as sh

import storage_device_managers as sdm


@pytest.fixture
def root_owned_tmp_path(tmp_path: Path) -> t.Iterable[Path]:
    """
    Create a temporary directory owned by root for testing
    """
    root_owned_path = tmp_path / "root_owned"
    root_owned_path.mkdir()
    current_user = sh.get_user()
    current_group = sh.get_group(current_user)
    chown_to_root: sh.StrPathList = ["sudo", "chown", "root:root", root_owned_path]
    chown_to_user: sh.StrPathList = [
        "sudo",
        "chown",
        f"{current_user}:{current_group}",
        root_owned_path,
    ]
    sh.run_cmd(cmd=chown_to_root)
    try:
        yield root_owned_path
    finally:
        sh.run_cmd(cmd=chown_to_user)


def test_ensure_directory_creates_missing_directory(tmp_path: Path) -> None:
    destination = tmp_path / "mount"
    assert not destination.exists()
    assert sdm.ensure_directory(destination) == destination
    assert destination.exists()


def test_ensure_directory_returns_first_created_ancestor(tmp_path: Path) -> None:
    expected = tmp_path / "grandparent"
    destination = expected / "parent" / "child"
    assert not destination.exists()
    assert sdm.ensure_directory(destination) == expected
    assert destination.exists()


def test_ensure_directory_creates_missing_directory_in_root_owned_path(
    root_owned_tmp_path: Path,
) -> None:
    expected = root_owned_tmp_path / "nested"
    destination = expected / "mount"
    assert not destination.exists()
    assert sdm.ensure_directory(destination) == expected
    assert destination.exists()


def test_ensure_directory_skips_existing_directory(tmp_path: Path) -> None:
    assert sdm.ensure_directory(tmp_path) is None
