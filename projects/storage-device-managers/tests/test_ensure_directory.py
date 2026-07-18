import typing as t
from pathlib import Path

import pytest
import shell_interface as sh
from loguru import logger

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


def test_remove_empty_ancestors_removes_empty_directory_chain(tmp_path: Path) -> None:
    stop = tmp_path / "grandparent"
    start = stop / "parent" / "child"
    start.mkdir(parents=True)

    sdm.remove_empty_ancestors(start=start, stop=stop)

    assert not start.exists()
    assert not start.parent.exists()
    assert not stop.exists()


def test_remove_empty_ancestors_aborts_on_first_rmdir_failure(tmp_path: Path) -> None:
    stop = tmp_path / "grandparent"
    parent = stop / "parent"
    start = parent / "child"
    start.mkdir(parents=True)
    (parent / "keep.txt").write_text("content")

    messages: list[str] = []
    logger.enable("storage_device_managers")
    sink_id = logger.add(messages.append, level="DEBUG")
    try:
        sdm.remove_empty_ancestors(start=start, stop=stop)
    finally:
        logger.remove(sink_id)
        logger.disable("storage_device_managers")

    assert not start.exists()
    assert parent.exists()
    assert stop.exists()
    assert messages == [
        f"Aborting empty-directory cleanup at {parent}: `rmdir` failed.\n"
    ]


def test_remove_empty_ancestors_rejects_non_ancestor_paths(tmp_path: Path) -> None:
    with pytest.raises(ValueError, match="is not a subpath"):
        sdm.remove_empty_ancestors(
            start=tmp_path / "child", stop=tmp_path / "other-parent"
        )
