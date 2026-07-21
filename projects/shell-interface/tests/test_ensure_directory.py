import typing as t
from pathlib import Path

import pytest

import shell_interface as sh


def sudo_mkdir(path: Path) -> None:
    """
    Create a directory with sudo privileges
    """
    sudo_mkdir_cmd: sh.StrPathList = ["sudo", "mkdir", "-p", path]
    sh.run_cmd(cmd=sudo_mkdir_cmd)


@pytest.fixture
def root_owned_tmp_path(tmp_path: Path) -> t.Iterable[Path]:
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


@pytest.mark.parametrize("destination", ["destDir", "dest dir with spaces"])
@pytest.mark.parametrize(
    "first_created",
    [None, "subdir", "sub dir/with spaces/", "very/deeply/nested/subdir"],
)
def test_ensure_directory_creates_directory(
    root_owned_tmp_path: Path, first_created: str | None, destination: str
) -> None:
    if first_created is None:
        first_created_p = root_owned_tmp_path / destination
        destination_p = root_owned_tmp_path / destination
    else:
        first_created_p = root_owned_tmp_path / first_created
        destination_p = root_owned_tmp_path / first_created / destination
        sudo_mkdir(first_created_p.parent)

    result_created = sh.ensure_directory(destination_p)
    result_noop = sh.ensure_directory(destination_p)
    assert destination_p is not None
    assert destination_p.is_dir()
    assert result_noop is None
    assert result_created == first_created_p
