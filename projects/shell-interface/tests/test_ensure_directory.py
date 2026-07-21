from pathlib import Path

import pytest

import shell_interface as sh


def sudo_mkdir(path: Path) -> None:
    """
    Create a directory with sudo privileges
    """
    sudo_mkdir_cmd: sh.StrPathList = ["sudo", "mkdir", "-p", path]
    sh.run_cmd(cmd=sudo_mkdir_cmd)


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
