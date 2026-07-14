import typing as t
from pathlib import Path

import pytest
import shell_interface as sh

import storage_device_managers as sdm


@pytest.fixture
def mounted_directory_with_spaces(tmp_path: Path) -> t.Iterable[Path]:
    src = tmp_path / "src"
    src.mkdir()
    mountpoint = tmp_path / "mount point with spaces"
    mountpoint.mkdir()
    # sdm.mounted_device cannot be used here because it checks for the filesystem type,
    # which is not present on this mock directory.
    mount_cmd: sh.StrPathList = ["sudo", "mount", "-o", "bind", src, mountpoint]
    umount_cmd: sh.StrPathList = ["sudo", "umount", mountpoint]
    sh.run_cmd(cmd=mount_cmd)
    try:
        yield mountpoint
    finally:
        sh.run_cmd(cmd=umount_cmd)


def test_get_mounted_devices_raises_on_unknown_device() -> None:
    with pytest.raises(KeyError):
        sdm.get_mounted_devices()["unknown-device"]


def test_get_mounted_devices_includes_correct_mountpoints(mounted_directories) -> None:
    _, dest = mounted_directories
    all_mounts = sdm.get_mounted_devices()
    for dest_set in all_mounts.values():
        if dest in dest_set:
            mount_options = dest_set[dest]
            break
    else:
        raise AssertionError(f"Expected mount point {dest} not found.")
    assert "rw" in mount_options


def test_get_mounted_devices_includes_root() -> None:
    assert any(Path("/") in dest_set for dest_set in sdm.get_mounted_devices().values())


def test_get_mounted_devices_handles_spaces_in_destination(
    mounted_directory_with_spaces,
) -> None:
    mount_point = mounted_directory_with_spaces
    all_mounts = sdm.get_mounted_devices()
    all_mount_points = {cur for dest_set in all_mounts.values() for cur in dest_set}
    errmsg = f"Expected mount point '{mount_point}' not found."
    assert mount_point in all_mount_points, errmsg
