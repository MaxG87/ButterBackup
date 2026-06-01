from __future__ import annotations

from pathlib import Path
from tempfile import TemporaryDirectory

import pytest
import shell_interface as sh

import storage_device_managers as sdm


def in_docker_container() -> bool:
    return Path("/.dockerenv").exists()


class MyCustomTestException(Exception):
    pass


def test_mount_ext4_device(ext4_device) -> None:
    with TemporaryDirectory() as mount_dir:
        mount_path = Path(mount_dir)
        sdm.mount_ext4_device(ext4_device, mount_path)
        assert sdm.is_mounted(ext4_device)
        assert mount_path in sdm.get_mounted_devices()[str(ext4_device)]
        sdm.unmount_device(ext4_device)
        assert not sdm.is_mounted(ext4_device)


def test_mount_device(device_with_fs) -> None:
    device, _ = device_with_fs
    with TemporaryDirectory() as mount_dir:
        mount_path = Path(mount_dir)
        sdm.mount_device(device, mount_path)
        assert sdm.is_mounted(device)
        assert mount_path in sdm.get_mounted_devices()[str(device)]
        sdm.unmount_device(device)
        assert not sdm.is_mounted(device)


@pytest.mark.parametrize("args", [[], [sdm.ValidCompressions.ZSTD9]])
def test_mounted_device_without_compression(btrfs_device, args) -> None:
    with sdm.mounted_device(btrfs_device, *args) as md:
        assert md.exists()
        assert md.is_dir()
        assert sdm.is_mounted(btrfs_device)
        assert md in sdm.get_mounted_devices()[str(btrfs_device)]
    assert not md.exists()
    assert not sdm.is_mounted(btrfs_device)
    assert str(btrfs_device) not in sdm.get_mounted_devices()


def test_mounted_device_takes_over_already_mounted_device(btrfs_device) -> None:
    compression = sdm.ValidCompressions.LZO
    with TemporaryDirectory() as td:
        sdm.mount_btrfs_device(btrfs_device, Path(td), compression)
        with sdm.mounted_device(btrfs_device, compression) as md:
            assert sdm.is_mounted(btrfs_device)
            assert md in sdm.get_mounted_devices()[str(btrfs_device)]
        assert not sdm.is_mounted(btrfs_device)


@pytest.mark.skipif(
    in_docker_container(), reason="Root file system may be missing in Docker container."
)
def test_mounted_device_fails_on_not_unmountable_device() -> None:
    def get_root_device() -> Path:
        for device, mount_points in sdm.get_mounted_devices().items():
            if Path("/") in mount_points:
                return Path(device)
        raise ValueError("No device mounted on / was found.")

    root = get_root_device()
    with pytest.raises(sdm.UnmountError):
        with sdm.mounted_device(root):
            pass


def test_mounted_device_unmounts_in_case_of_exception(btrfs_device) -> None:
    with pytest.raises(MyCustomTestException):
        with sdm.mounted_device(btrfs_device, sdm.ValidCompressions.ZLIB1) as md:
            # That the device is mounted properly is guaranteed by a test
            # above.
            raise MyCustomTestException
    assert not sdm.is_mounted(btrfs_device), "Device is still mounted after exception."
    assert not md.exists(), "Mounted device still exists after exception."
    assert str(btrfs_device) not in sdm.get_mounted_devices()


@pytest.mark.parametrize("device", sdm.get_mounted_devices())
def test_is_mounted_detects(device: Path) -> None:
    assert sdm.is_mounted(device)


def test_is_mounted_rejects() -> None:
    with TemporaryDirectory() as tempd:
        assert not sdm.is_mounted(Path(tempd))


def test_unmount_device(btrfs_device) -> None:
    with TemporaryDirectory() as mountpoint:
        sdm.mount_btrfs_device(btrfs_device, Path(mountpoint))
        sdm.unmount_device(btrfs_device)
        assert not sdm.is_mounted(btrfs_device)


def test_unmount_device_raises_unmounterror() -> None:
    # This test calls unmount_device on a Path that is not mounted, which will cause
    # `umount` to fail. On such a failure, unmount_device is expected to raise an
    # UnmountError, which is what this test checks for.
    with TemporaryDirectory() as mountpoint:
        with pytest.raises(sdm.UnmountError):
            sdm.unmount_device(Path(mountpoint))


def test_mounted_device_does_not_delete_content_on_umount_error(
    btrfs_device, mocker
) -> None:
    compression = sdm.ValidCompressions.ZSTD15
    mocker.patch(
        "storage_device_managers.unmount_device",
        side_effect=sdm.UnmountError("Mocked unmount error"),
    )
    user = sh.get_user()
    sentinel_text = "This file should not be deleted."
    with pytest.raises(sdm.UnmountError, match="Mocked unmount error"):
        with sdm.mounted_device(btrfs_device, compression) as md:
            sentinel = md / "sentinel-file"
            sdm.chown(md, user, recursive=True)
            sentinel.write_text("This file should not be deleted.")
    assert sentinel.exists(), "Sentinel file was deleted after unmount error."
    assert sentinel.read_text() == sentinel_text
    assert sdm.is_mounted(btrfs_device)
    mocker.stopall()
    sdm.unmount_device(btrfs_device)
    assert not sdm.is_mounted(btrfs_device)
