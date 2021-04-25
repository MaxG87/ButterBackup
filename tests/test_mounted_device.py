from pathlib import Path
from tempfile import TemporaryDirectory

import pytest

from butter_backup import __main__ as bb


def test_mounted_device(btrfs_device) -> None:
    with bb.MountedDevice(btrfs_device) as md:
        assert md.exists()
        assert md.is_dir()
        assert bb.is_mounted(btrfs_device)
        assert md in bb.get_mounted_devices().values()
    assert not md.exists()
    assert not bb.is_mounted(btrfs_device)
    assert md not in bb.get_mounted_devices().values()


def test_mounted_device_takes_over_already_mounted_device(btrfs_device) -> None:
    with TemporaryDirectory() as td:
        bb.mount_btrfs_device(btrfs_device, td)
        with bb.MountedDevice(btrfs_device) as md:
            assert bb.is_mounted(btrfs_device)
            assert md == bb.get_mounted_devices()[str(btrfs_device)]
        assert not bb.is_mounted(btrfs_device)


def test_mounted_device_fails_on_not_unmountable_device() -> None:
    for device, mount_point in bb.get_mounted_devices().items():
        if mount_point == Path("/"):
            root = device
            break
    with pytest.raises(UnboundLocalError):
        with bb.MountedDevice(root):
            pass
