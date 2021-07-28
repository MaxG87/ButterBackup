from pathlib import Path
from tempfile import TemporaryDirectory

import pytest

from butter_backup import device_managers as dm


def test_mounted_device(btrfs_device) -> None:
    with dm.mounted_device(btrfs_device) as md:
        assert md.exists()
        assert md.is_dir()
        assert dm.is_mounted(btrfs_device)
        assert md in dm.get_mounted_devices().values()
    assert not md.exists()
    assert not dm.is_mounted(btrfs_device)
    assert md not in dm.get_mounted_devices().values()


def test_mounted_device_takes_over_already_mounted_device(btrfs_device) -> None:
    with TemporaryDirectory() as td:
        dm.mount_btrfs_device(btrfs_device, Path(td))
        with dm.mounted_device(btrfs_device) as md:
            assert dm.is_mounted(btrfs_device)
            assert md == dm.get_mounted_devices()[str(btrfs_device)]
        assert not dm.is_mounted(btrfs_device)


def test_mounted_device_fails_on_not_unmountable_device() -> None:
    for device, mount_point in dm.get_mounted_devices().items():
        if mount_point == Path("/"):
            root = Path(device)
            break
    with pytest.raises(UnboundLocalError):
        with dm.mounted_device(root):
            pass


@pytest.mark.parametrize("dest", dm.get_mounted_devices())
def test_is_mounted_detects(dest: Path) -> None:
    assert dm.is_mounted(dest)


def test_is_mounted_rejects() -> None:
    with TemporaryDirectory() as tempd:
        assert not dm.is_mounted(Path(tempd))


def test_get_mounted_devices_includes_correct_mountpoints(mounted_directories) -> None:
    src, mountpoint = mounted_directories
    assert mountpoint in dm.get_mounted_devices().values()


def test_unmount_device(btrfs_device) -> None:
    with TemporaryDirectory() as mountpoint:
        dm.mount_btrfs_device(btrfs_device, Path(mountpoint))
        dm.unmount_device(btrfs_device)
        assert not dm.is_mounted(btrfs_device)


def test_decrypted_device(encrypted_device) -> None:
    map_name = "decrypted_device_test"
    passphrase, device = encrypted_device
    with dm.decrypted_device(
        device=device, map_name=map_name, pass_cmd=f"echo {passphrase}"
    ) as dd:
        assert dd.exists()
        assert dd.is_symlink()
    assert not dd.exists()
