from pathlib import Path
from tempfile import TemporaryDirectory

import pytest

from butter_backup import __main__ as bb


@pytest.fixture
def mounted_directories():
    with TemporaryDirectory() as src:
        with TemporaryDirectory() as mountpoint:
            bb.run_cmd(cmd=f"sudo mount -o bind {src} {mountpoint}")
            yield Path(src), Path(mountpoint)
            bb.run_cmd(cmd=f"sudo umount {mountpoint}")


def test_useful_error_on_missing_file_name() -> None:
    missing_cfg = Path("/path/to/nowhere/butter-backup.cfg")
    with pytest.raises(SystemExit) as sysexit:
        bb.load_configuration(missing_cfg)
    assert str(missing_cfg) in str(sysexit.value)
    assert "--help" in str(sysexit.value)


@pytest.mark.parametrize("dest", bb.get_mounted_devices())
def test_is_mounted_detects(dest: Path) -> None:
    assert bb.is_mounted(dest)


def test_is_mounted_rejects() -> None:
    with TemporaryDirectory() as tempd:
        assert not bb.is_mounted(Path(tempd))


def test_get_mounted_devices_includes_correct_mountpoints(mounted_directories) -> None:
    src, mountpoint = mounted_directories
    assert mountpoint in bb.get_mounted_devices().values()


def test_unmount_device(btrfs_device) -> None:
    with TemporaryDirectory() as mountpoint:
        bb.mount_btrfs_device(btrfs_device, Path(mountpoint))
        bb.unmount_device(btrfs_device)
        assert not bb.is_mounted(btrfs_device)
