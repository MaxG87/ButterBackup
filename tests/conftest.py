from pathlib import Path
from tempfile import TemporaryDirectory

import pytest

from butter_backup import __main__ as bb


@pytest.fixture
def btrfs_device():
    btrfs_dev_size = 128 * 1024 ** 2  # ~109MiB is the minimum size for BtrFS
    with TemporaryDirectory() as tempdir:
        btrfs_device = Path(tempdir) / "btrfs-test-device.iso"
        with btrfs_device.open("wb") as fh:
            fh.write(bytes(btrfs_dev_size))
        bb.run_cmd(cmd=f"sudo mkfs.btrfs {btrfs_device}")
        yield btrfs_device
