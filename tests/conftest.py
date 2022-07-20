from __future__ import annotations

import uuid
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

import pytest

from butter_backup import device_managers as dm
from butter_backup import shell_interface as sh


def get_random_filename(dir_: str) -> str:
    with NamedTemporaryFile(dir=dir_) as ntf:
        pass
    return ntf.name


@pytest.fixture
def mounted_directories():
    with TemporaryDirectory() as src:
        with TemporaryDirectory() as mountpoint:
            sh.run_cmd(cmd=["sudo", "mount", "-o", "bind", src, mountpoint])
            yield Path(src), Path(mountpoint)
            sh.run_cmd(cmd=["sudo", "umount", mountpoint])


@pytest.fixture
def big_file():
    min_size = 128 * 1024**2  # ~109MiB is the minimum size for BtrFS
    with TemporaryDirectory() as tempdir:
        filename = get_random_filename(dir_=tempdir)
        file = Path(filename)
        with file.open("wb") as fh:
            fh.write(bytes(min_size))
        yield file


@pytest.fixture
def virgin_device(big_file):
    device_uuid = uuid.uuid4()
    device = Path("/dev/disk/by-uuid/") / str(device_uuid)
    with dm.symbolic_link(big_file, device):
        yield device_uuid, device


@pytest.fixture
def encrypted_btrfs_device(virgin_device):
    """
    Prepare device for ButterBackup and return its config

    Returns
    -------
    config: BtrfsConfig
        configuration allowing to interact with the returned device
    device: Path
        temporary file prepared as encrypted BtrFS device
    """
    device_uuid, device = virgin_device
    config = dm.prepare_device_for_butterbackend(device_uuid)
    yield config


@pytest.fixture
def encrypted_restic_device(virgin_device):
    """
    Prepare device for Restic on BtrFS and return its config

    Returns
    -------
    config: ResticConfig
        configuration allowing to interact with the returned device
    device: Path
        temporary file prepared as encrypted BtrFS device
    """
    device_uuid, device = virgin_device
    config = dm.prepare_device_for_resticbackend(device_uuid)
    return config


@pytest.fixture(params=["encrypted_btrfs_device", "encrypted_restic_device"])
def encrypted_device(request):
    config = request.getfixturevalue(request.param)
    return config


@pytest.fixture
def btrfs_device(encrypted_btrfs_device):
    config = encrypted_btrfs_device
    with dm.decrypted_device(config.device(), config.DevicePassCmd) as decrypted:
        yield decrypted


@pytest.fixture
def mounted_btrfs_device(encrypted_device):
    config = encrypted_device
    with dm.decrypted_device(config.device(), config.DevicePassCmd) as decrypted:
        with dm.mounted_device(decrypted) as mounted_device:
            yield config, mounted_device
