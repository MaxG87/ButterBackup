from __future__ import annotations

import shutil
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

import pytest
import storage_device_managers as sdm

from butter_backup import device_managers as dm


def get_random_filename(dir_: str) -> str:
    with NamedTemporaryFile(dir=dir_) as ntf:
        pass
    return ntf.name


@pytest.fixture(scope="session")
def _big_file_persistent():
    min_size = 128 * 1024**2  # ~109MiB is the minimum size for BtrFS
    with TemporaryDirectory() as tempdir:
        filename = get_random_filename(dir_=tempdir)
        file = Path(filename)
        with file.open("wb") as fh:
            fh.write(bytes(min_size))
        yield file


@pytest.fixture
def big_file(_big_file_persistent):
    """
    Prepare a file of minimum size for BtrFS and return its path
    """
    with NamedTemporaryFile() as ntf:
        big_file = Path(ntf.name)
        shutil.copy(_big_file_persistent, big_file)
        yield big_file


@pytest.fixture
def encrypted_btrfs_device(big_file):
    """
    Prepare device for ButterBackup and return its config

    Returns
    -------
    config: BtrfsConfig
        configuration allowing to interact with the returned device
    """
    config = dm.prepare_device_for_butterbackend(big_file, fast_and_insecure=True)
    with sdm.symbolic_link(big_file, config.device()):
        yield config


@pytest.fixture
def encrypted_restic_device(big_file):
    """
    Prepare device for Restic on BtrFS and return its config

    Returns
    -------
    config: ResticConfig
        configuration allowing to interact with the returned device
    """
    config = dm.prepare_device_for_resticbackend(big_file, fast_and_insecure=True)
    with sdm.symbolic_link(big_file, config.device()):
        yield config


@pytest.fixture(params=["encrypted_btrfs_device", "encrypted_restic_device"])
def encrypted_device(request):
    config = request.getfixturevalue(request.param)
    return config


@pytest.fixture
def mounted_device(encrypted_device):
    config = encrypted_device
    with sdm.decrypted_device(config.device(), config.DevicePassCmd) as decrypted:
        with sdm.mounted_device(decrypted, config.Compression) as mounted_device:
            yield config, mounted_device
