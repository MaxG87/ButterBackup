import shutil
import uuid
from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest
import storage_device_managers as sdm

from butter_backup import backup_backends as bb
from butter_backup import config_parser as cp
from butter_backup import device_managers as dm

from . import complement_configuration


@pytest.fixture(scope="session")
def _big_file_persistent():
    """
    Prepare a file of minimum size for BtrFS and return its path
    """

    min_size = 128 * 1024**2  # ~109MiB is the minimum size for BtrFS
    with NamedTemporaryFile() as ntf:
        file = Path(ntf.name)
        file.write_bytes(bytes(min_size))
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


@pytest.fixture(scope="session")
def _encrypted_btrfs_device_persistent(
    _big_file_persistent,
):
    with NamedTemporaryFile() as ntf:
        big_file = Path(ntf.name)
        shutil.copy(_big_file_persistent, big_file)
        config = dm.prepare_device_for_butterbackend(big_file)
        yield big_file, config


@pytest.fixture
def encrypted_btrfs_device(_encrypted_btrfs_device_persistent):
    """
    Prepare device for ButterBackup and return its config

    Returns
    -------
    config: BtrfsConfig
        configuration allowing to interact with the returned device
    """

    # This implementation resets the UUID of the configuration. This causes the
    # partition's UUID and the configuration's UUID to differ. Since the only
    # relevance of the partition's UUID is to allow Linux to create the device
    # node in /dev/disk/by-uuid, this is not a problem.
    old_fs_file, old_config = _encrypted_btrfs_device_persistent
    with NamedTemporaryFile() as ntf:
        big_file = Path(ntf.name)
        shutil.copy(old_fs_file, big_file)
        config = old_config.model_copy(update={"UUID": uuid.uuid4()})
        with sdm.symbolic_link(big_file, config.device()):
            yield config


@pytest.fixture(scope="session")
def _encrypted_restic_device_persistent(_big_file_persistent):
    with NamedTemporaryFile() as ntf:
        big_file = Path(ntf.name)
        shutil.copy(_big_file_persistent, big_file)
        config = dm.prepare_device_for_resticbackend(big_file)
        yield big_file, config


@pytest.fixture
def encrypted_restic_device(_encrypted_restic_device_persistent):
    """
    Prepare device for Restic on BtrFS and return its config

    Returns
    -------
    config: ResticConfig
        configuration allowing to interact with the returned device
    """

    # This implementation resets the UUID of the configuration. This causes the
    # partition's UUID and the configuration's UUID to differ. Since the only
    # relevance of the partition's UUID is to allow Linux to create the device
    # node in /dev/disk/by-uuid, this is not a problem.
    old_fs_file, old_config = _encrypted_restic_device_persistent
    with NamedTemporaryFile() as ntf:
        big_file = Path(ntf.name)
        shutil.copy(old_fs_file, big_file)
        config = old_config.model_copy(update={"UUID": uuid.uuid4()})
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
            if isinstance(config, cp.BtrFSRsyncConfig):
                # Ensure `FilesDest` is a file, initially. This ensures correct handling
                # of single files backup, even if an existing backup suffered from the
                # erroneous behaviour of making FilesDest a file.
                complemented = complement_configuration(config, Path("."))
                backup_root = mounted_device / complemented.BackupRepositoryFolder
                source_snapshot = bb.BtrFSRsyncBackend.get_source_snapshot(backup_root)
                files_dest = source_snapshot / complemented.FilesDest
                files_dest.touch()
            yield config, mounted_device
