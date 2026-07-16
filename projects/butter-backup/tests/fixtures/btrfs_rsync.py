import shutil
import typing as t
import uuid
from contextlib import contextmanager
from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest
import storage_device_managers as sdm

from butter_backup import config_parser as cp
from butter_backup import device_managers as dm
from tests import DEVICE_NAMES


@contextmanager
def _btrfs_device_for_name(
    persistent: tuple[Path, cp.BtrFSRsyncConfig],
    device_name: str,
) -> t.Iterator[cp.BtrFSRsyncConfig]:
    # This implementation resets the UUID of the configuration. This causes the
    # partition's UUID and the configuration's UUID to differ. Since the only
    # relevance of the partition's UUID is to allow Linux to create the device
    # node in /dev/disk/by-uuid, this is not a problem.
    old_fs_file, old_config = persistent
    with NamedTemporaryFile() as ntf:
        big_file = Path(ntf.name)
        shutil.copy(old_fs_file, big_file)
        update: dict[str, str | uuid.UUID] = {"UUID": uuid.uuid4()}
        if device_name is not None:
            update["Name"] = device_name
        config = old_config.model_copy(update=update)
        with sdm.symbolic_link(big_file, config.device()):
            yield config


@pytest.fixture(scope="session")
def _encrypted_btrfs_device_persistent(
    _big_file_persistent,
):
    with NamedTemporaryFile() as ntf:
        big_file = Path(ntf.name)
        shutil.copy(_big_file_persistent, big_file)
        config = dm.prepare_device_for_butterbackend(big_file)
        yield big_file, config


@pytest.fixture(params=DEVICE_NAMES)
def encrypted_btrfs_device(
    _encrypted_btrfs_device_persistent,
    request: pytest.FixtureRequest,
) -> t.Iterator[cp.BtrFSRsyncConfig]:
    """
    Prepare device for ButterBackup and return its config

    Returns
    -------
    config: BtrfsConfig
        configuration allowing to interact with the returned device
    """
    with _btrfs_device_for_name(
        _encrypted_btrfs_device_persistent, request.param
    ) as config:
        yield config
