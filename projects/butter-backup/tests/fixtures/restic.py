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
def _restic_device_for_name(
    persistent: tuple[Path, cp.ResticConfig],
    device_name: str,
) -> t.Iterator[cp.ResticConfig]:
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
def _encrypted_restic_device_persistent(_big_file_persistent):
    with NamedTemporaryFile() as ntf:
        big_file = Path(ntf.name)
        shutil.copy(_big_file_persistent, big_file)
        config = dm.prepare_device_for_resticbackend(big_file, "ext4")
        yield big_file, config


@pytest.fixture(params=DEVICE_NAMES)
def encrypted_restic_device(
    _encrypted_restic_device_persistent,
    request: pytest.FixtureRequest,
) -> t.Iterator[cp.ResticConfig]:
    """
    Prepare device for Restic on BtrFS and return its config

    Returns
    -------
    config: ResticConfig
        configuration allowing to interact with the returned device
    """
    with _restic_device_for_name(
        _encrypted_restic_device_persistent, request.param
    ) as config:
        yield config
