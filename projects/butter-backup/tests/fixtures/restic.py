import shutil
import typing as t
from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest

from butter_backup import config_parser as cp
from butter_backup import device_managers as dm
from tests import DEVICE_NAMES, device_for_name


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
    with device_for_name(_encrypted_restic_device_persistent, request.param) as config:
        yield config
