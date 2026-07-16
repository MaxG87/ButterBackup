import itertools
import typing as t
from pathlib import Path

import pytest
import storage_device_managers as sdm

from butter_backup import backup_backends as bb
from butter_backup import config_parser as cp

from . import complement_configuration, device_for_name

DEVICE_NAMES = ["Seagate Rot", "MyBackupDevice", None]

pytest_plugins = [
    "tests.fixtures.big_file",
    "tests.fixtures.btrfs_rsync",
    "tests.fixtures.restic",
]


@pytest.fixture(
    params=[
        pytest.param((kind, device_name), id=f"{kind}-{device_name}")
        for kind, device_name in itertools.product(["btrfs", "restic"], DEVICE_NAMES)
    ],
)
def encrypted_device(request) -> t.Iterable[cp.DeviceConfiguration]:
    kind, device_name = request.param
    if kind == "btrfs":
        persistent = request.getfixturevalue("_encrypted_btrfs_device_persistent")
    elif kind == "restic":
        persistent = request.getfixturevalue("_encrypted_restic_device_persistent")
    else:
        raise ValueError(f"Unknown device kind: {kind}")

    with device_for_name(persistent, device_name) as config:
        yield config


@pytest.fixture
def mounted_device(encrypted_device) -> t.Iterator[tuple[cp.DeviceConfiguration, Path]]:
    config = encrypted_device
    with (
        sdm.decrypted_device(config.device(), config.DevicePassCmd) as decrypted,
        sdm.mounted_device(
            decrypted, compression=config.compression()
        ) as mounted_device,
    ):
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
