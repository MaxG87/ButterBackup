import itertools
import shutil
import typing as t
import uuid
from contextlib import AbstractContextManager, contextmanager
from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest
import storage_device_managers as sdm

from butter_backup import backup_backends as bb
from butter_backup import config_parser as cp

from . import complement_configuration

DEVICE_NAMES = ["Seagate Rot", "MyBackupDevice", None]

pytest_plugins = [
    "tests.fixtures.big_file",
    "tests.fixtures.btrfs_rsync",
    "tests.fixtures.restic",
]


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


@pytest.fixture(
    params=[
        pytest.param((kind, device_name), id=f"{kind}-{device_name}")
        for kind, device_name in itertools.product(["btrfs", "restic"], DEVICE_NAMES)
    ],
)
def encrypted_device(request) -> t.Iterable[cp.DeviceConfiguration]:
    kind, device_name = request.param
    device_context_manager: (
        t.Callable[
            [tuple[Path, cp.BtrFSRsyncConfig], str],
            AbstractContextManager[cp.BtrFSRsyncConfig],
        ]
        | t.Callable[
            [tuple[Path, cp.ResticConfig], str],
            AbstractContextManager[cp.ResticConfig],
        ]
    )
    if kind == "btrfs":
        persistent = request.getfixturevalue("_encrypted_btrfs_device_persistent")
        device_context_manager = _btrfs_device_for_name
    elif kind == "restic":
        persistent = request.getfixturevalue("_encrypted_restic_device_persistent")
        device_context_manager = _restic_device_for_name
    else:
        raise ValueError(f"Unknown device kind: {kind}")

    with device_context_manager(persistent, device_name) as config:
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
