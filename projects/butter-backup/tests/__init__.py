import shutil
import typing as t
import uuid
from contextlib import contextmanager
from pathlib import Path
from tempfile import NamedTemporaryFile

import storage_device_managers as sdm

from butter_backup import config_parser as cp

DEVICE_NAMES = ["Seagate Rot", "MyBackupDevice", None]


def get_random_filename() -> str:
    with NamedTemporaryFile() as named_file:
        return named_file.name


@t.overload
def complement_configuration(
    config: cp.BtrFSRsyncConfig, source_dir: Path
) -> cp.BtrFSRsyncConfig: ...


@t.overload
def complement_configuration(
    config: cp.ResticConfig, source_dir: Path
) -> cp.ResticConfig: ...


def complement_configuration(
    config: cp.DeviceConfiguration, source_dir: Path
) -> cp.DeviceConfiguration:
    """
    Register to configuration some files and folders that should be backed up

    This is used to test the handling of both files and folders in the backup backends.
    """

    folders_root = source_dir / "backup-root"
    single_files = {
        source_dir / "config" / "docker" / "daemon.json",
        source_dir / "etc" / "fstab",
        source_dir / "cache" / "randomfile.bin",
    }
    match config:
        case cp.BtrFSRsyncConfig():
            folder_dest_dir = "some-folder-name"
            return config.model_copy(
                update={
                    "Folders": {folders_root: folder_dest_dir},
                    "Files": single_files,
                    "FilesDest": "Einzeldateien",
                }
            )
        case cp.ResticConfig():
            return config.model_copy(
                update={"FilesAndFolders": {folders_root}.union(single_files)}
            )
        case _:
            t.assert_never(config)


@t.overload
@contextmanager
def device_for_name(
    persistent: tuple[Path, cp.BtrFSRsyncConfig],
    device_name: str,
) -> t.Iterator[cp.BtrFSRsyncConfig]: ...
@t.overload
@contextmanager
def device_for_name(
    persistent: tuple[Path, cp.ResticConfig],
    device_name: str,
) -> t.Iterator[cp.ResticConfig]: ...
@contextmanager
def device_for_name(
    persistent: tuple[Path, cp.BtrFSRsyncConfig | cp.ResticConfig],
    device_name: str,
) -> t.Iterator[cp.BtrFSRsyncConfig] | t.Iterator[cp.ResticConfig]:
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
