import os
import time
from collections import Counter
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Dict, Iterable, Union, overload

import pytest

from butter_backup import backup_logic as bl
from butter_backup import config_parser as cp
from butter_backup import device_managers as dm
from butter_backup import shell_interface as sh

TEST_RESOURCES = Path(__file__).parent / "resources"
FIRST_BACKUP = TEST_RESOURCES / "first-backup"
SECOND_BACKUP = TEST_RESOURCES / "second-backup"
USER = os.environ.get("USER", "root")


def list_files_recursively(path: Path) -> Iterable[Path]:
    for file_or_folder in path.rglob("*"):
        if file_or_folder.is_file():
            yield file_or_folder


@overload
def get_expected_content(config: cp.BtrfsConfig) -> Dict[Path, bytes]:
    ...


@overload
def get_expected_content(config: cp.ResticConfig) -> Counter[bytes]:
    ...


def get_expected_content(
    config: cp.Configuration,
) -> Union[Counter[bytes], Dict[Path, bytes]]:
    source_dir: Path
    if isinstance(config, cp.BtrfsConfig):
        source_dir = list(config.Folders.keys())[0]
    elif isinstance(config, cp.ResticConfig):
        source_dir = list(config.FilesAndFolders)[0]
    else:
        raise TypeError("Unsupported configuration encountered.")
    expected_content = {
        file.relative_to(source_dir): file.read_bytes()
        for file in list_files_recursively(source_dir)
    }
    if isinstance(config, cp.ResticConfig):
        return Counter(expected_content.values())
    return expected_content


@overload
def get_result_content(config: cp.BtrfsConfig) -> Dict[Path, bytes]:
    ...


@overload
def get_result_content(config: cp.ResticConfig) -> Counter[bytes]:
    ...


def get_result_content(
    config: cp.Configuration,
) -> Union[Counter[bytes], Dict[Path, bytes]]:
    with dm.decrypted_device(config.device(), config.DevicePassCmd) as decrypted:
        with dm.mounted_device(decrypted) as mounted:
            if isinstance(config, cp.BtrfsConfig):
                return get_result_content_for_btrfs(config, mounted)
            elif isinstance(config, cp.ResticConfig):
                return get_result_content_for_restic(config, mounted)
            else:
                raise TypeError("Unsupported configuration encountered.")


def get_result_content_for_btrfs(
    config: cp.BtrfsConfig, mounted: Path
) -> Dict[Path, bytes]:
    folder_dest_dir = list(config.Folders.values())[0]
    backup_repository = mounted / config.BackupRepositoryFolder
    latest_folder = sorted(backup_repository.iterdir())[-1]
    return {
        file.relative_to(latest_folder / folder_dest_dir): file.read_bytes()
        for file in list_files_recursively(latest_folder)
    }


def get_result_content_for_restic(
    config: cp.ResticConfig, mounted: Path
) -> Counter[bytes]:
    with TemporaryDirectory() as restore_dir:
        sh.pipe_pass_cmd_to_real_cmd(
            config.RepositoryPassCmd,
            [
                "sudo",
                "restic",
                "-r",
                mounted / config.BackupRepositoryFolder,
                "restore",
                "latest",
                "--target",
                restore_dir,
            ],
        )
        # Fix permissions to be able to list directory without sudo-helpers.
        sh.run_cmd(cmd=["sudo", "chown", "-R", USER, restore_dir])
        return Counter(
            file.read_bytes() for file in list_files_recursively(Path(restore_dir))
        )


@pytest.mark.parametrize(
    "source_directories",
    [[FIRST_BACKUP], [SECOND_BACKUP], [FIRST_BACKUP, SECOND_BACKUP]],
)
def test_do_backup(source_directories, encrypted_device) -> None:
    empty_config, device = encrypted_device
    config: Union[cp.BtrfsConfig, cp.ResticConfig]
    for source_dir in source_directories:
        if isinstance(empty_config, cp.BtrfsConfig):
            folder_dest_dir = "some-folder-name"
            config = empty_config.copy(
                update={"Folders": {source_dir: folder_dest_dir}}
            )
        elif isinstance(empty_config, cp.ResticConfig):
            config = empty_config.copy(update={"FilesAndFolders": {source_dir}})
        else:
            raise TypeError("Unsupported configuration encountered.")
        bl.do_backup(config)
        time.sleep(1)  # prevent conflicts in snapshot names
    result_content = get_result_content(config)
    expected_content = get_expected_content(config)
    assert result_content == expected_content
