import datetime as dt
import os
import time
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Iterable

import pytest

from butter_backup import backup_logic as bl
from butter_backup import device_managers as dm
from butter_backup import shell_interface as sh

USER = os.environ.get("USER", "root")
TEST_RESOURCES = Path(__file__).parent / "resources"
FIRST_BACKUP = TEST_RESOURCES / "first-backup"
SECOND_BACKUP = TEST_RESOURCES / "second-backup"


def list_files_recursively(path: Path) -> Iterable[Path]:
    for file_or_folder in path.rglob("*"):
        if file_or_folder.is_file():
            yield file_or_folder


@pytest.mark.parametrize(
    "source_directories",
    [[FIRST_BACKUP], [SECOND_BACKUP], [FIRST_BACKUP, SECOND_BACKUP]],
)
def test_do_backup_for_butterbackend(
    source_directories, encrypted_btrfs_device
) -> None:
    # Due to testing a timestamp, this test has a small chance to fail around
    # midnight. Since this is a hobby project, paying attention if the test
    # executes around midnight is a viable solution.
    empty_config, device = encrypted_btrfs_device
    folder_dest_dir = "some-folder-name"
    for source_dir in source_directories:
        config = empty_config.copy(update={"Folders": {source_dir: folder_dest_dir}})
        bl.do_backup(config)
        time.sleep(1)  # prevent conflicts in snapshot names
    with dm.decrypted_device(device, config.DevicePassCmd) as decrypted:
        with dm.mounted_device(decrypted) as mount_dir:
            latest_folder = sorted(mount_dir.iterdir())[-1]
            content = {
                file.relative_to(latest_folder / folder_dest_dir): file.read_bytes()
                for file in list_files_recursively(latest_folder)
            }
    latest_source_dir = source_directories[-1]
    expected_date = dt.date.today().isoformat()
    expected_content = {
        file.relative_to(latest_source_dir): file.read_bytes()
        for file in list_files_recursively(latest_source_dir)
    }
    assert expected_date in str(latest_folder)
    assert content == expected_content


@pytest.mark.parametrize(
    "source_directories",
    [[FIRST_BACKUP], [SECOND_BACKUP], [FIRST_BACKUP, SECOND_BACKUP]],
)
def test_do_backup_for_resticbackend(
    source_directories, encrypted_restic_device
) -> None:
    # Restic keeps track of the absolute path that was backed up. This makes it
    # quite hard to construct apporpriate file names for a dictionary
    # comparison. It seems to be a viable tradeoff to just test the file
    # contents, without file names here.
    empty_config, device = encrypted_restic_device
    for source_dir in source_directories:
        config = empty_config.copy(update={"FilesAndFolders": {source_dir}})
        bl.do_backup(config)
    with dm.decrypted_device(device, config.DevicePassCmd) as decrypted:
        with dm.mounted_device(decrypted) as mount_dir:
            with TemporaryDirectory() as restore_dir:
                sh.pipe_pass_cmd_to_real_cmd(
                    config.RepositoryPassCmd,
                    [
                        "sudo",
                        "restic",
                        "-r",
                        mount_dir,
                        "restore",
                        "latest",
                        "--target",
                        restore_dir,
                    ],
                )
                # Fix permissions to be able to list directory without sudo-helpers.
                sh.run_cmd(cmd=["sudo", "chown", "-R", USER, restore_dir])
                content = {
                    file.read_bytes()
                    for file in list_files_recursively(Path(restore_dir))
                }
    latest_source_dir = source_directories[-1]
    expected_content = {
        file.read_bytes() for file in list_files_recursively(latest_source_dir)
    }
    assert content == expected_content
