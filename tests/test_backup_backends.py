from __future__ import annotations

import datetime as dt
import os
import time
from collections import Counter
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Dict, Iterable, Union, overload

import pytest

from butter_backup import backup_backends as bb
from butter_backup import config_parser as cp
from butter_backup import shell_interface as sh

TEST_RESOURCES = Path(__file__).parent / "resources"
EXCLUDE_FILE = TEST_RESOURCES / "exclude-file"
FIRST_BACKUP = TEST_RESOURCES / "first-backup"
SECOND_BACKUP = TEST_RESOURCES / "second-backup"
USER = os.environ.get("USER", "root")


def list_files_recursively(path: Path) -> Iterable[Path]:
    for file_or_folder in path.rglob("*"):
        if file_or_folder.is_file():
            yield file_or_folder


@overload
def complement_configuration(
    config: cp.BtrFSRsyncConfig, source_dir: Path
) -> cp.BtrFSRsyncConfig:
    ...


@overload
def complement_configuration(
    config: cp.ResticConfig, source_dir: Path
) -> cp.ResticConfig:
    ...


def complement_configuration(
    config: cp.Configuration, source_dir: Path
) -> cp.Configuration:
    if isinstance(config, cp.BtrFSRsyncConfig):
        folder_dest_dir = "some-folder-name"
        return config.copy(update={"Folders": {source_dir: folder_dest_dir}})
    if isinstance(config, cp.ResticConfig):
        return config.copy(update={"FilesAndFolders": {source_dir}})
    raise TypeError("Unsupported configuration encountered.")


@overload
def get_expected_content(
    config: cp.BtrFSRsyncConfig, exclude_to_ignore_file: bool
) -> Dict[Path, bytes]:
    ...


@overload
def get_expected_content(
    config: cp.ResticConfig, exclude_to_ignore_file: bool
) -> Counter[bytes]:
    ...


def get_expected_content(
    config: cp.Configuration,
    exclude_to_ignore_file: bool,
) -> Union[Counter[bytes], Dict[Path, bytes]]:
    source_dir: Path
    if isinstance(config, cp.BtrFSRsyncConfig):
        source_dir = list(config.Folders.keys())[0]
    elif isinstance(config, cp.ResticConfig):
        source_dir = list(config.FilesAndFolders)[0]
    else:
        raise TypeError("Unsupported configuration encountered.")
    expected_content = {
        file.relative_to(source_dir): file.read_bytes()
        for file in list_files_recursively(source_dir)
        if exclude_to_ignore_file is False or "ignore" not in file.name
    }
    if isinstance(config, cp.ResticConfig):
        return Counter(expected_content.values())
    return expected_content


@overload
def get_result_content(config: cp.BtrFSRsyncConfig, mounted: Path) -> Dict[Path, bytes]:
    ...


@overload
def get_result_content(config: cp.ResticConfig, mounted: Path) -> Counter[bytes]:
    ...


def get_result_content(
    config: cp.Configuration, mounted: Path
) -> Union[Counter[bytes], Dict[Path, bytes]]:
    if isinstance(config, cp.BtrFSRsyncConfig):
        return get_result_content_for_btrfs(config, mounted)
    elif isinstance(config, cp.ResticConfig):
        return get_result_content_for_restic(config, mounted)
    else:
        raise TypeError("Unsupported configuration encountered.")


def get_result_content_for_btrfs(
    config: cp.BtrFSRsyncConfig, mounted: Path
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
                "restic",
                "-r",
                mounted / config.BackupRepositoryFolder,
                "restore",
                "latest",
                "--target",
                restore_dir,
            ],
        )
        return Counter(
            file.read_bytes() for file in list_files_recursively(Path(restore_dir))
        )


@pytest.mark.parametrize(
    "source_directories",
    [[FIRST_BACKUP], [SECOND_BACKUP], [FIRST_BACKUP, SECOND_BACKUP]],
)
def test_do_backup(source_directories, mounted_device) -> None:
    empty_config, device = mounted_device
    for source_dir in source_directories:
        time.sleep(1)  # prevent conflicts in snapshot names
        config = complement_configuration(empty_config, source_dir)
        backend = bb.BackupBackend.from_config(config)
        backend.do_backup(device)
    result_content = get_result_content(config, device)
    expected_content = get_expected_content(config, exclude_to_ignore_file=False)
    assert result_content == expected_content


@pytest.mark.parametrize(
    "source_directories",
    [[FIRST_BACKUP], [SECOND_BACKUP], [FIRST_BACKUP, SECOND_BACKUP]],
)
def test_do_backup_handles_exclude_list(source_directories, mounted_device) -> None:
    empty_config, device = mounted_device
    for source_dir in source_directories:
        time.sleep(1)  # prevent conflicts in snapshot names
        config = complement_configuration(empty_config, source_dir).copy(
            update={"ExcludePatternsFile": EXCLUDE_FILE}
        )
        backend = bb.BackupBackend.from_config(config)
        backend.do_backup(device)
    result_content = get_result_content(config, device)
    expected_content = get_expected_content(config, exclude_to_ignore_file=True)
    assert result_content == expected_content


def test_do_backup_for_btrfs_creates_snapshots_with_timestamp_names(
    mounted_device,
) -> None:
    empty_config, device = mounted_device
    if not isinstance(empty_config, cp.BtrFSRsyncConfig):
        # This test works for BtrfsConfig only. However, encrypted_device on
        # which mounted_device depends on, is parameterised over all backends.
        # Since this simplifies many other tests it seemed to be an acceptable
        # tradeoff to short-circuit the test here.
        return
    folder_dest_dir = "some-folder-name"
    config = empty_config.copy(update={"Folders": {FIRST_BACKUP: folder_dest_dir}})
    backend = bb.BtrFSRsyncBackend(config)
    backend.do_backup(device)
    backup_repository = device / config.BackupRepositoryFolder
    latest_folder = sorted(backup_repository.iterdir())[-1]
    expected_date = dt.date.today().isoformat()
    assert expected_date in str(latest_folder)


@pytest.mark.parametrize(
    "source_directories",
    [[FIRST_BACKUP], [SECOND_BACKUP], [FIRST_BACKUP, SECOND_BACKUP]],
)
def test_do_backup_for_restic_adapts_ownership(
    source_directories, mounted_device
) -> None:
    empty_config, device = mounted_device
    if not isinstance(empty_config, cp.ResticConfig):
        # This test works for ResticConfig only. However, encrypted_device on
        # which mounted_device depends on, is parameterised over all backends.
        # Since this simplifies many other tests it seemed to be an acceptable
        # tradeoff to short-circuit the test here.
        return
    for source_dir in source_directories:
        config = complement_configuration(empty_config, source_dir)
        backend = bb.BackupBackend.from_config(config)
        backend.do_backup(device)

    expected_user = sh.get_user()
    expected_group = sh.get_group(expected_user)
    found_user = {
        cur_f.owner() for cur_f in (device / config.BackupRepositoryFolder).rglob("*")
    }
    found_group = {
        cur_f.group() for cur_f in (device / config.BackupRepositoryFolder).rglob("*")
    }
    assert found_user == {expected_user}
    assert found_group == {expected_group}
