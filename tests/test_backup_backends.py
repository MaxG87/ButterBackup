import datetime as dt
import os
from collections import Counter
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Iterable, overload

import pytest
import shell_interface as sh

from butter_backup import backup_backends as bb
from butter_backup import config_parser as cp
from tests import complement_configuration

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
def get_expected_content(
    config: cp.BtrFSRsyncConfig, exclude_to_ignore_file: bool
) -> dict[Path, bytes]: ...


@overload
def get_expected_content(
    config: cp.ResticConfig, exclude_to_ignore_file: bool
) -> Counter[bytes]: ...


def get_expected_content(
    config: cp.Configuration,
    exclude_to_ignore_file: bool,
) -> Counter[bytes] | dict[Path, bytes]:
    source_dir: Path
    if isinstance(config, cp.BtrFSRsyncConfig):
        source_dir = next(iter(config.Folders))
    elif isinstance(config, cp.ResticConfig):
        source_dir = next(iter(config.FilesAndFolders))
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
def get_result_content(
    config: cp.BtrFSRsyncConfig, mounted: Path
) -> dict[Path, bytes]: ...


@overload
def get_result_content(config: cp.ResticConfig, mounted: Path) -> Counter[bytes]: ...


def get_result_content(
    config: cp.Configuration, mounted: Path
) -> Counter[bytes] | dict[Path, bytes]:
    if isinstance(config, cp.BtrFSRsyncConfig):
        return get_result_content_for_btrfs(config, mounted)
    elif isinstance(config, cp.ResticConfig):
        return get_result_content_for_restic(config, mounted)
    else:
        raise TypeError("Unsupported configuration encountered.")


def get_result_content_for_btrfs(
    config: cp.BtrFSRsyncConfig, mounted: Path
) -> dict[Path, bytes]:
    folder_dest_dir = next(iter(config.Folders.values()))
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
        restore_cmd: sh.StrPathList = [
            "restic",
            "-r",
            mounted / config.BackupRepositoryFolder,
            "restore",
            "latest",
            "--target",
            restore_dir,
        ]
        sh.pipe_pass_cmd_to_real_cmd(config.RepositoryPassCmd, restore_cmd)
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
        config = complement_configuration(empty_config, source_dir).model_copy(
            update={"ExcludePatternsFile": EXCLUDE_FILE}
        )
        backend = bb.BackupBackend.from_config(config)
        backend.do_backup(device)
    result_content = get_result_content(config, device)
    expected_content = get_expected_content(config, exclude_to_ignore_file=True)
    assert result_content == expected_content


@pytest.mark.parametrize(
    "first_source, second_source",
    [(FIRST_BACKUP, SECOND_BACKUP)],
)
def test_do_backup_removes_existing_files_in_exclude_list(
    first_source, second_source, mounted_device
) -> None:
    # This test ensures that files are removed even if they are matched by the
    # exclude patterns.
    # Imagine that a user has existing backups. Then she creates an
    # ExcludePatternsFile or adds a rule to it. Anyhow, imagine that now the
    # ExcludePatternsFile contains a rule that matches files that already exist
    # in the existing backups.
    # A prior version of BtrFSRsyncBackend would not delete files that would
    # match a rule in the ExcludePatternsFile. This lead to plenty of error
    # messages when rsync then attempted to remove the folder where the
    # not-deleted file was contained, because that folder was not empty.
    # However, if the folder is gone in the source, it must be removed in the
    # backup too.
    # This test explicitly tests this scenario.

    empty_config, device = mounted_device

    first_config = complement_configuration(empty_config, first_source)
    first_backend = bb.BackupBackend.from_config(first_config)
    first_backend.do_backup(device)

    second_config = complement_configuration(empty_config, second_source).model_copy(
        update={"ExcludePatternsFile": EXCLUDE_FILE}
    )
    second_backend = bb.BackupBackend.from_config(second_config)
    second_backend.do_backup(device)

    result_content = get_result_content(second_config, device)
    expected_content = get_expected_content(second_config, exclude_to_ignore_file=True)
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
    config = empty_config.model_copy(
        update={"Folders": {FIRST_BACKUP: folder_dest_dir}}
    )
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
