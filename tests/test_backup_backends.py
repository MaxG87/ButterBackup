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
    if isinstance(config, cp.BtrFSRsyncConfig):
        source_dirs = set(config.Folders)
        source_files = config.Files
    elif isinstance(config, cp.ResticConfig):
        source_dirs = {cur for cur in config.FilesAndFolders if cur.is_dir()}
        source_files = {cur for cur in config.FilesAndFolders if cur.is_file()}
    else:
        # TODO: Use t.assert_never when Python 3.11 is the minimum version!
        raise TypeError(
            f"Unsupported configuration type: {type(config).__name__}. "
            "Expected BtrFSRsyncConfig or ResticConfig."
        )

    expected_content_dirs = get_expected_content_recursive_dir(
        source_dirs, exclude_to_ignore_file
    )
    expected_content_files = get_expected_content_single_files(source_files)
    if isinstance(config, cp.BtrFSRsyncConfig):
        expected_content_files_by_path = {
            Path(config.FilesDest) / key: value
            for key, value in expected_content_files.items()
        }
        return expected_content_dirs | expected_content_files_by_path
    elif isinstance(config, cp.ResticConfig):
        expected_content = expected_content_dirs | expected_content_files
        return Counter(expected_content.values())
    else:
        # TODO: Use t.assert_never when Python 3.11 is the minimum version!
        raise TypeError(
            f"Unsupported configuration type: {type(config).__name__}. "
            "Expected BtrFSRsyncConfig or ResticConfig."
        )


def get_expected_content_recursive_dir(
    source_dirs: set[Path],
    exclude_to_ignore_file: bool,
) -> dict[Path, bytes]:
    expected_content = {}
    for cur_dir in source_dirs:
        cur_content = {
            file.relative_to(cur_dir): file.read_bytes()
            for file in list_files_recursively(cur_dir)
            if exclude_to_ignore_file is False or "ignore" not in file.name
        }
        expected_content.update(cur_content)
    return expected_content


def get_expected_content_single_files(source_files: set[Path]) -> dict[str, bytes]:
    expected_content = {file.name: file.read_bytes() for file in source_files}
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
        # TODO: Use t.assert_never when Python 3.11 is the minimum version!
        raise TypeError(
            f"Unsupported configuration type: {type(config).__name__}. "
            "Expected BtrFSRsyncConfig or ResticConfig."
        )


def get_result_content_for_btrfs(
    config: cp.BtrFSRsyncConfig, mounted: Path
) -> dict[Path, bytes]:
    folder_dest_by_config = next(iter(config.Folders.values()))
    backup_repository = mounted / config.BackupRepositoryFolder
    latest_snapshot = sorted(backup_repository.iterdir())[-1]

    folder_dest_dir = latest_snapshot / folder_dest_by_config
    folder_content = {
        file.relative_to(folder_dest_dir): file.read_bytes()
        for file in list_files_recursively(folder_dest_dir)
    }
    files_dest = latest_snapshot / config.FilesDest
    files_content = {
        file.relative_to(latest_snapshot): file.read_bytes()
        for file in list_files_recursively(files_dest)
    }
    return folder_content | files_content


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
    assert result_content.keys() == expected_content.keys()
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


def test_do_backup_for_btrfs_with_empty_files_creates_directory(
    mounted_device,
) -> None:
    """Test that exercises the code path that causes permission errors in production.

    Background:
    The code at src/butter_backup/backup_backends.py:53 calls files_dest.mkdir()
    without sudo, even when Files is empty. This fails in production when:
    1. A snapshot is created with 'sudo btrfs subvolume snapshot' (owned by root)
    2. Code tries to mkdir in the root-owned snapshot directory
    3. PermissionError is raised

    This test exercises this code path by:
    1. Creating a first backup with FilesDest="FirstFilesDest"
    2. Creating a second backup with FilesDest="SecondFilesDest"
    3. The second backup must create a NEW directory (SecondFilesDest) that
       doesn't exist in the snapshot source

    The test will PASS in environments where sudo preserves user ownership
    (most CI/test environments), but it exercises the same code path that
    fails in production with root-owned snapshots.

    To manually reproduce the production error, run this test in an environment
    where snapshots created by 'sudo btrfs subvolume snapshot' are owned by root.
    """
    empty_config, device = mounted_device
    if not isinstance(empty_config, cp.BtrFSRsyncConfig):
        # This test is specific to BtrFS backend
        return

    # First backup with one FilesDest name
    folder_dest_dir = "some-folder-name"
    config1 = empty_config.model_copy(
        update={
            "Folders": {FIRST_BACKUP: folder_dest_dir},
            "Files": set(),  # Empty Files set - triggers the bug
            "FilesDest": "FirstFilesDest",
        }
    )
    backend1 = bb.BtrFSRsyncBackend(config1)
    backend1.do_backup(device)

    # Verify the first FilesDest directory was created
    backup_repository = device / config1.BackupRepositoryFolder
    first_snapshot = sorted(backup_repository.iterdir())[-1]
    files_dest_first = first_snapshot / config1.FilesDest
    assert files_dest_first.exists()
    assert files_dest_first.is_dir()

    # Second backup with a DIFFERENT FilesDest name to force mkdir to create
    # a new directory that doesn't exist in the source snapshot
    config2 = empty_config.model_copy(
        update={
            "Folders": {FIRST_BACKUP: folder_dest_dir},
            "Files": set(),  # Empty Files set - triggers the bug
            "FilesDest": "SecondFilesDest",  # Different name forces mkdir
        }
    )
    backend2 = bb.BtrFSRsyncBackend(config2)

    # This is the critical line that would fail in production:
    # - Second backup creates snapshot with 'sudo btrfs subvolume snapshot'
    # - Snapshot is owned by root in production
    # - files_dest.mkdir() at src/butter_backup/backup_backends.py:53 is called WITHOUT sudo
    # - PermissionError is raised when trying to create SecondFilesDest
    #
    # In test environments, this passes because snapshots retain user ownership
    backend2.do_backup(device)

    # Verify the second FilesDest directory was created
    second_snapshot = sorted(backup_repository.iterdir())[-1]
    files_dest_second = second_snapshot / config2.FilesDest
    assert files_dest_second.exists()
    assert files_dest_second.is_dir()
