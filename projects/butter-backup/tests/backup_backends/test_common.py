import os
from pathlib import Path

import pytest

from . import get_expected_content, get_result_content, run_backup_cycle

TEST_RESOURCES = Path(__file__).parent / "resources"
EXCLUDE_FILE = TEST_RESOURCES / "exclude-file"
FIRST_BACKUP = TEST_RESOURCES / "first-backup"
SECOND_BACKUP = TEST_RESOURCES / "second-backup"
USER = os.environ.get("USER", "root")


@pytest.mark.parametrize(
    "source_directories",
    [[FIRST_BACKUP], [SECOND_BACKUP], [FIRST_BACKUP, SECOND_BACKUP]],
)
def test_do_backup(source_directories, mounted_device) -> None:
    empty_config, device = mounted_device
    for source_dir in source_directories:
        config = run_backup_cycle(empty_config, source_dir, device)
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
        config = run_backup_cycle(
            empty_config,
            source_dir,
            device,
            config_extension={"ExcludePatternsFile": EXCLUDE_FILE},
        )
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

    run_backup_cycle(empty_config, first_source, device)
    second_config = run_backup_cycle(
        empty_config,
        second_source,
        device,
        config_extension={"ExcludePatternsFile": EXCLUDE_FILE},
    )
    result_content = get_result_content(second_config, device)
    expected_content = get_expected_content(second_config, exclude_to_ignore_file=True)
    assert result_content == expected_content
