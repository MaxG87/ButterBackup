from pathlib import Path

import pytest
import shell_interface as sh

from butter_backup import config_parser as cp

from . import run_backup_cycle

TEST_RESOURCES = Path(__file__).parent.parent / "resources"
FIRST_BACKUP = TEST_RESOURCES / "first-backup"
SECOND_BACKUP = TEST_RESOURCES / "second-backup"


@pytest.mark.parametrize(
    "source_directories",
    [[FIRST_BACKUP], [SECOND_BACKUP], [FIRST_BACKUP, SECOND_BACKUP]],
)
def test_do_backup_for_restic_adapts_ownership(
    source_directories, mounted_device
) -> None:
    # restic needs to be run as root to be able to backup e.g. /home. However, then the
    # entire backup repository is owned by root, preventing the user from working with
    # it. This test ensures that the ownership of the backup repository is handed back
    # to the user running the backup after the backup is finished.
    empty_config, device = mounted_device
    if not isinstance(empty_config, cp.ResticConfig):
        # This test works for ResticConfig only. However, encrypted_device on
        # which mounted_device depends on, is parameterised over all backends.
        # Since this simplifies many other tests it seemed to be an acceptable
        # tradeoff to short-circuit the test here.
        return
    for source_dir in source_directories:
        config = run_backup_cycle(empty_config, source_dir, device)

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
