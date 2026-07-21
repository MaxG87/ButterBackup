from pathlib import Path
from uuid import uuid4

import pytest
import shell_interface as sh

from butter_backup import backup_backends as bb
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


def test_restic_backend_refreshes_sudo_session_in_do_backup(
    mocker, tmp_path: Path
) -> None:
    sudo_pass_cmd = "echo test_password"
    config = cp.ResticConfig(
        BackupRepositoryFolder="repo",
        DevicePassCmd="echo pass",
        FilesAndFolders=set(),
        Name="test-device",
        RepositoryPassCmd="echo repo_pass",
        UUID=uuid4(),
    )
    backend = bb.ResticBackend(config=config)
    mock_refresh = mocker.patch("butter_backup.backup_backends.sh.refresh_sudo")
    mocker.patch.object(bb.ResticBackend, "copy_files")
    mocker.patch.object(bb.ResticBackend, "adapt_ownership")

    backend.do_backup(tmp_path, sudo_pass_cmd)

    expected_nof_calls = 2
    result_calls = mock_refresh.call_args_list
    assert len(result_calls) == expected_nof_calls
