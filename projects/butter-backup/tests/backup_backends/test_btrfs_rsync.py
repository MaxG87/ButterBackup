import datetime as dt
import itertools
from pathlib import Path
from uuid import uuid4

import pytest
import shell_interface as sh

from butter_backup import backup_backends as bb
from butter_backup import config_parser as cp

from . import get_expected_content, get_result_content, run_backup_cycle

TEST_RESOURCES = Path(__file__).parent / "resources"
FIRST_BACKUP = TEST_RESOURCES / "first-backup"
SECOND_BACKUP = TEST_RESOURCES / "second-backup"


@pytest.mark.parametrize(
    "first_source, second_source",
    [(FIRST_BACKUP, SECOND_BACKUP)],
)
def test_btrfs_backend_gracefully_handles_existing_snapshots_owned_by_root(
    first_source, second_source, mounted_device
) -> None:
    # THIS IS A REGRESSION TEST!
    #
    # After the improvements on the handling of single files backups a weird error
    # ocurred. Sometimes backups would fail due to "PermissionError"s. This happened
    # when the existing snapshot was owned by root and was missing the single files
    # target folder. This test reproduces this scenario and ensures that the backup
    # works correctly, even in this case.
    empty_config, device = mounted_device
    if not isinstance(empty_config, cp.BtrFSRsyncConfig):
        # This test works for BtrfsConfig only. However, encrypted_device on
        # which mounted_device depends on, is parameterised over all backends.
        # Since this simplifies many other tests it seemed to be an acceptable
        # tradeoff to short-circuit the test here.
        return

    first_config = run_backup_cycle(empty_config, first_source, device)
    assert isinstance(first_config, cp.BtrFSRsyncConfig)  # for mypy

    snapshot_root = device / first_config.BackupRepositoryFolder
    latest_snapshot = sorted(snapshot_root.iterdir())[-1]
    for cur in itertools.chain(snapshot_root.glob("*"), snapshot_root.glob("*/*")):
        print(f"Changing ownership of {cur} to root:root")
        sh.run_cmd(cmd=["sudo", "chown", "root:root", cur])
    sh.run_cmd(cmd=["sudo", "rm", "-rf", latest_snapshot / first_config.FilesDest])

    second_config = run_backup_cycle(empty_config, second_source, device)
    result_content = get_result_content(second_config, device)
    expected_content = get_expected_content(second_config, exclude_to_ignore_file=False)
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
    config = run_backup_cycle(empty_config, FIRST_BACKUP, device)
    backup_repository = device / config.BackupRepositoryFolder
    latest_folder = sorted(backup_repository.iterdir())[-1]
    expected_date = dt.date.today().isoformat()
    assert expected_date in str(latest_folder)


def test_btrfs_backend_refreshes_sudo_session_in_do_backup(
    mocker, tmp_path: Path
) -> None:
    sudo_pass_cmd = "echo test_password"
    config = cp.BtrFSRsyncConfig(
        BackupRepositoryFolder="repo",
        DevicePassCmd="echo pass",
        Files=set(),
        FilesDest="files",
        Folders={},
        Name="test-device",
        UUID=uuid4(),
    )
    backend = bb.BtrFSRsyncBackend(config=config)
    mock_refresh = mocker.patch("butter_backup.backup_backends._refresh_sudo")
    mocker.patch.object(
        bb.BtrFSRsyncBackend, "get_source_snapshot", return_value=tmp_path
    )
    mocker.patch.object(bb.BtrFSRsyncBackend, "snapshot", return_value=tmp_path)
    mocker.patch.object(bb.BtrFSRsyncBackend, "adapt_ownership")

    backend.do_backup(tmp_path, sudo_pass_cmd)

    # Only one call before adapting ownership. The other calls are not covered by this
    # test to keep the setup simple.
    mock_refresh.assert_called_once_with(sudo_pass_cmd)
