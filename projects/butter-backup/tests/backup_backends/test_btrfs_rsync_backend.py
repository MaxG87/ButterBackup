import datetime as dt
import itertools
import shutil
from collections import defaultdict
from pathlib import Path
from tempfile import NamedTemporaryFile
from uuid import uuid4

import pytest
import shell_interface as sh
import storage_device_managers as sdm

from butter_backup import backup_backends as bb
from butter_backup import config_parser as cp

from . import (
    get_expected_content,
    get_result_content,
    run_backup_cycle,
)

TEST_RESOURCES = Path(__file__).parent.parent / "resources"
FIRST_BACKUP = TEST_RESOURCES / "first-backup"
SECOND_BACKUP = TEST_RESOURCES / "second-backup"


def get_uid_gid(path: Path) -> tuple[int, int]:
    stat_info = path.stat()
    return stat_info.st_uid, stat_info.st_gid


def get_current_user_uid_gid() -> tuple[int, int]:
    with NamedTemporaryFile() as tmp_file:
        tmp_path = Path(tmp_file.name)
        uid_gid = get_uid_gid(tmp_path)
    return uid_gid


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
        sdm.chown(cur, "root", "root", recursive=False)
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


@pytest.mark.parametrize(
    "source_directories",
    [[FIRST_BACKUP], [SECOND_BACKUP], [FIRST_BACKUP, SECOND_BACKUP]],
)
def test_do_backup_for_btrfs_rsync_preserves_ownership_of_source_files(
    source_directories, mounted_device, tmp_path: Path
) -> None:
    empty_config, device = mounted_device
    test_owner_uid = 1337
    test_group_gid = 1337
    expected_uid_gid = (test_owner_uid, test_group_gid)
    if not isinstance(empty_config, cp.BtrFSRsyncConfig):
        # This test works for BtrFSRsyncConfig only. However, encrypted_device on which
        # mounted_device depends on, is parameterised over all backends. Since this
        # simplifies many other tests it seemed to be an acceptable tradeoff to
        # short-circuit the test here.
        return
    for cur in source_directories:
        source_dir = tmp_path / cur.name
        shutil.copytree(cur, source_dir)
        rm_cmd: sh.StrPathList = ["sudo", "rm", "-r", source_dir]
        sdm.chown(source_dir, f"{test_owner_uid}", f"{test_group_gid}", recursive=True)
        config = run_backup_cycle(empty_config, source_dir, device)
        # remove source dir to avoid permission issues with pytest and "user" 1337
        sh.run_cmd(cmd=rm_cmd)

    # Using dicts instead of sets allows to understand whose files' or folder's
    # ownership is not preserved.
    expected_permissions = defaultdict(set)
    result_permissions = defaultdict(set)
    for snapshot in (device / config.BackupRepositoryFolder).iterdir():
        for dest_dir in snapshot.iterdir():
            for cur_f in dest_dir.rglob("*"):
                uid_gid = get_uid_gid(cur_f)
                result_permissions[uid_gid].add(cur_f)
                expected_permissions[expected_uid_gid].add(cur_f)
    assert result_permissions == expected_permissions
