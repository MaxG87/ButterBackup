import datetime as dt
import time
from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest
import shell_interface as sh
import storage_device_managers as sdm

from butter_backup import config_parser as cp
from butter_backup.cli import app


def wait_until_gone(p: Path, timeout: dt.timedelta = dt.timedelta(seconds=3)) -> None:
    """Wait until the given path is gone."""
    start = dt.datetime.now()
    while p.exists():
        if dt.datetime.now() - start > timeout:
            raise TimeoutError(f"Path {p} did not disappear in time.")
        time.sleep(0.01)  # Sleep a bit to avoid busy waiting


@pytest.mark.parametrize(
    "backend", ["BackupBackend", "fvglxvleaeb", "NotYetImplementedBackend"]
)
def test_format_device_refuses_incorrect_backend(runner, backend: str) -> None:
    with NamedTemporaryFile() as tempf:
        result = runner.invoke(app, ["format-device", tempf.name, backend])
        assert result.exit_code != 0


@pytest.mark.parametrize("file_system", ["btrfs", "ext4"])
def test_format_device_accepts_file_system_for_restic(
    runner, file_system: str, big_file: Path
) -> None:
    result = runner.invoke(
        app,
        ["format-device", "restic", str(big_file), "--file-system", file_system],
    )
    assert result.exit_code == 0


@pytest.mark.parametrize("file_system", ["btrfs"])
def test_format_device_accepts_btrfs_system_for_btrfs_rsync(
    runner, file_system: str, big_file: Path
) -> None:
    result = runner.invoke(
        app,
        ["format-device", "restic", str(big_file), "--file-system", file_system],
    )
    assert result.exit_code == 0


@pytest.mark.parametrize("file_system", ["ext4", "xfs", "ntfs", "fat32"])
def test_format_device_refuses_other_fs_for_btrfs_rsync(
    runner, file_system: str, big_file: Path
) -> None:
    result = runner.invoke(
        app,
        ["format-device", "btrfs-rsync", str(big_file), "--file-system", file_system],
    )
    assert result.exit_code != 0


@pytest.mark.parametrize("file_system", ["xfs", "ntfs", "fat32"])
def test_format_device_refuses_invalid_file_system(
    runner, big_file: Path, file_system: str
) -> None:
    result = runner.invoke(
        app,
        ["format-device", "restic", str(big_file), "--file-system", file_system],
    )
    assert result.exit_code != 0


@pytest.mark.parametrize(
    "file_system",
    ["btrfs", "ext4"],
)
def test_format_device_creates_expected_file_system(
    runner, big_file: Path, file_system: str
) -> None:
    format_result = runner.invoke(
        app,
        ["format-device", "restic", str(big_file), "--file-system", file_system],
    )
    assert format_result.exit_code == 0
    serialised_config = format_result.stdout
    parsed = cp.parse_configuration(serialised_config)
    config_lst = parsed.DeviceConfigurations
    assert len(config_lst) == 1
    config = config_lst[0]
    with sdm.decrypted_device(big_file, config.DevicePassCmd) as decrypted:
        result_fs = sdm.get_filesystem(decrypted)
    assert result_fs == file_system


@pytest.mark.parametrize("backend", ["restic", "btrfs-rsync"])
def test_format_device(runner, backend: str, big_file: Path) -> None:
    format_result = runner.invoke(app, ["format-device", backend, str(big_file)])
    serialised_config = format_result.stdout
    parsed = cp.parse_configuration(serialised_config)
    config_lst = parsed.DeviceConfigurations
    assert len(config_lst) == 1
    device_uuid = config_lst[0].UUID
    device_name = config_lst[0].Name
    link_dest = Path(f"/dev/disk/by-uuid/{device_uuid}")
    wait_until_gone(link_dest, dt.timedelta(seconds=3))
    with NamedTemporaryFile("w", suffix=".json") as fh:
        fh.write(serialised_config)
        fh.seek(0)
        with sdm.symbolic_link(big_file, link_dest):
            open_result = runner.invoke(app, ["open", "--config", fh.name])
            close_result = runner.invoke(app, ["close", "--config", fh.name])
    assert format_result.exit_code == 0
    assert open_result.exit_code == 0
    assert close_result.exit_code == 0
    assert str(device_name) in open_result.stdout


@pytest.mark.parametrize("backend", ["restic", "btrfs-rsync"])
def test_format_device_chowns_filesystem_to_user(
    runner, backend: str, big_file: Path
) -> None:
    format_result = runner.invoke(app, ["format-device", backend, str(big_file)])
    serialised_config = format_result.stdout
    parsed = cp.parse_configuration(serialised_config)
    config_lst = parsed.DeviceConfigurations
    assert len(config_lst) == 1
    config = config_lst[0]

    with (
        sdm.decrypted_device(big_file, config.DevicePassCmd) as decrypted,
        sdm.mounted_device(decrypted) as mounted,
    ):
        owner = mounted.owner()
        group = mounted.group()
    expected_user = sh.get_user()
    expected_group = sh.get_group(expected_user)
    assert owner == expected_user
    assert group == expected_group
