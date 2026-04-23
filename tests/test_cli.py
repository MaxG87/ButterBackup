import datetime as dt
import json
import re
import time
import uuid
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory
from unittest import mock

import pytest
import shell_interface as sh
import storage_device_managers as sdm
from loguru import logger
from typer.testing import CliRunner

from butter_backup import cli
from butter_backup import config_parser as cp
from butter_backup.cli import app
from tests import complement_configuration, get_random_filename


def in_docker_container() -> bool:
    return Path("/.dockerenv").exists()


def prepare_tmp_path(
    config: cp.BtrFSRsyncConfig | cp.ResticConfig, parent: Path
) -> None:
    if isinstance(config, cp.BtrFSRsyncConfig):
        prepare_tmp_path_for_btrfs(config, parent)
    elif isinstance(config, cp.ResticConfig):
        prepare_tmp_path_for_restic(config)
    else:
        # TODO: Use t.assert_never when Python 3.11 is the minimum version!
        raise TypeError(
            f"Unsupported configuration type: {type(config).__name__}. "
            "Expected BtrFSRsyncConfig or ResticConfig."
        )


def prepare_tmp_path_for_btrfs(config: cp.BtrFSRsyncConfig, parent: Path) -> None:
    for cur in config.Folders:
        cur.mkdir(exist_ok=True)
    for cur in config.Files:
        cur.parent.mkdir(exist_ok=True, parents=True)
        cur.touch()
    (parent / config.FilesDest).mkdir(exist_ok=True)


def prepare_tmp_path_for_restic(config: cp.ResticConfig) -> None:
    for cur in config.FilesAndFolders:
        # For the purpose of the test that uses this helper function,
        # test_do_backup_refuses_backup_when_device_is_already_open, it does not matter
        # what the content of tmp_path is as long as the configuration can be parsed.
        # Therefore, all items will be folders, since this is much easier to achieve.
        cur.mkdir(parents=True, exist_ok=True)


def wait_until_gone(p: Path, timeout: dt.timedelta = dt.timedelta(seconds=3)) -> None:
    """Wait until the given path is gone."""
    start = dt.datetime.now()
    while p.exists():
        if dt.datetime.now() - start > timeout:
            raise TimeoutError(f"Path {p} did not disappear in time.")
        time.sleep(0.01)  # Sleep a bit to avoid busy waiting


@pytest.fixture
def runner():
    return CliRunner()


def test_get_default_config_path() -> None:
    with TemporaryDirectory() as tempdir:
        xdg_config_dir = Path(tempdir)
    with mock.patch("os.getenv", {"XDG_CONFIG_HOME": xdg_config_dir}.get):
        config_file = cli.get_default_config_path()
    expected_cfg = xdg_config_dir / cli.DEFAULT_CONFIG_NAME
    assert str(expected_cfg) == config_file


@pytest.mark.parametrize(
    "logmsg",
    [
        "Schläft ein Lied in allen Dingen,",
        "Die da träumen fort und fort,",
        "Und die Welt hebt an zu singen,",
        "Triffst du nur das Zauberwort.",
    ],
)
@pytest.mark.parametrize("logfunc", [logger.warning, logger.error])
def test_setup_logging_logs_errors_and_warnings_by_default(
    logmsg: str, logfunc, capsys
) -> None:
    cli.setup_logging(verbosity=0)
    logfunc(logmsg)
    out, err = capsys.readouterr()
    err_without_linebreak = err[:-1]
    assert out == ""
    assert err_without_linebreak.endswith(logmsg)


def test_setup_logging_does_not_log_more_than_warnings_by_default(capsys) -> None:
    cli.setup_logging(verbosity=0)
    logger.success("This line will not appear anywhere.")
    out, err = capsys.readouterr()
    assert out == ""
    assert err == ""


def test_setup_logging_logs_success(capsys) -> None:
    successmsg = "☕️🤎📰📜⚰️🕰🕯🎻🖋"
    infomsg = "🦖🦕🐊"
    cli.setup_logging(verbosity=1)
    logger.success(successmsg)
    logger.info(infomsg)
    out, err = capsys.readouterr()
    err_without_linebreak = err[:-1]
    assert out == ""
    assert infomsg not in err
    assert err_without_linebreak.endswith(successmsg)


def test_setup_logging_clamps_level(capsys) -> None:
    successmsg = "√-1 2³ Σ π and it was delicious"
    tracemsg = "Trace me if you can!"
    cli.setup_logging(verbosity=1337)
    logger.success(successmsg)
    logger.trace(tracemsg)
    out, err = capsys.readouterr()
    assert out == ""
    assert successmsg in err
    assert tracemsg in err


@pytest.mark.parametrize(
    "subprogram",
    ["backup", "close", "open"],
)
def test_subprograms_refuse_missing_config(subprogram, runner) -> None:
    config_file = Path(get_random_filename())
    result = runner.invoke(app, [subprogram, "--config", str(config_file)])
    assert f"{config_file}" in result.stderr
    assert result.exit_code != 0


@pytest.mark.skipif(in_docker_container(), reason="All files are readable for root")
@pytest.mark.parametrize(
    "subprogram",
    ["backup", "close", "open"],
)
def test_subprograms_refuse_unreadable_file(subprogram, runner) -> None:
    with NamedTemporaryFile() as fh:
        config_file = Path(fh.name)
        config_file.chmod(0)
        result = runner.invoke(app, [subprogram, "--config", str(config_file)])
        assert f"{config_file}" in result.stderr
        assert result.exit_code != 0


@pytest.mark.parametrize(
    "subprogram",
    ["backup", "close", "open"],
)
def test_subprograms_refuse_directories(subprogram, runner) -> None:
    with TemporaryDirectory() as tmp_dir:
        result = runner.invoke(app, [subprogram, "--config", tmp_dir])
        assert tmp_dir in result.stderr
        assert result.exit_code != 0


@pytest.mark.skip("Impossible to implement!")
def test_open_refuses_missing_xdg_config(runner) -> None:
    # It seems as if this test cannot be implemented at the moment.
    #
    # This test resets XDG_CONFIG_HOME to provoke that get_default_config_path
    # returns a not existing config file. However, get_default_config_path is
    # executed at import time, rendering resetting XDG_CONFIG_HOME effectless.
    with TemporaryDirectory() as xdg_config_dir:
        pass
    with mock.patch("os.getenv", {"XDG_CONFIG_HOME": xdg_config_dir}.get):
        result = runner.invoke(app, ["open"])
    assert xdg_config_dir in result.stderr
    assert result.exit_code != 0


@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_close_does_not_close_unopened_device(runner, encrypted_btrfs_device) -> None:
    config = encrypted_btrfs_device
    with NamedTemporaryFile() as tempf:
        config_file = Path(tempf.name)
        config_file.write_text(f"[{config.model_dump_json()}]")
        close_result = runner.invoke(app, ["close", "--config", str(config_file)])
        assert close_result.stdout == ""
        assert close_result.exit_code == 0


@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_open_close_roundtrip(runner, encrypted_device) -> None:
    config = encrypted_device
    expected_cryptsetup_map = Path(f"/dev/mapper/{config.UUID}")
    with NamedTemporaryFile() as tempf:
        config_file = Path(tempf.name)
        config_file.write_text(f"[{config.model_dump_json()}]")
        open_result = runner.invoke(app, ["open", "--config", str(config_file)])
        expected_msg = (
            f"Speichermedium {config.Name} wurde in (?P<mount_dest>/[^ ]+) geöffnet."
        )
        match = re.fullmatch(expected_msg, open_result.stdout.strip())
        assert match is not None
        mount_dest = Path(match.group("mount_dest"))
        assert any(
            mount_dest in destinations
            for destinations in sdm.get_mounted_devices().values()
        )
        assert expected_cryptsetup_map.exists()
        runner.invoke(app, ["close", "--config", str(config_file)])
        assert not expected_cryptsetup_map.exists()
        assert not sdm.is_mounted(mount_dest)
        assert not mount_dest.exists()


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
    config_lst = list(cp.parse_configuration(serialised_config))
    assert len(config_lst) == 1
    config = config_lst[0]
    with sdm.decrypted_device(big_file, config.DevicePassCmd) as decrypted:
        blkid_result = sh.run_cmd(
            cmd=["sudo", "blkid", "-o", "value", "-s", "TYPE", str(decrypted)],
            capture_output=True,
        )
    actual_fs = blkid_result.stdout.decode().strip()
    assert actual_fs == file_system


@pytest.mark.parametrize("backend", ["restic", "btrfs-rsync"])
def test_format_device(runner, backend: str, big_file: Path) -> None:
    format_result = runner.invoke(app, ["format-device", backend, str(big_file)])
    serialised_config = format_result.stdout
    config_lst = list(cp.parse_configuration(serialised_config))
    assert len(config_lst) == 1
    device_uuid = config_lst[0].UUID
    device_name = config_lst[0].Name
    link_dest = Path(f"/dev/disk/by-uuid/{device_uuid}")
    wait_until_gone(link_dest, dt.timedelta(seconds=3))
    with NamedTemporaryFile("w") as fh:
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
    config_lst = list(cp.parse_configuration(serialised_config))
    assert len(config_lst) == 1
    config = config_lst[0]

    with sdm.decrypted_device(big_file, config.DevicePassCmd) as decrypted:
        with sdm.mounted_device(decrypted) as mounted:
            owner = mounted.owner()
            group = mounted.group()
    expected_user = sh.get_user()
    expected_group = sh.get_group(expected_user)
    assert owner == expected_user
    assert group == expected_group


def test_version(runner) -> None:
    result = runner.invoke(app, ["version"])
    lines = result.stdout.splitlines()
    assert len(lines) == 1
    parts = lines[0].split(".")
    assert len(parts) == 3  # noqa: PLR2004


@pytest.mark.parametrize("subprogram", ["open", "backup"])
@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_do_backup_refuses_backup_when_device_is_already_open(
    subprogram: str, runner: CliRunner, encrypted_device, tmp_path: Path
) -> None:
    config = complement_configuration(encrypted_device, tmp_path)
    prepare_tmp_path(config, tmp_path)

    config_file = tmp_path / "config.json"

    config_file.write_text(f"[{config.model_dump_json()}]")
    runner.invoke(app, ["open", "--config", str(config_file)])
    result = runner.invoke(app, [subprogram, "--config", str(config_file)])
    expected_msg = (
        f"Speichermedium {config.Name} ist bereits geöffnet. Es wird übersprungen."
    )

    assert result.exit_code == 0
    assert expected_msg in result.stderr


def test_unmount_error_does_not_cause_content_deletion(
    runner: CliRunner, encrypted_device: cp.Configuration, tmp_path: Path, mocker
) -> None:
    # THIS IS A REGRESSION TEST!
    #
    # A previous version of the code had a serious bug, where a failed unmount operation
    # would cause the content of the mount point (i.e. **the backups**!) to be deleted.
    # This test ensures that this bug is fixed by provoking an unmount error and
    # checking that:
    #
    # 1. Unmounting indeed failed (exit_code == 1).
    # 2. The backup repository still exists after the failed unmount operation
    # 3. The device can be closed successfully after the failed unmount operation
    #
    # This test "successfully" provoked the buggy behaviour before the bug was fixed.
    mocker.patch(
        "storage_device_managers.unmount_device",
        side_effect=sdm.UnmountError("Mocked unmount error"),
    )

    config = complement_configuration(encrypted_device, tmp_path)
    prepare_tmp_path(config, tmp_path)
    config_file = tmp_path / "config.json"
    config_file.write_text(f"[{config.model_dump_json()}]")

    result = runner.invoke(app, ["backup", "--config", str(config_file)])
    assert result.exit_code == 1
    # Check that BackupRepositoryFolder still exists after the failed unmount operation.
    # It is assumed that the device is still mounted, since the unmounting is mocked to
    # fail.
    mounts = sdm.get_mounted_devices()
    mount_of_device = next(iter(mounts[str(config.map_name())]))
    expected_backup_repository = mount_of_device / config.BackupRepositoryFolder
    assert expected_backup_repository.exists()
    assert expected_backup_repository.is_dir()
    # Check that the device can be closed successfully after the failed unmount
    # operation.
    mocker.stopall()
    result = runner.invoke(app, ["close", "--config", str(config_file)])
    assert result.exit_code == 0
    assert not mount_of_device.exists()
    assert sdm.is_mounted(mount_of_device) is False


def _make_btrfs_config_json(
    name: str, device_uuid: uuid.UUID | None = None
) -> dict[str, object]:
    """Return a JSON-serialisable dict for a minimal BtrFSRsyncConfig."""
    if device_uuid is None:
        device_uuid = uuid.uuid4()
    return {
        "BackupRepositoryFolder": "repo",
        "DevicePassCmd": "echo pass",
        "Files": [],
        "FilesDest": "files",
        "Folders": {},
        "Name": name,
        "UUID": str(device_uuid),
    }


def test_open_with_dest_dir_skips_non_empty_device_folder(
    runner, mocker, tmp_path
) -> None:
    device_name = "TestDevice"
    dest_dir = tmp_path / "dest"
    dest_dir.mkdir()
    device_folder = dest_dir / device_name
    device_folder.mkdir()
    (device_folder / "existing_file").touch()

    config_file = tmp_path / "config.json"
    config_file.write_text(json.dumps([_make_btrfs_config_json(device_name)]))

    mocker.patch("butter_backup.cli._skip_device", return_value=False)
    mocker.patch("storage_device_managers.open_encrypted_device")
    mocker.patch("storage_device_managers.mount_btrfs_device")

    result = runner.invoke(app, ["open", "--config", str(config_file), str(dest_dir)])

    assert result.exit_code == 0
    # Device was skipped: no mount was attempted
    sdm.open_encrypted_device.assert_not_called()  # type: ignore[attr-defined]
    assert device_name in result.stderr


def test_open_with_dest_dir_mounts_to_named_subdir(runner, mocker, tmp_path) -> None:
    device_name = "TestDevice"
    dest_dir = tmp_path / "dest"
    dest_dir.mkdir()

    config_file = tmp_path / "config.json"
    config_file.write_text(json.dumps([_make_btrfs_config_json(device_name)]))

    mocker.patch("butter_backup.cli._skip_device", return_value=False)
    mocker.patch(
        "storage_device_managers.open_encrypted_device",
        return_value=Path("/dev/mapper/fake"),
    )
    mocker.patch("storage_device_managers.mount_btrfs_device")

    result = runner.invoke(app, ["open", "--config", str(config_file), str(dest_dir)])

    assert result.exit_code == 0
    assert (dest_dir / device_name).is_dir()
    assert str(dest_dir / device_name) in result.stdout


def test_open_with_dest_dir_skips_duplicate_names(runner, mocker, tmp_path) -> None:
    device_name = "SameDevice"
    dest_dir = tmp_path / "dest"
    dest_dir.mkdir()

    config_file = tmp_path / "config.json"
    config_file.write_text(
        json.dumps(
            [
                _make_btrfs_config_json(device_name),
                _make_btrfs_config_json(device_name),
            ]
        )
    )

    mocker.patch("butter_backup.cli._skip_device", return_value=False)
    mocker.patch(
        "storage_device_managers.open_encrypted_device",
        return_value=Path("/dev/mapper/fake"),
    )
    mocker.patch("storage_device_managers.mount_btrfs_device")

    result = runner.invoke(app, ["open", "--config", str(config_file), str(dest_dir)])

    assert result.exit_code == 0
    # First device opened, second skipped: only one named subdir exists
    assert list(dest_dir.iterdir()) == [dest_dir / device_name]
    # ERROR message must appear in stderr
    assert device_name in result.stderr
